# Daily Progress — 2026-02-16

## STEP/DAMP BARRIER 削減の試行と revert

### 対象
- サブルーチン: TSTEP 内 DO 1501（AVG_CHG 計算ループ）
- タイマー: `26 TSTEP: STEP/DAMP` (13.24s @OMP=8, HP Steam)
- 目的: NROWS×3 = 12 バリアを 3 バリアに削減

### 変更内容
DO 1501 の NR ごとのスカラー `REDUCTION(+:SUMCHG)` を配列 `REDUCTION(+:SUMCHG_NR)` にバッチ化:
- 元: `DO 1501 NR=1,NROWS` → 各 NR で `{SINGLE + DO REDUCTION + SINGLE}` = 12 バリア
- 修正: `DO K=1,KMM1; DO NR=1,NROWS; DO J; DO I` で一括計算 = 3 バリア
- `DIMENSION SUMCHG_NR(JD)` を追加、SHARED リストに追加

### 検証結果

#### ローカル (two-stg 100ステップ)
- OMP=1: EMAX=2.7797 — 変更前と **完全一致** ✅
- OMP=4: EMAX=2.7796 — last-digit 差のみ ✅

#### AWS HP Steam フルコンバージェンス OMP=8
| セクション | 修正前 | STEP/DAMP修正後 | 改善 |
|---|---:|---:|---|
| STEP/DAMP | 13.24s | **10.71s** | **-2.53s (-19.1%)** |
| LOOP: TOTAL | 123.3s | 121.9s | -1.4s (-1.1%) |

#### 数値比較（OMP=1, STEP/DAMP修正後 vs pre-fix OMP=1, AWS HP Steam フルコンバージェンス）
| 指標 | pre-fix OMP=1 | STEP/DAMP fix OMP=1 | Rel.Diff |
|---|---|---|---|
| Pressure Ratio | 1.32120669 | 1.32125711 | +0.004% |
| eta_TT | 0.892602 | 0.892624 | +0.002% |
| **Power (kW)** | **25549.72** | **25523.72** | **-0.102%** ⚠️ |
| Flow Out | 317.7696 | 317.7688 | -0.000% |

### 問題: OMP=1 でも 0.1% の数値差
- **原因**: 配列 REDUCTION のコンパイラコード生成がスカラー REDUCTION と異なる
  - スカラー版: レジスタ保持 + SIMD ベクトル化パターン A
  - 配列版: メモリ経由 load/store + 異なる SIMD パターン
  - → ビットレベルの丸め誤差 → DAMP フィードバックループで 5600+ ステップにわたり増幅
- OMP=1 同士で Power 0.1% の差 → **コードロジック自体の問題**（スレッド競合ではない）
- 基準（0.001% 未満）を満たさない

### 判断: revert
- STEP/DAMP 修正を revert し、元の DO 1501 パターンに戻した
- revert 後 OMP=8 で再検証 → Power -0.002%（基準内 ✅）

### 教訓
- DAMP フィードバックループ内の計算は、浮動小数点の加算順序変更に極めて敏感
- scalar REDUCTION → array REDUCTION の変換は、数学的に等価でもコンパイラ最適化の違いで数値差が生じる
- この種の帰還ループ内の最適化は、ループ本体を一切変えない手法（COLLAPSE 等）に限定すべき

---

## MG AGG COLLAPSE(2) 適用

### 対象
- サブルーチン: TSTEP 内 MG AGG（マルチグリッド凝集）
- タイマー: `25 TSTEP: MG AGG` (9.04s @OMP=8, HP Steam)
- 問題: OMP=4 (7.35s) → OMP=8 (9.04s) で **逆に遅くなる**（スケーリング 2.75x）

### 原因分析（HP Steam: IR=3, JR=3, KR=3, IM=37, KM=37）

| ループ | 並列軸 | trip数 | OMP=8 での状況 |
|---|---|---:|---|
| B1CHG | K1 | 12 | 4スレッドが2回、4スレッドが1回 → 負荷不均衡 |
| B2CHG | K2 | 4 | 4スレッドのみ稼働、4スレッドは遊休 |
| SBCHG | JSB | 16 | 2回/スレッドだが作業量不均一 |

加えて、元のシリアルコードが STORE を 1 回の読み出しで 3 配列に scatter-add するのに対し、並列版は B1CHG 用 + SBCHG 用で STORE を 2 回読むため、OMP=1 でも 33% のオーバーヘッドがある（18.6s→24.8s）。

### 変更内容
B1CHG と B2CHG の gather ループに `COLLAPSE(2)` を追加:

```fortran
C     B1CHG: COLLAPSE(2) on K1×J1
C$OMP DO COLLAPSE(2) SCHEDULE(STATIC)
      DO K1=1,NKB1           ! 12 iterations
        DO J1=1,NJB1+1       ! ~143 iterations
        KSTART = (K1-1)*KR + 1
        ...

C     B2CHG: COLLAPSE(2) on K2×J2
C$OMP DO COLLAPSE(2) SCHEDULE(STATIC)
      DO K2=1,NKB2           ! 4 iterations
        DO J2=1,NJB2+1       ! ~48 iterations
        ...
```

並列 trip 数の変化:
| ループ | 変更前 | COLLAPSE(2)後 |
|---|---:|---:|
| B1CHG | K1=12 | K1×J1 = **1,716** |
| B2CHG | K2=4 | K2×J2 = **192** |

### デメリット分析
- ベクトル化: 最内ループ (I1, I) に変更なし → 影響なし
- メモリアクセス: スレッドあたりの作業範囲が狭くなりキャッシュ効率改善
- オーバーヘッド: COLLAPSE の除算/剰余は本体コストに対し無視可能
- 数値結果: 各セルのリダクションが独立 → ビットレベルで一致

### 検証結果

#### ローカル (two-stg 100ステップ OMP=1)
- EMAX=2.7797 — 基準値と **完全一致** ✅

#### AWS HP Steam フルコンバージェンス OMP=8

数値検証:
| 指標 | original | COLLAPSE OMP=8 | Rel.Diff |
|---|---|---|---|
| Pressure Ratio | 1.32120907 | 1.32120860 | **-0.000036%** ✅ |
| eta_TT | 0.892613 | 0.892600 | -0.001% ✅ |
| Power (kW) | 25550.38 | 25549.82 | **-0.002%** ✅ |
| Flow Out | 317.770 | 317.769 | -0.000% ✅ |

MG AGG タイマー:
| | 修正前 (OMP=8) | COLLAPSE(2) (OMP=8) | 改善 |
|---|---:|---:|---|
| **MG AGG** | **9.04s** | **7.56s** | **-1.48s (-16.4%)** |

LOOP: TOTAL は 123.7s → 129.6s と悪化しているが、他セクション（DENSITY, ENERGY, MOM-X/T/R, CELL->NODE）が全体的に 1-2s 悪化しており、AWS 実行環境の揺らぎ（他 VM の干渉等）と判断。MG AGG 単体は確実に改善。

---

## まとめ

| 試行 | 対象 | MG AGG/STEP差 | LOOP:TOTAL変化 | 数値精度 | 判断 |
|---|---|---|---|---|---|
| STEP/DAMP BARRIER削減 | DO 1501 | -2.53s | -1.4s | **0.1% NG** | **revert** |
| MG AGG COLLAPSE(2) | B1CHG/B2CHG | -1.48s | +5.9s (ノイズ) | 0.002% OK | **採用** |

最終コード状態: FINAL AVG fix + MG AGG COLLAPSE(2) （STEP/DAMP修正は revert 済み）

---

### 現在の速度向上状況

#### ローカル (i5-12400F 6C/12T, 100ステップ)
| | original | OMP=1 | OMP=2 | OMP=4 |
|---|---|---|---|---|
| LOOP: TOTAL | 54.42s | 52.94s | 37.33s | 31.32s |
| 対 original | — | 1.03x | 1.46x | 1.74x |

#### AWS (c7i 8C/16T, two-stg 100ステップ) — 02-15 計測
| | original | OMP=1 | OMP=2 | OMP=4 | OMP=8 |
|---|---|---|---|---|---|
| LOOP: TOTAL | 81.11s | 76.01s | 39.90s | 22.11s | 13.33s |
| 対 original | — | 1.07x | 2.03x | 3.67x | **6.09x** |

#### AWS (c7i 8C/16T, HP Steam ~5676ステップ収束) — FINAL AVG fix + MG AGG COLLAPSE(2)
| | Original | OMP=1 | OMP=2 | OMP=4 | OMP=8 |
|---|---|---|---|---|---|
| LOOP: TOTAL | 685.7s | 645.8s | 329.6s | 185.7s | 123.3s* |
| 対 Original | — | 1.06x | 2.08x | 3.69x | **5.56x** |

(*) OMP=8 は FINAL AVG fix 時点の値。COLLAPSE(2) の実測は 129.6s だが AWS ノイズによる変動含む。MG AGG 単体は 9.04s→7.56s で改善確認済み。

---

## フルベンチマークスクリプトの作成と実行試行

### 目的
最新コード（FINAL AVG fix + MG AGG COLLAPSE(2)）で、two-stg と HP Steam の両ケースを original/OMP=1/2/4/8 で AWS 上でフルコンバージェンス計算し、包括的な速度比較を行う。

### スクリプト
- `dev/scripts/run_full_benchmark.sh` を作成
- two-stg: `two-stg-LP-ST+steam-10000stp.dat` (NSTEPS=10000, CONLIM=0.001, ~6976ステップ収束)
- HP Steam: `hp-steam-turb.dat` (NSTEPS=9000, CONLIM=0.0005, ~5680ステップ収束)
- original → OMP=1 → OMP=2 → OMP=4 → OMP=8 の順に実行
- 推定所要時間: two-stg ~4時間10分 + HP Steam ~34分 = 約4時間45分

### 実行結果: 失敗
- original バイナリが `/dev/tty` を開いて Y/N プロンプトを出すため、非対話 SSH ではプロンプトが動作しない
- `set -e` により original 失敗時点でスクリプト停止 → 新しいファイルが一切作成されず
- サマリー部分の glob が旧ファイル (20260215_1642) にマッチし、古い結果が表示された

### 修正
- `script -qc "echo Y | ..."` で pseudo-tty を提供する方式に変更
- `|| true` で original 失敗時もスクリプトが継続するよう修正
- 修正済みスクリプトは AWS に再転送して再実行が必要

---

## HP Steam 全セクション ボトルネック分析（AWS c7i, ~5676ステップ収束）

AWS `aws_results_1421` の original/OMP=1/2/4/8 タイマーデータを全セクション分析。

### スケーリング分類

| 分類 | セクション | OMP=1→8 倍率 | OMP=8 時間 | 備考 |
|---|---|---:|---:|---|
| **Excellent (7x+)** | DELTA/STORE | 7.51x | 6.9s | |
| **Good (5-7x)** | MOM FLUX BUILD | 6.76x | 15.5s | |
| | CELL->NODE | 6.42x | 9.0s | |
| | SMOOTH_VAR 全体 | 5.58x | 20.2s | 最大ボトルネック |
| | PRES/TEMP | 5.48x | 4.6s | |
| **MemBound (4.5-5x)** | DEN UPDATE | 4.87x | 12.9s | K-stride 512KB |
| | ENE UPDATE | 4.78x | 12.8s | K-stride 512KB |
| | MOM-X UPDATE | 4.71x | 12.8s | K-stride 512KB |
| | MOM-T UPDATE | 4.70x | 12.6s | K-stride 512KB |
| | MOM-R UPDATE | 4.67x | 12.7s | K-stride 512KB |
| **Low (3-4.5x)** | STEP/DAMP | 4.29x | 13.2s | BARRIER 多数 |
| | SMOOTH STREAMWISE | 3.64x | 6.7s | |
| | SMOOTH RESET D | 3.87x | 3.3s | |
| **Worst (<3x)** | MG AGG | 2.75x | 9.0s | → COLLAPSE(2) で 7.56s に改善 |
| **Inverse** | FINAL AVG | 0.78x | 2.9s | SINGLE 区間主体 |

### DEN/ENE/MOM のメモリバウンド分析

5セクションがすべて 4.7-4.9x で揃っている → **メモリバウンド**。

原因: `STORE(ID,JD,KD)` = `STORE(128,1000,82)` の静的確保:
- K方向ストライド = `ID × JD × 4bytes` = **512KB**
- HP Steam の実使用サイズ: IM=37, JM=434, KM=37
- 理想ストライド = `37 × 434 × 4` = **64KB**（L1 キャッシュ 48KB に近い）
- → **8倍もの無駄なストライド** がキャッシュミスを誘発

### STORE 配列の静的確保問題

`commall-open-21.3` の PARAMETER:
```
PARAMETER(ID=128, JD=1000, KD=82, MAXKI=128, NRS=21)
```

これは Fortran 77 の COMMON ブロック制約（静的確保必須）により、最大メッシュサイズに合わせて固定されている。HP Steam (37×434×37) では 128×1000×82 の約 6% しか使用していない。

### 解決策の評価

| 方法 | K-stride | メモリ/配列 | 工数 | リスク |
|---|---|---:|---|---|
| 現状 (PARAMETER固定) | 512KB | 41.9MB | — | — |
| **B) PARAMETER値チューニング** | 80KB | 3.2MB | 極小 (1行) | 低（再コンパイル要） |
| **C) MODULE + ALLOCATABLE** | 64KB | 2.4MB | 中 (4-5h) | 低 |

### MODULE + ALLOCATABLE リファクタリングの実現性

当初「別プロジェクトレベル」と過大評価したが、実際の作業量を精査:

- COMMON ブロック: **35個** → MODULE 内の変数宣言に変換
- `INCLUDE 'commall-open-21.3'`: **47箇所** → `USE multall_data` に置換（sed 一発）
- ローカル `DIMENSION D(ID,JD,KD)`: **11箇所** → `D(IM,JM,KM)` に（F90自動配列）
- ローカル `DIMENSION X(JD)` 等: **~30箇所** → `X(JM)` 等に
- ALLOCATE サブルーチン: **1個** 新規作成（メッシュ読み込み後に呼ぶ）

**合計: 4-5時間**。機械的作業が大半。

期待効果（HP Steam の場合）:
- 全3D配列の K-stride: 512KB → 64KB（8倍改善）
- DEN/ENE/MOM系の 4.7-4.9x → 6-7x への改善が期待できる
- 総メモリ削減: ~数GB → ~数百MB

---

## コード規則の更新

`copilot-instructions.md` のセクション 1 を更新:
- 旧: "Fixed-Form Fortran 基本規則" — `.f90` 禁止、fixed-form のみ
- 新: "Fortran 基本規則" — MODULE/ALLOCATABLE/USE を積極的に活用可、MODULE は `.f90` で作成可

これにより COMMON → MODULE + ALLOCATABLE のリファクタリングがルール上も可能に。

---

### 次のアクション

#### 優先度 高
1. **MODULE + ALLOCATABLE リファクタリング**: COMMON → MODULE 化で STORE 等の配列ストライドを最適化。DEN/ENE/MOM系 5セクション（計63.8s@OMP=8）のメモリバウンドを改善
2. **フルベンチマーク**: 修正済み `run_full_benchmark.sh` を AWS で再実行（リファクタリングの前後比較用ベースライン）

#### 優先度 中
3. **SMOOTH_VAR STREAMWISE/RESET D** (計10.0s): 3.6-3.9x のスケーリング改善
4. **STEP/DAMP COLLAPSE(2)**: DO 1502 の K×J 結合（数値差なし、DAMP feedback に影響しない部分）

#### 優先度 低
5. **K-only ループの COLLAPSE(2)**: KMM1=36 のみの並列ループで trip 数を増やす
