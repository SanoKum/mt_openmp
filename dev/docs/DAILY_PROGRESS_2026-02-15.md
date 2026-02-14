# Daily Progress — 2026-02-15

## DO 1100 (STEPUP) の SIMD ベクトル化

### 対象
- サブルーチン: STEPUP 内 DO 1100（温度→速度計算ループ）
- タイマー: `5STP: STEPUP` (ID=71)
- 問題: ベクトル化率が低い（packed 302 vs scalar 352 = 46%）

### 変更内容

#### 1. ブランチホイスティング（IFGAS / ITIMST 分岐の外出し）
DO 1100 の最内 I ループ内にあった 3 つの分岐をすべて J ループレベルに引き上げ、4 つの特化 I ループに分割:
- ITIMST ≥ 5: TSTAT 計算あり
- IFGAS = 0: 理想気体
- IFGAS = 1: TFROMH 関数呼び出し（→ インライン化）
- IFGAS = 3: テーブル補間

#### 2. TFROMH 関数のインライン化（IFGAS=1 パス）
TFROMH は 5 次多項式: `TREF + HT1*DH + HT2*DH2 + HT3*DH*DH2 + HT4*DH2*DH2`
外部関数呼び出しがベクトル化を阻んでいたため、直接展開。DH, DH2 を PRIVATE 変数に追加。

#### 3. MIN/MAX 変換
`IF(WSQ.LT.VLIM) WSQ = VLIM` → `WSQ = MAX(WSQ, VLIM)` に置換。条件分岐を排除。

#### 4. C$OMP SIMD 付加
4 つの特化 I ループすべてに `C$OMP SIMD` ディレクティブを追加。

### アセンブリ検証結果
| | packed | scalar | SIMD 率 |
|---|---|---|---|
| 変更前 | 302 | 352 | 46% |
| 変更後 | 282 | **164** | **63%** |

scalar 命令が 352 → 164（**53% 削減**）。

### 数値検証
- OMP=1: EMAX=2.7796, EAVG=0.13162, ECONT=0.70818, FLOW=194.06 — 変更前と完全一致

---

## AWS ベンチマーク（02-14 vs 02-11 比較）

### 全体結果 (LOOP: TOTAL)

| | original | OMP=1 | OMP=2 | OMP=4 | OMP=8 |
|---|---:|---:|---:|---:|---:|
| 02-11 | 77.66s | 74.85s | 43.73s | 28.04s | 20.27s |
| 02-14 | 83.93s | 79.35s | 43.34s | **24.16s** | **15.52s** |
| OMP=1→8 倍率 (02-11) | — | — | 1.71x | 2.67x | 3.69x |
| OMP=1→8 倍率 (02-14) | — | — | 1.83x | 3.28x | **5.11x** |
| Orig→8 倍率 | 3.83x | — | — | — | **5.40x** |

※ OMP=1 / Original の絶対値差はインスタンス配置差（NUMA 等）によるもの。スケーリング比率で評価。

### 数値検証
- OMP=1 vs Original: EMAX の last-digit 差のみ（0.27796 vs 0.27797）。他項目一致。

### 項目別スケーリング比較（OMP=1 → OMP=8 倍率）

| ID | Timer | 02-11 倍率 | 02-14 倍率 | 改善 |
|---:|-------|---:|---:|---:|
| **28** | **CELL->NODE** | **2.02x** | **7.38x** | **+265%** |
| 16 | ENERGY | 4.50x | 6.16x | +37% |
| 18 | MOMENTUM-T | 4.75x | 6.39x | +35% |
| 14 | DENSITY | 4.59x | 6.09x | +33% |
| 19 | MOMENTUM-R | 4.58x | 6.02x | +31% |
| 17 | MOMENTUM-X | 4.55x | 5.90x | +30% |
| 26 | STEP/DAMP | 5.72x | 6.87x | +20% |
| 29 | SMOOTH_VAR | 6.18x | 6.74x | +9% |
| 9 | PRESSURE/TEMP | 7.07x | 7.02x | -1% |
| 10 | VISCOUS/TURB | 6.94x | 6.76x | -3% |
| 25 | MG AGG | 6.30x | 5.17x | -18% |

### 最大の改善: CELL->NODE (Timer 28)
- OMP=8: 2.17s → 0.63s（**70% 削減**、全改善 4.75s 中の 1.54s = **32%**）
- Phase1/Phase2 分割 + 並列化により 2.02x → 7.38x にスケーリング改善

### TSTEP スケーリング改善の原因分析
TSTEP（DENSITY/ENERGY/MOMENTUM 等）のコードは 02-11 → 02-14 で**一切変更なし**。
スケーリングが ~4.5x → ~6.1x に改善した原因:

**上流セクションの並列化によるキャッシュウォーミング効果**

- 02-11: 上流フラックス計算（MASS FLUX, MOM FLUX, ENERGY FLUX 等）がシリアル → データはコア 0 の L1/L2 キャッシュに集中 → TSTEP の PARALLEL 開始時にコア 1〜7 がキャッシュコールドスタート
- 02-14: 上流セクションが並列化済み → 各コアの L1/L2 に担当データが載った状態 → SCHEDULE(STATIC) により同じスレッドが同じ範囲を処理 → キャッシュウォーム

裏付け: 改善したのはすべて**細格子 K-J-I ループ**（上流と同データ領域）。粗格子データの MG AGG は逆に悪化 (-18%)。

### OMP=8 時点の残存ボトルネック (02-14)

| Timer | OMP=8 (s) | LOOP占有率 | スケーリング |
|-------|---:|---:|---:|
| EVERY 5 STEPS | 2.39 | 15.4% | 1.44x (I/O律速) |
| 　└ OUTPUT/LOSS | 2.04 | 13.1% | 1.10x |
| MOM FLUX BUILD | 2.32 | 14.9% | 5.52x |
| SMOOTH_VAR | 2.20 | 14.2% | 6.74x |
| MOMENTUM-X | 1.29 | 8.3% | 5.90x |
| DENSITY | 1.26 | 8.1% | 6.09x |
| MOMENTUM-R | 1.26 | 8.1% | 6.02x |
| ENERGY | 1.24 | 8.0% | 6.16x |
| MOMENTUM-T | 1.20 | 7.7% | 6.39x |
| DELTA/STORE | 1.14 | 7.3% | 6.08x |

### 次のアクション
- EVERY 5 STEPS の OUTPUT/LOSS (2.04s) は I/O 律速 → 並列化不可、削減困難
- MOM FLUX BUILD の VX SINGLE (0.20s) は未並列化部分 → 効果限定的
- MG AGG のスケーリング悪化 (-18%) の原因調査
- 5000 ステップベンチマークでの長時間安定性確認

---

## AWS 100ステップ再現性確認

2回の AWS ベンチマーク（同一コード、同一インスタンス c7i）で再現性を確認。

| LOOP: TOTAL | 1回目 (1536) | 2回目 (1618) | 差 |
|---|---:|---:|---:|
| Original | 83.93s | 84.48s | +0.7% |
| OMP=1 | 79.35s | 83.04s | +4.7% |
| OMP=2 | 43.34s | 44.40s | +2.4% |
| OMP=4 | 24.16s | 24.88s | +3.0% |
| OMP=8 | 15.52s | 15.89s | +2.4% |

- 2回目が全体的に 2-5% 遅い → AWS マルチテナント環境のノイズ範囲内
- OMP=8 スケーリング: 1回目 5.40x / 2回目 5.32x → **安定して再現**
- OMP=1 の変動が最大（4.7%）→ 単一コアの周波数スケジューリング変動の影響

### AWS インスタンスのクロック状況
- Xeon Platinum 8488C: ベース 2.4 GHz、最大ターボ 3.8 GHz
- 実測: ほとんどのコアが 2.4 GHz（ベース）、1コアのみ 3.4 GHz
- AWS VM ではターボブーストが限定的（マルチテナント制約）

---

## 1000ステップベンチマーク（実行待ち）

100ステップでのセットアップ時間の比率を下げ、計算ループの性能をより正確に評価するため、1000ステップベンチマークを準備。

- 入力ファイル: `two-stg-LP-ST+steam-1000stp.dat`（NSTEPS_MAX=100→1000 に変更）
- スクリプト: `dev/scripts/run_1000stp_benchmark.sh`
- AWS に転送済み、ユーザが手動実行中

---

## タイマー計測版 (timer original) の修正と検証

### 背景
- 旧タイマー版 (`multall-open-21.3_timer.f`, NT=53) が AWS 上で 1000 ステップ実行時にステップ 905 で発散（NaN）
- 純粋オリジナル (`multall-open-21.3.f`) はローカルで 1000 ステップ完走、数値も正常
- 開発版 (dev, OMP=1) も 1000 ステップ正常完走
- 原因: 旧タイマー版は NT=53 で開発版 (NT=75) との ID 不整合あり、タイマーインフラを最新化して再構築

### 変更内容

#### 1. タイマーインフラの更新 (NT=53→75)
- `PARAMETER (NT=53)` → `PARAMETER (NT=75)` （4箇所: TIMER_INIT, START, STOP, REPORT）
- `TSTART(53)/TACCUM(53)` → `TSTART(75)/TACCUM(75)`
- `DATA NAME(54)〜NAME(75)` を追加（開発版と同一の22タイマー名）
- SUBROUTINE LOOP 内に PARAMETER/INTEGER 宣言を追加（ID=64〜75）

#### 2. TIMER_START/STOP 呼び出しの挿入 (ID=64〜75)
以下の12セクションに計26箇所の TIMER_START/STOP を挿入:

| ID | 名称 | 内容 |
|---|---|---|
| 64 | T_LOOP_DNSCHECK | 密度勾配チェックループ |
| 65 | T_LOOP_MACHCHECK | マッハ数チェックループ |
| 66 | T_LOOP_MIXPLAN | 混合面処理 |
| 67 | T_LOOP_TESEP | TE 分離点処理 |
| 68 | T_LOOP_CONV5STP | 5ステップ毎収束チェック全体 (STOP 2箇所) |
| 69 | T_CONV5_BLADEFLOW | ブレード流量計算 |
| 70 | T_CONV5_EMAXEAVG | EMAX/EAVG 計算 |
| 71 | T_CONV5_STEPUP | STEPUP タイムステップ更新 |
| 72 | T_5STP_ECONT | ECONT 計算 |
| 73 | T_5STP_OUTPUT | OUTPUT/LOSS 出力 |
| 74 | T_5STP_EFICOOL | EFICOOL 効率計算 |
| 75 | T_5STP_IO | サマリー I/O |

#### 3. Makefile 更新
- `build_timer` ターゲットを追加（`make build_timer`）
- バイナリ: `../bin/multall-open21.3-timer`

### 検証結果

#### 1000 ステップ実行（ローカル i5-12400F）
- **全 1000 ステップ完走、発散なし**

#### 数値検証 — 収束指標 (stage.log)
純粋オリジナルとの stage.log diff: **差異なし（bit-for-bit 一致）**

| 指標 (STEP 1000) | 純粋オリジナル | タイマー版 |
|---|---|---|
| EMAX | 0.39932 | 0.39932 |
| EAVG | 0.01830 | 0.01830 |
| ECONT | 0.21457 | 0.21457 |
| FLOW | 135.30 | 135.30 |

#### 数値検証 — 全体性能 (compare_performance.sh)

| 指標 | 純粋オリジナル | タイマー版 | Rel. Diff |
|---|---|---|---|
| PR | 9.86742973 | 9.86742973 | 0.000000% |
| eta_TT | 0.816291749 | 0.816291749 | 0.000000% |
| eta_TS | 0.774164438 | 0.774164438 | 0.000000% |
| Power (kW) | 40372.1641 | 40372.1641 | 0.000000% |
| Flow In | 135.163040 | 135.163040 | 0.000000% |
| Flow Out | 110.514687 | 110.514687 | 0.000000% |

**全項目 0.000000% — 完全一致**

#### タイマーレポート (1000 ステップ, ローカル)

| ID | セクション | 時間 (s) | LOOP 占有率 |
|---|---|---:|---:|
| 4 | LOOP: TOTAL | 613.38 | 100% |
| 29 | TSTEP: SMOOTH_VAR | 116.69 | 19.0% |
| 40 | LOOP: MOM FLUX BUILD | 86.46 | 14.1% |
| 9 | LOOP: PRESSURE/TEMP | 79.86 | 13.0% |
| 14 | TSTEP: DENSITY | 58.25 | 9.5% |
| 16 | TSTEP: ENERGY | 56.70 | 9.2% |
| 17 | TSTEP: MOMENTUM-X | 56.55 | 9.2% |
| 18 | TSTEP: MOMENTUM-T | 56.35 | 9.2% |
| 19 | TSTEP: MOMENTUM-R | 56.30 | 9.2% |
| 26 | TSTEP: STEP/DAMP | 53.35 | 8.7% |
| 23 | TSTEP: DELTA/STORE | 50.42 | 8.2% |
| 28 | TSTEP: CELL->NODE | 42.63 | 6.9% |
| 10 | LOOP: VISCOUS/TURB | 42.89 | 7.0% |
| 11 | LOOP: MASS FLUX | 31.13 | 5.1% |
| 68 | LOOP: EVERY 5 STEPS | 28.04 | 4.6% |
| 71 | 5STP: STEPUP | 23.63 | 3.9% |
| 25 | TSTEP: MG AGG | 18.20 | 3.0% |
| 65 | LOOP: MACH CHECK | 10.91 | 1.8% |

### 変更ファイル
- `dev/src/multall-open-21.3_timer.f` — NT=75 化、ID=64〜75 のタイマー計装追加（19,657 行）
- `dev/src/Makefile` — `build_timer` ターゲット追加

### 次のアクション
- AWS でタイマー版 1000 ステップテスト（旧版は step 905 で発散 → 新版で再現しないか確認）
- タイマー版の AWS 結果をベースラインとして、開発版との区間別比較に使用

---

### 現在の速度向上状況

#### ローカル (i5-12400F 6C/12T, 100ステップ)
| | original | OMP=1 | OMP=2 | OMP=4 |
|---|---|---|---|---|
| LOOP: TOTAL | 54.42s | 52.94s | 37.33s | 31.32s |
| 対 original | — | 1.03x | 1.46x | 1.74x |

#### AWS (c7i 8C/16T, 100ステップ) — 02-14 計測（2回平均）
| | original | OMP=1 | OMP=2 | OMP=4 | OMP=8 |
|---|---|---|---|---|---|
| LOOP: TOTAL | 84.2s | 81.2s | 43.9s | 24.5s | 15.7s |
| 対 original | — | 1.04x | 1.92x | 3.44x | 5.36x |
