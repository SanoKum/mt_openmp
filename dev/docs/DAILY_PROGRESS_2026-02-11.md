# Daily Progress — 2026-02-11

## セッション 1: Copilot ルール整備

### 実施内容

1. **`.github/copilot-instructions.md` を新規作成**
   - セッションをまたいで維持される Copilot 向けプロジェクトルールファイルを整備。

2. **Fixed-Form Fortran 基本規則を記載**
   - `.f` のみ、列 7〜72、継続行は列 6、コメントは列 1 に `C`。

3. **OpenMP ディレクティブルールを記載**
   - `C$OMP` 列 1 統一、`!$OMP` 禁止。
   - 3 パターン（PARALLEL+DO / SIMD / PARALLEL DO SIMD）を使い分ける方針に整理。
   - `DEFAULT(NONE)` 必須、PRIVATE/SHARED 明示ルール。

4. **コンパイル・実行の受け入れ条件を記載**
   - `-fopenmp` フラグ必須、`OMP_DISPLAY_ENV=TRUE` での検証、性能目標 30%。

5. **コード変更後の検証手順を記載**
   - `make build` → `two-stg-LP-ST+steam.dat` で 2 並列実行。
   - `stage.log` を日付付きで保存、`stage.log.original` と計算時間比較。
   - 並列化・ベクトル化の効果検証（1 vs 2 スレッド比較、`vec.log` 確認）。
   - ※数値一致の検証は収束計算を行うようになってから追加予定。

6. **ファイル構成ルールを整理**
   - `dev/scripts/` に検証スクリプトを配置するルールを追加。
   - `diff_stage_logs.sh` を `test_cases/` → `scripts/` に移動。
   - `dev/test_cases/` にはスクリプトを置かないルール。

7. **進捗記録ルールを記載**
   - `dev/docs/DAILY_PROGRESS_YYYY-MM-DD.md` にユーザ指示時のみ記録。

### 次のアクション

- ソースコード内の既存 OpenMP ディレクティブを点検し、`copilot-instructions.md` のルールに沿って整理する（`!$OMP` → `C$OMP` 変換、PRIVATE/SHARED の見直しなど）。

---

## セッション 2: OpenMP ディレクティブ全修正 + PRES/TEMP 高速化

### 実施内容

#### 1. SMOOTH_VAR 全6領域の OpenMP 修正

全 `!$OMP` → `C$OMP`（列1）、`DEFAULT(SHARED)` → `DEFAULT(NONE)` + 明示的 PRIVATE/SHARED。

| 領域 | 修正内容 | 備考 |
|---|---|---|
| STREAMWISE CORE | センチネル変換 + DEFAULT(NONE) | — |
| 境界平滑化 | 構造バグ修正（孤立OMP DOにPARALLEL追加） | `!$OMP`が無視されていたため潜在バグ |
| LEADING EDGE | 2領域修正、PRIVATE(J)→SHARED(J) | J が PARALLEL 前に設定される変数だった |
| RESET D | PARALLEL DO COLLAPSE(2) + DEFAULT(NONE) | ベクトル化確認済 |
| PITCH/SPANWISE | 内側K/I並列→外側J並列化 | 最大の改善: 4thr で 4.07→2.20s |
| EXIT FLOW | 2領域（I方向・K方向）修正 | 微小領域（0.02s） |

#### 2. PRESSURE/TEMPERATURE 領域の OpenMP 修正

- `!$OMP PARALLEL DEFAULT(SHARED)` → `C$OMP PARALLEL DEFAULT(NONE)` + SHARED 37変数明示
- `C$OMP SINGLE` で ITABTIME/TS_CNT 設定と VSOUND 計算を保護
- 4条件分岐（IFGAS=0/1/3, ITIMST=5/6）の全 `!$OMP DO` → `C$OMP DO`

#### 3. PRES/TEMP 高速化（作戦A+C+D）

| 作戦 | 内容 | 効果 |
|---|---|---|
| A: サンプリング除去 | COMMON /TABTIME/ の TS_CNT インクリメントを除去。false sharing（キャッシュラインバウンシング）が4thr並列化を完全に殺していた | 4thr: 7.92→2.72s |
| B: バイナリサーチ | 2段階線形サーチをバイナリサーチに置換 → **逆効果**（分岐予測悪化）| **revert** |
| C: ヒントベース探索 | THREADPRIVATE で前回の IFOUND/JFOUND を記憶。隣接格子点は同じテーブルセルに落ちるため、探索をほぼスキップ | 1thr: 7.96→5.18s |
| D: COLLAPSE除去 | COLLAPSE(3)→K のみ並列化。K方向スライス内でI,J連続→ヒント率向上 | C との相乗効果 |
| E: AoS パッキング | 15配列を TAB_PACK(15,ITB,JTB) に詰めてキャッシュ局所性向上 → **効果なし**（ヒントでキャッシュ済み） | **revert** |
| — | FINDIT5 インライン化確認 | gfortran -O3 で既に自動インライン化済 |

#### 4. ソース全体から `!$OMP` 完全除去

`grep -n '!$OMP' multall-open21.3-s1.0.f` → 0件。全ディレクティブがルール準拠。

### 性能結果

#### SMOOTH_VAR

| スレッド | original | 最終 | 短縮率 |
|---|---|---|---|
| 1thr | 8.51s | 5.94s | -30% |
| 2thr | 8.06s | 5.23s | -35% |
| 4thr | — | 3.10s | — |

#### PRESSURE/TEMP

| スレッド | 修正前 | 作戦A後 | A+C+D後 | 短縮率 |
|---|---|---|---|---|
| 1thr | 8.87s | 7.96s | 5.18s | -42% |
| 4thr | 7.92s | 2.72s | 1.95s | -75% |

#### TOTAL WALL TIME

| スレッド | original | 最終 |
|---|---|---|
| 1thr | 63.45s | 54.20s |
| 4thr | — | 48.05s |

### 学び

- **`!$OMP` は fixed-form Fortran で無視される** — 全ての並列化が実質無効だった
- **false sharing は並列化を完全に殺す** — TS_CNT の1変数除去で 4thr PRES が 3倍速に
- **ヒントベース探索は CFD に非常に有効** — 隣接格子点の物理量連続性を利用
- **バイナリサーチは小配列（150要素）で逆効果** — 分岐予測が効く線形走査の方が速い
- **AoS パッキングはヒント併用時に無効** — 同じセルがキャッシュに残るため

### 次のアクション

- 他のサブルーチンの並列化検討（DENSITY 3.6s, ENERGY 3.5s, MOMENTUM 3.5s×3）
- SETUP 領域（15.8s）の分析と最適化可能性調査

---

## セッション 3: TSTEP 並列化（DENSITY/ENERGY/MOMENTUM）

### 目標

TSTEP 内部のループ並列化。TSTEP は RO/ROE/ROVX/RORVT/ROVR の 5変数更新を行う共通ルーチンで、
合計 14.9s（LOOP全体の 44%）を占める最大のボトルネック群。

### 実施内容

#### 1. 並列化プラン策定

[OPENMP_PLAN_TSTEP.md](OPENMP_PLAN_TSTEP.md) を作成。TSTEP 内部の各区間を分析:

| 区間 | 時間 (s) | 方針 |
|---|---|---|
| DELTA/STORE (DO 1000) | 2.6 | 要素独立 → PARALLEL DO |
| MG AGG (DO 700) | 1.6 | scatter蓄積 → ATOMIC |
| STEP (DO 1500) | (一部) | 要素独立 → PARALLEL DO |
| DAMP (DO 1502+1525) | (一部) | REDUCTION + 要素独立 |
| CELL->NODE (DO 1100) | 3.3 | scatter → ATOMIC |
| SMOOTH_VAR | 4.1 | 既に並列化済 |

#### 2. 実装: 個別 PARALLEL DO アプローチ

各ループに独立した `C$OMP PARALLEL DO` を配置:
- DO 1000: `COLLAPSE(3) PRIVATE(I,J,K,RATPITCH,DELTA)`
- DO 1500: `COLLAPSE(3) PRIVATE(I,J,K,I1,I2,J1,J2,K1,K2,JSB,DELTA)`
- DO 1502: `COLLAPSE(3) REDUCTION(+:SUMCHG)`
- DO 1525: `COLLAPSE(3) PRIVATE(I,J,K,NR,DELTA,ABSCHG,FDAMP)`
- DO 700: ATOMIC 付き PARALLEL DO
- DO 1100: ATOMIC 付き PARALLEL DO（8個の ATOMIC）

#### 3. テスト結果

**ATOMIC アプローチ（DO 700 + DO 1100）は壊滅的:**

| 区間 | 変更前 (OMP=2) | ATOMIC OMP=1 |
|---|---|---|
| CELL->NODE | 3.3s | **75.5s** (+22倍) |
| MG AGG | 1.6s | **30.0s** (+18倍) |
| MAIN: TOTAL | 49.9s | **161.4s** (+3.2倍) |

→ **DO 700, DO 1100 の ATOMIC を即座に revert**

**ATOMIC 除去後（DO 1000/1500/1502/1525 のみ並列化）:**

| 区間 | 変更前 (OMP=2) | OMP=1 | OMP=2 |
|---|---|---|---|
| DELTA/STORE | 2.593 | 4.665 (+80%) | 2.751 (+6%) |
| STEP/DAMP | 3.008 | 8.524 (+183%) | 4.722 (+57%) |
| LOOP: TOTAL | 33.766 | 45.648 (+35%) | 36.984 (+9.5%) |
| MAIN: TOTAL | 49.902 | 61.247 (+23%) | 52.616 (+5.4%) |

**結論: 現状は改悪。** ループ単位の `PARALLEL DO` は fork/join オーバーヘッドが並列化利得を上回る。

#### 4. 原因分析

- TSTEP は 1タイムステップで 5回呼ばれる × 100ステップ = 500回
- 各呼び出しで 4箇所の `PARALLEL DO` → fork/join 合計 2000回
- この問題サイズ（64×351×64）では fork/join コストがループ計算コストに対して大きい
- 特に STEP/DAMP は SMOOTH_RESID 呼び出しの前後で PARALLEL 領域が分断される

### 現在のソースコード状態

以下の OpenMP ディレクティブが **TSTEP 内に残っている**（改悪状態のまま）:
- L6634: `C$OMP PARALLEL DO` — DO 1000 (DELTA/STORE)
- L6738: `C$OMP PARALLEL DO` — DO 1500 (STEP)
- L6785: `C$OMP PARALLEL DO` — DO 1502 (REDUCTION)
- L6819: `C$OMP PARALLEL DO` — DO 1525 (DAMP)

DO 700 (MG AGG) と DO 1100 (CELL->NODE) は ATOMIC が壊滅的だったため**逐次版に戻し済み**。

### 保存済みログファイル

| ファイル | 内容 |
|---|---|
| `stage.log.omp1_tstep_atomic` | ATOMIC 版 OMP=1（161.4s、壊滅） |
| `stage.log.omp1_tstep_noatomic` | ATOMIC 除去後 OMP=1（61.2s） |
| `stage.log.omp2_tstep_noatomic` | ATOMIC 除去後 OMP=2（52.6s） |

### 学び

- **ATOMIC は小配列への高頻度衝突で壊滅的** — DO 1100 の 8個同時 ATOMIC、DO 700 の粗視化配列への集中 ATOMIC はどちらも信じられないほど遅い
- **ループ単位の PARALLEL DO は fork/join が支配的** — 2000回の fork/join はこの問題サイズでは割に合わない
- **gfortran の ATOMIC 実装は特に高コスト** — ハードウェア ATOMIC が使えない場合ロックベースになる

### 次のアクション（次セッションへの引き継ぎ）

#### 最優先: TSTEP 全体を単一 PARALLEL 領域に統合

現在のループ単位 `PARALLEL DO` を全て除去し、TSTEP の冒頭で `C$OMP PARALLEL` を開き、
個別ループを `C$OMP DO` で並列化する。これにより fork/join を 500回（5変数×100ステップ）に削減:

```
C$OMP PARALLEL DEFAULT(NONE) PRIVATE(...) SHARED(...)
C     ... MG INIT loops: C$OMP DO ...
C     ... TIP GAP: C$OMP SINGLE / sequential ...
C     ... FEXTRAP: C$OMP SINGLE / sequential ...
C$OMP DO
      DO 1000 ...   ← DELTA/STORE
C$OMP END DO
C     ... PITCH AVG: C$OMP SINGLE ...
C     ... MG AGG DO 700: C$OMP SINGLE (ATOMIC不可) ...
C$OMP DO
      DO 1500 ...   ← STEP
C$OMP END DO
C     ... SMOOTH_RESID: C$OMP SINGLE or 内部並列化 ...
C$OMP DO REDUCTION(+:SUMCHG)
      DO 1502 ...   ← AVG_CHG
C$OMP END DO
C$OMP DO
      DO 1525 ...   ← DAMP
C$OMP END DO
C     ... PITCH AVG POST-MG: C$OMP SINGLE ...
C     ... CELL->NODE DO 1100: 要検討 ...
C$OMP END PARALLEL
C     ... SMOOTH_VAR(D): 内部に独自PARALLEL ...
```

**注意点:**
- `SMOOTH_RESID` が TSTEP 内 PARALLEL 領域の中で呼ばれる → `C$OMP SINGLE` で囲むか、`SMOOTH_RESID` 自体を並列化するか
- `SMOOTH_VAR` は独自の PARALLEL 領域を持つ → TSTEP の PARALLEL 領域はその前で閉じる
- DO 700 (MG AGG) と DO 1100 (CELL->NODE) は scatter パターンで ATOMIC 不可 →
  `C$OMP SINGLE` で逐次実行するか、gather 変換が必要
- NROWS ループ（DO 1501）内の PARALLEL DO REDUCTION は、外側 PARALLEL 領域内で入れ子の並列領域にはできない → `C$OMP DO` + `C$OMP SINGLE` の組み合わせを検討

#### 代替案: CELL->NODE の gather 変換

scatter（セル→ノード分配）を gather（ノード←周囲セル集約）に変換すれば、
`D(I,J,K)` の書き込みが各ノードで独立し、ATOMIC 不要で完全並列化可能。
ただし境界条件の IF 分岐が多くなるデメリットあり。
