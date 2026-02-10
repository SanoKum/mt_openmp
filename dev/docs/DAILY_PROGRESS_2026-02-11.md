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
