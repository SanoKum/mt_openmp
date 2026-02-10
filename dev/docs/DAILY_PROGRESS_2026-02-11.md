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
