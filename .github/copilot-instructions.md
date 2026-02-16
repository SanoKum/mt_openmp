# MULTALL Project — Copilot Instructions

このリポジトリは Fortran CFD ソルバです。
以下のルールを **すべてのコード生成・編集で厳守** してください。

---

## 1. Fortran 基本規則

- メインソース (`.f`) は **fixed-form** で記述する。
- Fortran 90 以降の機能（`MODULE`, `ALLOCATABLE`, `USE` 等）を **積極的に活用してよい**。
- MODULE ファイルは free-form (`.f90`) で作成してもよい。
- fixed-form コードでは **列 7〜72** に収める。
- 継続行は **列 6 に `&` 等の継続文字**。
- コメントは **列 1 に `C` または `c`**。`!` によるインラインコメントは使わない（fixed-form ファイル内）。

---

## 2. OpenMP ディレクティブ — 絶対ルール

### 2.1 センチネル
- OpenMP ディレクティブは **`C$OMP`（列 1 始まり）のみ使用**する。
- **`!$OMP` は禁止**。fixed-form では `!` がコメント扱いになり、ディレクティブが無視される事故が起きるため。
- ディレクティブにインデントを付けてはならない（必ず列 1 から開始）。

### 2.2 ディレクティブの継続行
- 継続行は **列 1〜3 が `C$O`、列 4〜5 が `MP`、列 6 が継続文字（`&` 等）** とする。
  ```
  C$OMP PARALLEL DEFAULT(NONE)
  C$OMP&PRIVATE(I,J,K) SHARED(A,B,C)
  ```

### 2.3 並列ループのパターン

状況に応じて以下のパターンを使い分ける。

#### パターン A — PARALLEL + DO（基本）
```fortran
C$OMP PARALLEL DEFAULT(NONE)
C$OMP&PRIVATE(...) SHARED(...)
C$OMP DO SCHEDULE(STATIC)
      DO I = 1, N
        ... loop body ...
      ENDDO
C$OMP END DO
C$OMP END PARALLEL
```

#### パターン B — SIMD（ベクトル化のみ）
```fortran
C$OMP SIMD
      DO I = 1, N
        ... loop body ...
      ENDDO
```

#### パターン C — PARALLEL DO SIMD（スレッド並列＋ベクトル化）
```fortran
C$OMP PARALLEL DO SIMD DEFAULT(NONE)
C$OMP&PRIVATE(...) SHARED(...)
      DO I = 1, N
        ... loop body ...
      ENDDO
C$OMP END PARALLEL DO SIMD
```

#### 共通ルール
- `PARALLEL` を含む場合は `DEFAULT(NONE)` を必ず付け、すべての変数を `PRIVATE` / `SHARED` で明示する。
- ループ内のローカル変数（ループカウンタ含む）は `PRIVATE`。
- 配列・定数・共有データは `SHARED`。
- `REDUCTION` が必要な場合は `REDUCTION(+:SUM)` 等を追加。
- 軽量なループには SIMD 単体（パターン B）を優先し、不要なスレッド生成オーバーヘッドを避ける。

### 2.4 SIMD ディレクティブ

- `C$OMP SIMD` は対象 `DO` 文の **直前の行** に置く（間に空行やコメントを挟まない）。
  ```fortran
  C$OMP SIMD
        DO I = 1, N
  ```

### 2.5 COMMON ブロック / EQUIVALENCE への対応

- `COMMON` ブロックの変数は通常 `SHARED` にする。
- `COMMON` 内にスレッドローカルな作業変数がある場合は `THREADPRIVATE` を検討し、必ずコメントで理由を記載する。

---

## 3. コンパイル・実行の基本設定

### 3.1 コンパイル
- `gfortran` 使用時: **`-fopenmp`** フラグ必須。
- `ifort` 使用時: **`-qopenmp`** フラグ必須。
- Makefile の `FFLAGS` に OpenMP フラグが含まれていることを確認する。

### 3.2 実行時設定
- `OMP_NUM_THREADS` で明示的にスレッド数を制御。
- `OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE` を設定する。
- 検証時は `OMP_DISPLAY_ENV=TRUE` でスレッド数・スケジュール設定が正しく適用されていることを確認する。

### 3.3 性能目標
- 1 スレッド → N スレッドで **最低 30% の実行時間改善** を目標とする。

---

## 4. 並列化ワークフロー

並列化は以下のステップで進める。各ステップの実施判断はユーザが行い、AI はユーザの指示に従う。

### ステップ 1 — 計算時間の細分化計測

対象コードの計算時間を区間ごとに細分化し、ボトルネックを特定する。

- 計測区間は **可能な限り細かく分割** する。
- 区間名は以下の命名規則に従う：
  ```
  LOOP: (処理名)           ← メインループ内の区間
  MAIN: (処理名)           ← メインループ外（セットアップ・後処理等）
  (サブルーチン名): (処理名) ← サブルーチン内の区間
  ```
- タイマーは `CALL TIMER_START(T_XXX)` 〜 `CALL TIMER_STOP(T_XXX)` で挟む。
- **PARALLEL 領域内部で計測する場合** は、MASTER + BARRIER で全スレッドの同期を取ってから切り替える：
  ```fortran
  C     BARRIER: 全スレッドの同期後に MASTER が計測を切り替える
  C     BARRIER なしだと他スレッドが前区間を実行中に計測が切り替わる
  C$OMP BARRIER
  C$OMP MASTER
        CALL TIMER_STOP(T_SECTION_A)
        CALL TIMER_START(T_SECTION_B)
  C$OMP END MASTER
  C$OMP BARRIER
  ```

### ステップ 2 — 並列化方針の提案

ユーザが並列化対象として指定したセクションについて、AI が並列化方針を提案する。

- 提案内容は `dev/docs/OPENMP_PLAN_*.md` に記録する。
- ファイル名は `OPENMP_PLAN_(セクション名).md` とする。
- 以下のセクション構成を標準とする：
  1. **対象コード** — ファイル名・行番号・タイマーID・OMP=1 実測時間
  2. **現状コード** — 対象箇所の抜粋
  3. **データ依存性分析** — 読み込み変数・書き込み変数・依存性・安全な並列化軸
  4. **並列化方針** — 具体的なコード案 + 変数分類表（PRIVATE/SHARED/REDUCTION）
  5. **リスク評価** — データ競合リスク・並列粒度の評価
  6. **期待効果** — 予想される速度向上

### ステップ 3 — ユーザによる方針決定

ユーザが次のアクションを選択する：
- **3.1** 提案のブラッシュアップ（OPENMP_PLAN の内容を対話しながら改善）
- **3.2** 実行する並列化方針を確定し、ステップ 4 へ
- **3.3** 追加の状況深掘り（ステップ 1 の追加計測、コード解析など）

### ステップ 4 — コード修正

確定した方針に基づきコードを修正する。

- **ビルド**: `dev/src/` で `make build` を実行。
- 修正後、コンパイルエラーがないことを確認する。
- ユーザが修正内容に納得するまで繰り返す。

### ステップ 5 — 計算実行と数値検証

修正後のコードで計算を実行し、結果の正当性を確認する。

- 基本は **OMP=1, 2, 4** で実行する（ユーザの要望に応じて適宜変更）。
  ```bash
  cd dev/src && make build
  cd dev/test_cases && OMP_NUM_THREADS=N OMP_PROC_BIND=true OMP_PLACES=cores \
    ../bin/multall-open21.3-s < two-stg-LP-ST+steam.dat > run_YYYYMMDD_HHMM.out
  ```
- 計算後、`stage.log` を日時付きでリネーム保存する：
  ```bash
  cp stage.log stage.log.$(date +%Y%m%d_%H%M)
  ```
- **数値検証**: 以下の2段階で確認する。不一致の場合はステップ 4 に戻る。
  1. **収束指標**: OMP=1 の EMAX/EAVG/ECONT/FLOW が変更前と一致すること（last-digit の浮動小数点差のみ許容）。
  2. **全体性能**: stdout ログから全体（WHOLE MACHINE）の圧力比(PR)・等エントロピー効率(eta_TT, eta_TS)・出力(Power)・流量(Flow In/Out) を抽出し、変更前と比較する。相対差 0.001% 未満であること。
  - 比較には `dev/scripts/compare_performance.sh` を使用する：
    ```bash
    bash dev/scripts/compare_performance.sh run_before.out run_after.out
    ```

### ステップ 6 — 区間別の計算時間比較

並列化前後の計算時間を、各区間で細分化して整理する。

- **LOOP: TOTAL** を基準に速度向上を評価する（MAIN: TOTAL にはセットアップ時間が含まれるため）。
- 対象セクションだけでなく、他セクションへの影響（リグレッション）もチェックする。
- 比較結果は表形式で整理し、ユーザに報告する。

### ステップ 7 — 効果分析と次のアクション提案

並列化の効果を分析し、次のアクションを提案する。

- 効果があった場合：その要因（並列粒度・メモリバウンド・スケーリング特性等）を報告。
- 効果がなかった／不十分だった場合：原因を分析し、次の打ち手（別の並列化戦略・変更の revert など）を提案する。
- 次の並列化ターゲットの候補とその優先度を報告する。

### ステップ 8 — ユーザの指示

ユーザが方針を連絡し、上記ステップのいずれかに戻る。

---

## 5. 変更時の注意事項

- 並列化対象のサブルーチンを変更する際は、変更前後で `stage.log` の数値差異を確認する。
- 新しい OpenMP 領域を追加したら、`OMP_NUM_THREADS=1` での結果がシリアル版と一致することを必ず確認する。
- 数値検証は **収束指標**（EMAX/EAVG/ECONT/FLOW）と **全体性能**（PR/eta_TT/eta_TS/Power/Flow）の両方で行う。全体性能の比較には `dev/scripts/compare_performance.sh` を使用する。
- `GOTO` 文を含むループの並列化は避ける（構造化が先）。
- I/O（`WRITE`, `READ`）を含むループの並列化は原則禁止。必要な場合は `CRITICAL` セクションで保護する。

---

## 6. ファイル構成

- メインソース: `dev/src/multall-open21.3-s1.0.f`（開発版）
- オリジナル: `dev/src/multall-open-21.3.f`（リファレンス、変更禁止）
- 共通ブロック定義: `dev/src/commall-open-21.3`
- ビルド: `dev/src/Makefile`
- 並列化方針: `dev/docs/OPENMP_PLAN_*.md`（セクションごとの並列化計画）
- 検証・ユーティリティスクリプト: `dev/scripts/`（検証用スクリプトは必ずここに配置する）
- テスト入力・出力: `dev/test_cases/`（入力データと `stage.log.*` のみ配置。スクリプトは置かない）
- 進捗記録: `dev/docs/DAILY_PROGRESS_YYYY-MM-DD.md`（下記セクション 7 参照）

---

## 7. 進捗記録（Daily Progress）

- 実施内容とその結果の要約（並列化の成否、トライした内容、性能変化など）は、`dev/docs/DAILY_PROGRESS_YYYY-MM-DD.md` に記録する。
- **ユーザから記録を残すよう指示があったタイミング**（セッション終了時など）で作成・追記する。指示がない限り自動的には作成しない。
- 記録後、変更を **git commit & push** する。
- 同日に既にファイルが存在する場合は追記する。
- 記載内容の目安：
  - 対象サブルーチン・ループ
  - 変更内容（並列化手法、SIMD 適用など）
  - 結果（速度変化、成功/失敗、原因分析）
  - 次のアクション
- 進捗記録の末尾には、**現在の速度向上状況**を記載する。解析条件（マシン・ステップ数）ごとに、LOOP: TOTAL の original → OMP=1/2/4/8 の実行時間と対 original 倍率を表形式で整理する。
  ```
  ### 現在の速度向上状況
  #### ローカル (i5-12400F 6C/12T, 100ステップ)
  | | original | OMP=1 | OMP=2 | OMP=4 |
  |---|---|---|---|---|
  | LOOP: TOTAL | 54.4s | 52.9s | 37.3s | 31.3s |
  | 対 original | — | 1.03x | 1.46x | 1.74x |

  #### AWS (c7i 8C/16T, 100ステップ)
  | | original | OMP=1 | OMP=2 | OMP=4 | OMP=8 |
  | ...
  ```

### セッション開始時の手順

- セッション開始時、AI はまず最新の `dev/docs/DAILY_PROGRESS_*.md` を読み、現在の進捗状況・性能ベースライン・次のアクションを把握する。
- ユーザが指定した並列化対象がある場合は、対応する `dev/docs/OPENMP_PLAN_*.md` も参照し、データ依存性分析や変数分類を再利用する。
- これらの情報を踏まえた上で、ユーザの指示に対応する。

---

## 8. AWS ベンチマーク手順

### 8.1 サーバー情報

- **インスタンス**: AWS EC2 c7i（Intel Xeon Platinum 8488C, 8 物理コア / 16 vCPU）
- **SSH 設定**: `~/.ssh/config` に `Host ec2-c7i` として登録（IP はインスタンス起動ごとに変わるため都度更新）
- **OS**: Ubuntu 24.04, gfortran 13.3.0
- **AWS 上のパス**: `~/MULTALL-project/dev/`（ローカルと同じ構造）

### 8.2 環境構築（新規インスタンス起動時）

```bash
# 1. SSH config の HostName を新しい IP に更新
vi ~/.ssh/config

# 2. ビルドツールのインストール
ssh ec2-c7i "sudo apt-get update -qq && sudo apt-get install -y -qq gfortran make"

# 3. ディレクトリ作成
ssh ec2-c7i "mkdir -p ~/MULTALL-project/dev/{src,bin,test_cases,scripts}"

# 4. ファイル転送
cd ~/work/MULTALL-project
scp dev/src/multall-open21.3-s1.0.f dev/src/multall-open-21.3.f \
    dev/src/commall-open-21.3 dev/src/Makefile \
    ec2-c7i:~/MULTALL-project/dev/src/

scp dev/test_cases/two-stg-LP-ST+steam.dat dev/test_cases/intype \
    dev/test_cases/mixbconds \
    ec2-c7i:~/MULTALL-project/dev/test_cases/

scp original/test_cases/props_table.dat original/test_cases/stopit \
    ec2-c7i:~/MULTALL-project/dev/test_cases/

scp dev/scripts/run_100stp_benchmark.sh dev/scripts/diff_stage_logs.sh \
    ec2-c7i:~/MULTALL-project/dev/scripts/

# 5. コンパイル（OpenMP 版 + オリジナル版）
ssh ec2-c7i "cd ~/MULTALL-project/dev/src && make build"
ssh ec2-c7i "cd ~/MULTALL-project/dev/src && make build_original"
```

### 8.3 ベンチマーク実行

original バイナリは `/dev/tty` を開いて Y/N プロンプトを出すため、
**ユーザーが AWS に SSH して直接実行する**（非対話 SSH ではプロンプトが動作しない）。

```bash
# AWS に SSH 接続
ssh ec2-c7i

# ベンチマークスクリプトを実行
cd ~/MULTALL-project/dev/test_cases
bash ~/MULTALL-project/dev/scripts/run_100stp_benchmark.sh
# → original の Y/N プロンプトには Y を入力
# → original, OMP=1, 2, 4, 8 の順に実行される
# → 最後に LOOP: TOTAL のサマリーが表示される
```

### 8.4 結果の回収と評価

```bash
# ローカルから stage.log を回収
scp ec2-c7i:~/MULTALL-project/dev/test_cases/stage.log.aws_* \
    dev/test_cases/

# LOOP: TOTAL で速度向上量を評価
grep "LOOP: TOTAL" dev/test_cases/stage.log.aws_*
```

評価基準:
- **LOOP: TOTAL** の original → OMP=8 での倍率（スケーリング）で速度向上を評価する。ステップ数が少ない（100ステップ）ため、MAIN: TOTAL にはセットアップ時間が含まれ正確な比較にならない。**常に LOOP: TOTAL を基準とする**。
- セクション別の内訳で並列化効果を確認（特に新規並列化セクション）
- **数値検証**（2段階）:
  1. **収束指標**: original と OMP=1 の `stage.log` を比較し、EMAX/EAVG/ECONT/FLOW の値が大きく変化していないことを確認する。last-digit の浮動小数点差（例: 0.22094 vs 0.22093）のみ許容。桁が変わるような差異があれば並列化のバグを疑う。
  2. **全体性能**: stdout ログ（`run_*.out`）から全体の圧力比(PR)・等エントロピー効率(eta_TT, eta_TS)・出力(Power)・流量(Flow In/Out) を抽出し、original と OMP=1 で比較する。相対差 0.001% 未満であること。
  ```bash
  bash dev/scripts/compare_performance.sh run_original.out run_omp1.out
  ```

### 8.5 注意事項

- AWS インスタンスは **使い終わったら停止する**（課金防止）。
- IP アドレスはインスタンス起動ごとに変わるため、`~/.ssh/config` の `HostName` を都度更新すること。
- ベンチマーク中は他のプロセスを同時に実行しない（タイミング精度のため）。
- `OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE` を必ず設定する。
