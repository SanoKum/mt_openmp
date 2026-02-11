# MULTALL Project — Copilot Instructions

このリポジトリは Fortran **fixed-form (.f)** の CFD ソルバです。
以下のルールを **すべてのコード生成・編集で厳守** してください。

---

## 1. Fixed-Form Fortran 基本規則

- ソースは **fixed-form (`.f`)** のみ。free-form (`.f90`) は使わない。
- コードは **列 7〜72** に収める。
- 継続行は **列 6 に `&` 等の継続文字**。
- コメントは **列 1 に `C` または `c`**。`!` によるインラインコメントは使わない。

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

## 3. コンパイル・実行の受け入れ条件

### 3.1 コンパイル
- `gfortran` 使用時: **`-fopenmp`** フラグ必須。
- `ifort` 使用時: **`-qopenmp`** フラグ必須。
- Makefile の `FFLAGS` に OpenMP フラグが含まれていることを確認する。

### 3.2 実行時検証
- `OMP_DISPLAY_ENV=TRUE` で実行し、スレッド数・スケジュール設定がログに出ることを確認。
- `OMP_NUM_THREADS` で明示的にスレッド数を制御。

### 3.3 性能目標
- 1 スレッド → N スレッドで **最低 30% の実行時間改善** を目標とする。

---

## 4. コード変更後の検証手順

コードを変更したら、以下の手順で必ず動作確認と性能評価を行う。

### 4.1 ビルドと実行

1. `dev/src/` で `make build` を実行してコンパイルする。
2. `dev/test_cases/` で入力ファイル `two-stg-LP-ST+steam.dat` を使って計算を実行する。
3. 実行時のスレッド数は **当面 2 並列（`OMP_NUM_THREADS=2`）** とする。

```bash
cd dev/src && make build
cd dev/test_cases && OMP_NUM_THREADS=2 OMP_PROC_BIND=true OMP_PLACES=cores \
  ../bin/multall-open21.3-s < two-stg-LP-ST+steam.dat > run_YYYYMMDD_HHMM.out
```

### 4.2 stage.log の保存と比較

- 計算完了後に生成される `stage.log` を、**日付と時刻がわかる名前** にリネームして保存する。
  ```bash
  cp stage.log stage.log.$(date +%Y%m%d_%H%M)
  ```
- 計算時間（CPU time）を `stage.log.original` と比較し、速度向上量を評価する。

### 4.3 並列化・ベクトル化の効果検証

変更箇所が OpenMP 並列化やベクトル演算化に関わる場合は、以下の点も確認する：

- **並列化の確認**: `OMP_DISPLAY_ENV=TRUE` で実行し、スレッド数・スケジュール設定が正しく適用されていること。
- **性能比較**: `OMP_NUM_THREADS=1` と `OMP_NUM_THREADS=2` で実行し、実際にスレッド並列の効果が出ているか確認する（並列化しているつもりが効いていない、というケースを防ぐ）。
- **ベクトル化の確認**: コンパイル時に生成される `vec.log` を確認し、対象ループが実際にベクトル化されているか確認する。`vec-missed` の報告があれば原因を調査する。

---

## 5. 変更時の注意事項

- 並列化対象のサブルーチンを変更する際は、変更前後で `stage.log` の数値差異を確認する。
- 新しい OpenMP 領域を追加したら、`OMP_NUM_THREADS=1` での結果がシリアル版と一致することを必ず確認する。
- `GOTO` 文を含むループの並列化は避ける（構造化が先）。
- I/O（`WRITE`, `READ`）を含むループの並列化は原則禁止。必要な場合は `CRITICAL` セクションで保護する。

---

## 6. ファイル構成

- メインソース: `dev/src/multall-open21.3-s1.0.f`（開発版）
- オリジナル: `dev/src/multall-open-21.3.f`（リファレンス、変更禁止）
- 共通ブロック定義: `dev/src/commall-open-21.3`
- ビルド: `dev/src/Makefile`
- 検証・ユーティリティスクリプト: `dev/scripts/`（検証用スクリプトは必ずここに配置する）
- テスト入力・出力: `dev/test_cases/`（入力データと `stage.log.*` のみ配置。スクリプトは置かない）
- 進捗記録: `dev/docs/DAILY_PROGRESS_YYYY-MM-DD.md`（下記セクション 7 参照）

---

## 7. 進捗記録（Daily Progress）

- 実施内容とその結果の要約（並列化の成否、トライした内容、性能変化など）は、`dev/docs/DAILY_PROGRESS_YYYY-MM-DD.md` に記録する。
- **ユーザから記録を残すよう指示があったタイミング**（セッション終了時など）で作成・追記する。指示がない限り自動的には作成しない。
- 同日に既にファイルが存在する場合は追記する。
- 記載内容の目安：
  - 対象サブルーチン・ループ
  - 変更内容（並列化手法、SIMD 適用など）
  - 結果（速度変化、成功/失敗、原因分析）
  - 次のアクション

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
- **数値検証**: original と OMP=1 の `stage.log` を比較し、EMAX/EAVG/ECONT/FLOW の値が大きく変化していないことを確認する。last-digit の浮動小数点差（例: 0.22094 vs 0.22093）のみ許容。桁が変わるような差異があれば並列化のバグを疑う。

### 8.5 注意事項

- AWS インスタンスは **使い終わったら停止する**（課金防止）。
- IP アドレスはインスタンス起動ごとに変わるため、`~/.ssh/config` の `HostName` を都度更新すること。
- ベンチマーク中は他のプロセスを同時に実行しない（タイミング精度のため）。
- `OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE` を必ず設定する。
