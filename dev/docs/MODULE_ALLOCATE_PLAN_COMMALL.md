# commall-open-21.3 単一 module 化 + pre-NEW_READIN 先読み ALLOCATE 計画

## 目的
- `commall-open-21.3` の COMMON を単一 module (`MULTALL_DATA`) に集約する。
- `NEW_READIN` 実行前に入力を先読みし、必要寸法を決めてから `ALLOCATE` する。
- 固定 `PARAMETER` 依存を最小化し、`IM/JM/KM` 実寸に合わせてキャッシュ効率を改善する。

## 先読み方式（今回の主方針）
`NEW_READIN` をそのまま2回呼ぶのではなく、以下の2段階に分解する。

1. `PRE_READIN_DIMS`（新規）
   - 入力ファイルから **寸法と必要上限のみ** を読む。
   - 例: `IM, JM, KM, NROWS, NCOOLB, NCOOLW, NBLEED, ITAB, JTAB`。
   - 物理場や大配列の値は読まない。

2. `ALLOC_MULTALL_DATA`（新規）
   - `PRE_READIN_DIMS` で得た寸法を受け、配列を一括 `ALLOCATE`。

3. `NEW_READIN`（既存）
   - 既に確保済み配列へ、本来の全データを読む。

この構成なら、I/O重複を最小化しつつ、`NEW_READIN` 実行前に必要サイズ確定が可能。

### 現在の実装ステータス（2026-02-21）
- 暫定実装として、`PRE_READIN_DIMS` は先に `NEW_READIN` を1回実行して
  `IM/JM/KM/NROWS` を取得し、`REWIND(5)` 後に本番 `NEW_READIN` を再実行する。
- 次ステップで、`PRE_READIN_DIMS` を「寸法のみ読取」の専用パーサに置換する。

## PARAMETER の扱い再設計
現行:

      PARAMETER(ID=128,JD=1000,KD=82,MAXKI=128,NRS=21,IG1=32,
     &          JG1=1000,KG1=41)

      PARAMETER(IG2=25,JG2=500,KG2=21,JG3=100,NCWL=100,NBLED=25)

再設計方針:
- `ID/JD/KD` は **実行時サイズ変数**（`IM/JM/KM`）へ置換対象。
- `MAXKI/NRS/NCWL/NBLED` は第1段階では「上限PARAMETER維持」でも可。
- `IG1/JG1/KG1, IG2/JG2/KG2, JG3` は MG 配列用のため、
  - 第1段階: 現状 PARAMETER 維持
  - 第2段階: `IR/JR/KR` と実メッシュから動的算出して allocatable 化

## 実装フェーズ（改訂版）

### Phase A: 先読み基盤の追加（0.5〜1日）
- `MULTALL_DATA` に以下を追加:
  - 実行時寸法: `IMR,JMR,KMR,NROWSR,...`（名称は既存と衝突しないもの）
  - `SUBROUTINE PRE_READIN_DIMS(...)`
  - `SUBROUTINE ALLOC_MULTALL_DATA(...)`
  - `SUBROUTINE ASSERT_ALLOCATED()`
- `PRE_READIN_DIMS` は「読み飛ばし中心」で必要値のみ取得。

### Phase B: 呼び出し順の変更（0.5日）
- メイン開始部を次順序に変更:
  1) `CALL PRE_READIN_DIMS(...)`
  2) `CALL ALLOC_MULTALL_DATA(...)`
  3) `CALL NEW_READIN(...)`
- `NEW_READIN` 内で固定長前提の初期化があれば、確保済み配列前提へ修正。

### Phase C: 配列の allocatable 化（1〜2日）
- 優先度1（3D主配列）:
  - `STORE, TEMP1..4, STORE2, SGEN`
  - `RO, P, ROE, ROVX, ROVR, ROVT, RORVT, HO, ...`
  - `ABX/ABR/AQX/AQR/ASX/ASR/VOL/...`
- 優先度2（2D/1D配列）
- 優先度3（MG・テーブル関連）

### Phase D: INCLUDE→USE 移行（段階適用）
- 一括置換はしない。
- サブルーチン単位で宣言順を守って `USE MULTALL_DATA` に置換。
- 各塊ごとに `make build` を通す。

### Phase E: 検証（1日）
- ビルド: `cd dev/src && make build`
- 数値検証: OMP=1 の収束指標・全体性能を変更前と比較
- 性能: OMP=1/2/4/8 の `LOOP: TOTAL` を比較

## 変更対象ファイル（改訂）
- 更新: `dev/src/multall_data.f`
- 更新: `dev/src/multall-open21.3-s1.0.f`
- 必要に応じ更新: `dev/src/multall-open-21.3_timer.f`
- 更新: `dev/src/Makefile`（実施済み）

## リスクと対策（先読み方式特有）
- リスク1: `PRE_READIN_DIMS` と `NEW_READIN` の読取ロジック不一致
  - 対策: 先読み対象は寸法と件数のみ、解釈ルールを共通化
- リスク2: 先読み後にファイル位置がずれる
  - 対策: `REWIND` または再OPENで `NEW_READIN` を常に先頭から開始
- リスク3: 上限配列（MG/テーブル）不足
  - 対策: 第1段階は PARAMETER 維持、段階的に動的化

## 受け入れ基準
1. `PRE_READIN_DIMS -> ALLOC -> NEW_READIN` の順で実行できる。
2. `make build` が通る。
3. OMP=1 の数値基準を満たす。
4. 3D主配列が実寸確保され、`ID/JD/KD` 固定依存が実質解消される。
