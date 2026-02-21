# Daily Progress — 2026-02-21

## COMMON → MODULE + ALLOCATABLE リファクタリング完了

### 概要
`commall-open-21.3` の全 COMMON ブロックを `multall_data.f` MODULE に置き換え、ID/JD/KD/MAXKI 依存の全配列を ALLOCATABLE 化した。配列サイズが実行時にメッシュサイズに合わせて動的に設定される。

### 変更内容

#### multall_data.f（新規 MODULE）
- **~270 配列** を ALLOCATABLE 化（ID/JD/KD 依存 ~238 + MAXKI 依存 ~35）
- Fixed PARAMETERs（変更なし）: NRS=21, IG1=32, JG1=1000, KG1=41, IG2=25, JG2=500, KG2=21, JG3=100, NCWL=100, NBLED=25, ITB=150, JTB=150
- Runtime 変数:
  - ID (初期値 64), JD (初期値 2500), KD (初期値 82) — commall と整合
  - MAXKI — `ALLOC_MULTALL_DATA` 内で `MAX(ID, KD)` として設定
- CONTAINS:
  - `PRE_READIN_DIMS`: 入力ファイルから IM/JM/KM/NROWS を事前推定
  - `ALLOC_MULTALL_DATA(ID_REQ, JD_REQ, KD_REQ, NROWS_REQ)`: 全配列の ALLOCATE
  - `DEALLOC_MULTALL_DATA`: 全配列の DEALLOCATE
- BKSTRESS（12 × 3D 配列: TXX,TXR,TXT,TRX,TRR,TRT,TTX,TTR,TTT,QXX,QRR,QTT）もモジュールに統合
- BKODDS はサブルーチン間共有不要のためローカル DIMENSION 配列に変換

#### multall-open21.3-s1.0.f（メインソース）
- 47 箇所の `INCLUDE 'commall-open-21.3'` → `USE MULTALL_DATA` に置換
- NEW_READIN パス: `PRE_READIN_DIMS` → `ALLOC_MULTALL_DATA` で動的サイズ確保
- OLD_READIN パス: `ALLOC_MULTALL_DATA(64,2500,82,0)` で commall 互換サイズ確保
- COMMON/BKODDS/ → ローカル DIMENSION 配列（NEW_READIN, OLD_READIN 内）
- COMMON/BKSTRESS/ → USE で参照（2 つの viscous サブルーチン）
- CHECK_FLOW/BLADE_FLOW: `REAL, ALLOCATABLE, SAVE` + lazy allocation に変換
- 30 箇所のローカル `DIMENSION X(ID,JD,KD)` 等 → 自動配列（gfortran が 64KB 超をヒープ割当）

#### Makefile
- MODULE_SRC を依存関係に追加

### 検証結果

#### ビルド
- `make clean && make build`: エラー 0 ✅（Fortran 2018 deprecation warning のみ）

#### 数値検証（original_timer vs OMP=1, two-stg 100ステップ）
| 指標 | original_timer | OMP=1 (ALLOCATABLE) | Rel.Diff |
|---|---|---|---|
| EMAX | 2.7797 | 2.7797 | 0.000% |
| EAVG | 0.1316 | 0.1316 | 0.000% |
| Pressure Ratio | 8.99166107 | 8.99166298 | +0.000021% ✅ |
| eta_TT | 0.868755102 | 0.868756652 | +0.000178% ✅ |
| eta_TS | 0.801260710 | 0.801264286 | +0.000446% ✅ |
| Power (kW) | 59476.4570 | 59476.6680 | +0.000355% ✅ |
| Flow In (kg/s) | 198.140503 | 198.140579 | +0.000038% ✅ |

#### 動的サイズ確認
```
ALLOC_MULTALL_DATA: ID,JD,KD,MAXKI,NROWS = 64 979 64 64 4
```
- commall 固定値: ID=64, JD=2500, KD=82, MAXKI=82
- ALLOCATABLE 版: ID=64, JD=979, KD=64, MAXKI=64（実メッシュサイズ）

#### ローカルベンチマーク（i5-12400F 6C/12T, two-stg 100ステップ）

| | original_timer | OMP=1 | OMP=2 | OMP=4 |
|---|---:|---:|---:|---:|
| LOOP: TOTAL | 44.26s | 44.03s | 30.52s | 24.17s |
| 対 original | — | 1.01x | 1.45x | 1.83x |
| MAIN: TOTAL | 60.92s | 60.39s | 40.17s | 30.33s |

注: commall の PARAMETER 値が前回計測時 (ID=128, JD=1000) から変更されている（ID=64, JD=2500）ため、前回の original 54.42s と今回の 44.26s は直接比較できない。同一セッション内の original_timer vs OMP=1 では ALLOCATABLE 化による速度差はほぼなし（-0.5%）。これは two-stg ケースで ID=64 が実メッシュと一致し I 方向ストライドに差がないため。HP Steam ケース（IM=37 vs ID=64）では改善が期待される。

### 主要セクション別タイマー比較（秒）

| セクション | original | OMP=1 | OMP=2 | OMP=4 | OMP1→4x |
|---|---:|---:|---:|---:|---:|
| PRESSURE/TEMP | 7.90 | 10.74 | 5.80 | 3.37 | 3.19x |
| SMOOTH_VAR | 7.95 | 6.88 | 4.55 | 3.22 | 2.14x |
| MOM FLUX BUILD | 4.40 | 4.26 | 3.80 | 3.54 | 1.20x |
| DENSITY | 3.83 | 3.71 | 2.60 | 2.22 | 1.67x |
| ENERGY | 3.78 | 3.68 | 2.67 | 2.17 | 1.70x |
| MOM-X | 3.82 | 3.72 | 2.52 | 2.06 | 1.81x |
| MOM-T | 3.85 | 3.69 | 2.53 | 2.06 | 1.79x |
| MOM-R | 3.84 | 3.69 | 2.55 | 2.02 | 1.83x |
| STEP/DAMP | 3.16 | 3.88 | 2.54 | 1.95 | 1.99x |
| CELL->NODE | 3.57 | 2.61 | 1.74 | 1.61 | 1.62x |
| DELTA/STORE | 2.55 | 2.63 | 2.40 | 2.27 | 1.16x |
| MG AGG | 1.69 | 2.37 | 1.42 | 1.14 | 2.08x |
| EVERY 5 STEPS | 3.91 | 1.50 | 1.36 | 1.20 | 1.25x |

---

### 現在の速度向上状況

#### ローカル (i5-12400F 6C/12T, 100ステップ, commall ID=64/JD=2500/KD=82)
| | original | OMP=1 | OMP=2 | OMP=4 |
|---|---|---|---|---|
| LOOP: TOTAL | 44.26s | 44.03s | 30.52s | 24.17s |
| 対 original | — | 1.01x | 1.45x | 1.83x |

#### AWS (c7i 8C/16T, two-stg 100ステップ, commall ID=128/JD=1000) — 02-15 計測
| | original | OMP=1 | OMP=2 | OMP=4 | OMP=8 |
|---|---|---|---|---|---|
| LOOP: TOTAL | 81.11s | 76.01s | 39.90s | 22.11s | 13.33s |
| 対 original | — | 1.07x | 2.03x | 3.67x | **6.09x** |

注: AWS 数値は ALLOCATABLE 化前（PARAMETER 固定, commall ID=128/JD=1000）の計測。ALLOCATABLE 版での AWS 再計測が必要。

---

### 次のアクション

#### 優先度 高
1. **COMMON 版で OMP=1,2,4 ベンチマーク**: two-stg + steamtest 両方でスケーリング計測
2. **COMMON 版のメモリ効率改善検討**: `commall-open-21.3` の PARAMETER 値チューニング

#### 優先度 中
3. **SMOOTH_VAR STREAMWISE/RESET D**: スケーリング改善
4. **STEP/DAMP COLLAPSE(2)**: DAMP feedback に影響しない DO 1502 部分

---

## ALLOCATABLE 版の性能問題発見と COMMON 版への revert

### 問題: PRESSURE/TEMP が OMP=1 で異常に遅い

ベンチマーク中に、ALLOCATABLE 版の OMP=1 で PRESSURE/TEMP が original_timer 比 **+44%** も遅いことが判明。

| テストケース | original_timer | ALLOCATABLE OMP=1 | 増加率 |
|---|---:|---:|---|
| two-stg 100stp | 7.90s | 10.74s | +36% |
| steamtest 5000stp | 29.24s | 42.12s | +44% |

### steamtest+steam テストケースの整備

steamtest+steam を初めて動作確認する際に以下の問題を修正:
- `intype` が空ファイル → `N`（NEW 形式）に設定
- `props_table.dat` が存在しなかった → two-stg から複写（両方 IFGAS=3, 蒸気テーブル）
- `PRE_READIN_DIMS` がパターンマッチングで失敗 → NEW_READIN の READ 構造をそのまま追跡する方式に完全書き直し

### steamtest+steam ベンチマーク（ALLOCATABLE版）

| | original_timer | OMP=1 | OMP=2 | OMP=4 |
|---|---:|---:|---:|---:|
| LOOP: TOTAL | 174.26s | 154.26s | 111.17s | 97.80s |
| PRESSURE/TEMP | 29.24s | 42.12s | 23.40s | 14.31s |
| 対 original | — | 1.13x | 1.57x | 1.78x |

数値検証: step 2686 で収束、EAVG=0.0002, ECONT=0.0059, FLOW=19.8477 完全一致 ✅

### MOMENTUM FLUX: PARALLEL DO vs SINGLE の分析

steamtest (IM=46) ではメッシュが小さく、MOMENTUM FLUX の PARALLEL DO 部分がスケールせず SINGLE 部分（fork/join オーバーヘッド）が増加:

| 部分 | OMP=1 | OMP=2 | OMP=4 |
|---|---:|---:|---:|
| PARALLEL DO 合計 | 15.56s | 14.22s | 13.99s |
| SINGLE 合計 | 0.66s | 1.11s | 1.78s |
| 合計 | 16.23s | 15.34s | 15.77s |

OMP=2→4 で合計が逆に悪化。並列粒度不足。

### 原因分析: ALLOCATABLE ディスクリプタの間接参照コスト

IFGAS=3（蒸気テーブル）の場合、PRESSURE/TEMP ループ内で毎格子点 `CALL TABSEARCH` が呼ばれる。TABSEARCH は `USE MULTALL_DATA` 経由で 15個の2Dテーブル配列（`P_TAB`, `T_TAB`, `ENT_TAB`, `GA_PV_TAB`, `DRY_TAB` + 各 `_HDI/_HDJ` ）にアクセス。

- **ALLOCATABLE**: ディスクリプタ（ポインタ + サイズ情報）経由の間接参照 × 15配列 × 全格子点 × 全ステップ
- **COMMON**: 固定アドレス → コンパイラが直接アクセスコード生成

### `-flto` (Link Time Optimization) の検証

`-flto` でインライン展開によるディスクリプタ参照のホイストを試みたが効果不十分:

| テストケース | セクション | No LTO | With LTO | 改善 |
|---|---|---:|---:|---|
| two-stg 100stp | PRESSURE/TEMP | 10.74s | 10.34s | -3.7% |
| two-stg 100stp | LOOP: TOTAL | 44.03s | 46.57s | **+5.8% 悪化** |
| steamtest | PRESSURE/TEMP | 42.12s | 38.97s | -7.5% |

two-stg で LOOP: TOTAL が悪化 → `-flto` は revert。

### COMMON 版との A/B 比較

`998c77e`（ALLOCATABLE 化前の最終コミット）のソースを取り出し、`commall-open-21.3`（ID=64, JD=2500, MAXKI=82）で別バイナリをビルド。

#### two-stg 100stp OMP=1

| セクション | ALLOCATABLE | COMMON | 差 |
|---|---:|---:|---|
| **LOOP: TOTAL** | **44.03s** | **39.49s** | **-10.3%** |
| **PRESSURE/TEMP** | **10.74s** | **5.54s** | **-48.4%** |
| VISCOUS/TURB | 3.50s | 3.21s | -8.3% |
| CELL->NODE | 2.61s | 2.27s | -13.0% |
| MG AGG | 2.37s | 2.13s | -10.1% |
| SMOOTH_VAR | 6.88s | 8.54s | +24.1% (COMMON が遅い) |
| TSTEP: DENSITY | 3.71s | 3.87s | +4.3% |

#### steamtest 5000stp OMP=1

| セクション | ALLOCATABLE | COMMON | 差 |
|---|---:|---:|---|
| **LOOP: TOTAL** | **154.26s** | **152.22s** | **-1.3%** |
| **PRESSURE/TEMP** | **42.12s** | **21.53s** | **-48.9%** |

### 判断: COMMON 版を正とする

- PRESSURE/TEMP は COMMON で約半分（ALLOCATABLE の間接参照コスト確認）
- LOOP: TOTAL は two-stg で -10%、steamtest で -1.3%
- SMOOTH_VAR は COMMON で 24% 遅い箇所もあるが、PRESSURE/TEMP の改善が支配的
- **コミット 87c2f34**: COMMON 版を `multall-open21.3-s1.0.f` として正、ALLOCATABLE 版は `-alloc.f` としてバックアップ保持

### 教訓

1. **ALLOCATABLE はパフォーマンスコストがある**: 特に頻繁に呼ばれるサブルーチン内で MODULE 変数を多数参照する場合、ディスクリプタ間接参照のオーバーヘッドが蓄積
2. **TABSEARCH が最も影響を受けた**: 15個テーブル配列 × 全格子点 × 全ステップ
3. **COMMON の固定長配列はメモリ浪費だが高速**: コンパイル時アドレス確定 → 直接アクセスコード生成
4. **`-flto` は万能ではない**: gfortran では効果限定的で副作用あり

---

### 現在の速度向上状況

#### ローカル (i5-12400F 6C/12T, two-stg 100ステップ, COMMON版)

| | original_timer | COMMON OMP=1 |
|---|---:|---:|
| LOOP: TOTAL | 44.26s | 39.49s |
| 対 original | — | 1.12x |

※ OMP=2,4 は COMMON 版で未計測（次セッションで実施予定）

#### ローカル (i5-12400F 6C/12T, steamtest 5000ステップ, ALLOCATABLE版)

| | original_timer | OMP=1 | OMP=2 | OMP=4 |
|---|---:|---:|---:|---:|
| LOOP: TOTAL | 174.26s | 154.26s | 111.17s | 97.80s |
| 対 original | — | 1.13x | 1.57x | 1.78x |
