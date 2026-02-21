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
1. **AWS ベンチマーク**: ALLOCATABLE 版で two-stg + HP Steam の計測。特に HP Steam（IM=37 vs 旧 ID=128）でのキャッシュ効率改善効果を確認
2. **git commit & push**: ALLOCATABLE 化の全変更をコミット

#### 優先度 中
3. **SMOOTH_VAR STREAMWISE/RESET D**: スケーリング改善（3.6-3.9x → 目標 5x+）
4. **STEP/DAMP COLLAPSE(2)**: DAMP feedback に影響しない DO 1502 部分

#### 優先度 低
5. **K-only ループの COLLAPSE(2)**: trip 数増加による負荷分散改善
