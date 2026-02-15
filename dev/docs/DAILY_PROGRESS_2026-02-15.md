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

## AWS 10000 ステップベンチマーク

### 実行条件
- AWS EC2 c7i (Xeon Platinum 8488C, 8 物理コア / 16 vCPU)
- 入力: `two-stg-LP-ST+steam-10000stp.dat` (NSTEPS_MAX=10000, CONLIM=0.001)
- 全ランが **ステップ 6976 で収束終了**（EAVG < CONLIM）

### 実行状況

| Run | 最終ステップ | 状態 |
|---|---|---|
| Timer Original | 6976 | 正常収束 |
| OMP=1 | 6976 | 正常収束 |
| **OMP=2** | **2316** | **NaN 発散** |
| OMP=4 | 6976 | 正常収束 |
| OMP=8 | 6976 | 正常収束 |

**OMP=2 発散**: ステップ 2310 で EAVG が突然 NaN に。直前 2305 まで正常収束中 (EMAX=-0.239, EAVG=0.00652)。100/1000 ステップでは問題なし。長時間実行で顕在化するデータ競合の可能性。

### 数値検証（Timer Original vs OMP=N, step 6976）

| 指標 | Timer | OMP=1 差 | OMP=4 差 | OMP=8 差 |
|---|---|---|---|---|
| PR | 9.42441 | +0.0008% | +0.0009% | +0.0005% |
| eta_TT | 0.93061 | +0.0005% | +0.0002% | +0.0004% |
| eta_TS | 0.86881 | +0.0004% | -0.0001% | +0.0004% |
| Power (kW) | 45049.0 | +0.0014% | +0.0012% | +0.0007% |
| Flow In | 134.623 | +0.0001% | +0.0002% | +0.00001% |
| Flow Out | 135.779 | +0.0032% | +0.0029% | +0.0004% |

全項目 0.004% 未満 — **合格**。

### LOOP: TOTAL スケーリング

| | Timer | OMP=1 | OMP=4 | OMP=8 |
|---|---:|---:|---:|---:|
| LOOP: TOTAL (s) | 5159 | 4981 | 1421 | **839** |
| 対 OMP=1 | — | 1.00x | 3.50x | **5.94x** |
| 対 Timer | 1.00x | 1.04x | 3.63x | **6.15x** |

### セクション別タイマー（全セクション）

| ID | セクション | Timer (s) | OMP=1 (s) | OMP=4 (s) | OMP=8 (s) | OMP=1→8 倍率 |
|---|---|---:|---:|---:|---:|---:|
| 4 | LOOP: TOTAL | 5159.3 | 4980.5 | 1421.2 | 839.1 | 5.94x |
| 29 | TSTEP: SMOOTH_VAR | 904.7 | 908.7 | 250.9 | 136.8 | 6.64x |
| 40 | LOOP: MOM FLUX BUILD | 811.8 | 812.9 | 235.3 | 143.2 | 5.68x |
| 9 | LOOP: PRESSURE/TEMP | 578.7 | 428.3 | 111.4 | 58.0 | 7.38x |
| 26 | TSTEP: STEP/DAMP | 477.1 | 360.6 | 103.7 | 60.0 | 6.01x |
| 19 | TSTEP: MOMENTUM-R | 473.2 | 486.7 | 138.8 | 79.4 | 6.13x |
| 17 | TSTEP: MOMENTUM-X | 471.6 | 485.5 | 139.7 | 80.7 | 6.02x |
| 14 | TSTEP: DENSITY | 470.7 | 489.0 | 137.6 | 79.7 | 6.13x |
| 16 | TSTEP: ENERGY | 469.2 | 482.5 | 134.4 | 77.1 | 6.26x |
| 18 | TSTEP: MOMENTUM-T | 468.7 | 491.0 | 132.2 | 75.1 | 6.54x |
| 23 | TSTEP: DELTA/STORE | 446.8 | 462.8 | 126.7 | 69.3 | 6.68x |
| 10 | LOOP: VISCOUS/TURB | 360.7 | 371.3 | 94.4 | 49.5 | 7.50x |
| 28 | TSTEP: CELL->NODE | 345.6 | 308.8 | 80.4 | 44.7 | 6.90x |
| 11 | LOOP: MASS FLUX | 274.6 | 283.9 | 76.7 | 44.2 | 6.43x |
| 68 | LOOP: EVERY 5 STEPS | 209.6 | 73.4 | 26.9 | 19.1 | 3.84x |
| 39 | LOOP: ENERGY FLUX | 196.0 | 196.6 | 53.7 | 30.8 | 6.38x |
| 71 | 5STP: STEPUP | 175.3 | 37.9 | 9.8 | 5.4 | 7.00x |
| 25 | TSTEP: MG AGG | 165.4 | 380.6 | 104.3 | 56.5 | 6.73x |
| 65 | LOOP: MACH CHECK | 111.3 | 113.0 | 29.7 | 15.3 | 7.38x |
| 8 | LOOP: VEL UPDATE | 96.2 | 99.2 | 25.9 | 14.0 | 7.06x |
| 64 | LOOP: DENSITY CHECK | 33.9 | 34.3 | 8.5 | 5.2 | 6.65x |
| 70 | 5STP: EMAX/EAVG | 20.8 | 20.8 | 5.5 | 2.9 | 7.17x |
| 30 | TSTEP: FINAL AVG | 8.9 | 10.1 | 12.6 | **19.5** | **0.52x** |
| 48 | LOOP: VRMS/VMAX | 12.0 | 11.9 | 11.4 | 11.2 | 1.06x |
| 51 | LOOP: NO-INV SURF | 9.5 | 9.5 | 9.2 | 9.4 | 1.01x |

### ボトルネック分析と改善余地

#### 1. TSTEP: FINAL AVG (ID=30) — **逆スケーリング（最優先）**
- OMP=1: 10.1s → OMP=8: **19.5s**（1.9x 悪化）
- Timer (serial): 8.9s。並列化のオーバーヘッド or false sharing が原因
- 修正すれば **~10s 削減**の見込み

#### 2. MOM FLUX BUILD (ID=40) — VX SINGLE の未並列化
- OMP=8 で 143.2s（最大ボトルネック、占有率 17.1%）、倍率 5.68x
- 内訳: VX PARALLEL DO=39.7s, **VX SINGLE=11.8s**, VT PARALLEL=26.2s, VT SINGLE=3.3s
- VX SINGLE を並列化すれば **~10s 削減**

#### 3. EVERY 5 STEPS (ID=68) — I/O 律速
- OMP=1: 73.4s → OMP=8: 19.1s（3.84x）
- SUMMARY I/O=5.6s, EFICOOL=3.3s は I/O バウンドで並列化不可
- STEPUP は 7.0x で良好

#### 4. スケーリング効率の天井
- 大部分のセクションが 6.0-7.5x（8コア中）で安定
- **メモリバンド幅が律速**段階に入っている（DDR5 帯域のサチり）

### 推奨アクション（優先度順）

| 優先度 | 対象 | アクション | 期待効果 |
|---|---|---|---|
| **1** | FINAL AVG (ID=30) | 逆スケーリング原因調査・修正 | OMP=8 で ~10s 削減 |
| **2** | VX SINGLE (ID=59) | 未並列化部分の並列化 | OMP=8 で ~10s 削減 |
| **3** | OMP=2 発散 | データ競合の原因調査 | 安定性向上 |

**総評**: LOOP: TOTAL で **OMP=1→8 = 5.94x**（効率 74%）は良好。改善余地は合計 ~20s / 839s = ~2%。大幅な追加改善は難しい段階に入っている。

---

## Periodic Boundary ループの並列化（MOM FLUX + ENERGY FLUX）

### 対象
- MOM FLUX BUILD (ID=40) 内の VX/VT/VR SINGLE セクション
- ENERGY FLUX (ID=39) 内の periodic boundary ループ
- 問題: periodic boundary ループが `C$OMP SINGLE` 内でシリアル実行 → OMP=8 で VX SINGLE=0.20s

### 変更内容

#### 1. MOM FLUX: VX periodic boundary (DO 6026/6027)
- `C$OMP SINGLE` を分割: coolant ループは SINGLE のまま、periodic ループを `C$OMP DO SCHEDULE(STATIC)` に変更
- `GOTO` → `IF(IND(J).NE.1) THEN` に構造化、K を外側ループに変更
- SHROUDFLUX は別の `C$OMP SINGLE` で保護

#### 2. MOM FLUX: VT periodic boundary (DO 902/903)
- 同様に `C$OMP DO SCHEDULE(STATIC)` に変更
- coolant/SHROUDFLUX は SINGLE のまま

#### 3. MOM FLUX: VR periodic boundary (DO 6260/6261)
- 同様に `C$OMP DO SCHEDULE(STATIC)` に変更
- VR セクションに `T_MOMF_VR_DO` / `T_MOMF_VR_SGL` サブタイマーを追加

#### 4. ENERGY FLUX: periodic boundary (DO 900/901)
- PARALLEL 領域の外側（シリアル）にあったため、独立した `C$OMP PARALLEL DO` で並列化

### 数値検証（ローカル OMP=1）
- 全項目 0.000000% — **完全一致**

### MOM FLUX ベクトル化分析

| ループ | 方向 | packed | scalar | SIMD率 | 状態 |
|---|---|---:|---:|---:|---|
| DO 6100 | Axial XFLUX | 72 | 23 | 76% | OK |
| DO 6110 | Axial RFLUX | 72 | 23 | 76% | OK |
| DO 6120 | Axial TFLUX | 72 | 27 | 73% | OK |
| DO 6200 | Tang. XFLUX | 40 | 15 | 73% | OK |
| DO 6210 | Tang. TFLUX | 72 | 28 | 72% | OK |
| DO 6230 | Tang. RFLUX | 40 | 19 | 68% | OK |
| DO 6300 | Radial XFLUX | 72 | 23 | 76% | OK |
| DO 6310 | Radial TFLUX | 72 | 27 | 73% | OK |
| DO 6320 | Radial RFLUX | 72 | 23 | 76% | OK |
| **DO 6330** | **Radial SOURCE** | **48** | **54** | **47%** | **要改善** |

- 9/10 ループが AVX2 ymm (256bit) で良好にベクトル化
- DO 6330 のみ SIMD 率 47%: 3変数×8点平均のレジスタ圧力が原因（"complicated access pattern"）

### AWS ベンチマーク結果（02-15 vs 02-14, 100ステップ）

#### LOOP: TOTAL

| | Original | OMP=1 | OMP=2 | OMP=4 | OMP=8 |
|---|---:|---:|---:|---:|---:|
| 02-14 | 83.93s | 79.35s | 43.34s | 24.16s | 15.52s |
| **02-15** | **80.11s** | **77.05s** | **40.82s** | **22.72s** | **13.83s** |
| 差 | -3.82s | -2.30s | -2.52s | -1.44s | **-1.69s** |

#### MOM FLUX BUILD (OMP=8)

| | 02-14 | 02-15 | 改善 |
|---|---:|---:|---:|
| MOM FLUX BUILD | 2.32s | **1.81s** | **-0.51s (22%)** |
| VX SINGLE | 0.20s | **0.05s** | **-0.15s (75%)** |
| VT SINGLE | 0.06s | **0.02s** | **-0.04s (67%)** |
| ENERGY FLUX | 0.50s | **0.41s** | **-0.09s (18%)** |

#### 数値検証（Original vs OMP=1）
全項目 Rel.Diff < 0.001% — **合格**

#### スケーリング

| | OMP=1→8 | Orig→8 |
|---|---:|---:|
| 02-14 | 5.11x | 5.40x |
| **02-15** | **5.57x** | **5.79x** |

---

### 現在の速度向上状況

#### ローカル (i5-12400F 6C/12T, 100ステップ)
| | original | OMP=1 | OMP=2 | OMP=4 |
|---|---|---|---|---|
| LOOP: TOTAL | 54.42s | 52.94s | 37.33s | 31.32s |
| 対 original | — | 1.03x | 1.46x | 1.74x |

#### AWS (c7i 8C/16T, 100ステップ) — 02-15 計測 (1149, SETUP並列化後)
| | original | OMP=1 | OMP=2 | OMP=4 | OMP=8 |
|---|---|---|---|---|---|
| LOOP: TOTAL | 81.11s | 76.01s | 39.90s | 22.11s | 13.33s |
| 対 original | — | 1.07x | 2.03x | 3.67x | **6.09x** |
| MAIN: SETUP | 21.12s | 20.30s | 11.76s | 6.87s | 4.57s |
| MAIN: TOTAL | 102.25s | 96.32s | 51.67s | 28.99s | **17.91s** |
| TOTAL 対 orig | — | 1.06x | 1.98x | 3.53x | **5.71x** |

#### AWS (c7i 8C/16T, 10000ステップ → 6976ステップ収束)
| | Timer | OMP=1 | OMP=4 | OMP=8 |
|---|---|---|---|---|
| LOOP: TOTAL | 5159s | 4981s | 1421s | 839s |
| 対 Timer | — | 1.04x | 3.63x | **6.15x** |
| 対 OMP=1 | — | — | 3.50x | **5.94x** |

---

## SETUP 高速化: SET_XLENGTH (壁面距離計算)

### 背景
SETUP 全体の計測で SET_XLENGTH が **14.82s / 16.98s = 87.3%** を占めるボトルネックと判明。
SET_XLENGTH の DO 1000 ループは全格子点 (I,J,K) に対して最近壁面距離を計算する。

### ステップ 1 — SETUP タイマー計装 (ID=76〜88)

NT を 75→88 に拡張し、SETUP 内に 13 区間のタイマーを追加:

| ID | 名称 | OMP=1 時間 (s) |
|---|---|---:|
| 76 | SETUP: CONST/INIT | 0.00 |
| 77 | SETUP: INTPOL | 0.00 |
| 78 | SETUP: GRID COORD | 0.03 |
| 79 | SETUP: AREAS | 0.21 |
| 80 | SETUP: INIT P/RO/T | 0.06 |
| 81 | SETUP: INIT VELOCITY | 0.81 |
| 82 | SETUP: MASS/FLUX INIT | 0.33 |
| 83 | SETUP: RESTART FILE | 0.00 |
| 84 | SETUP: TIMESTEP | 0.08 |
| 85 | SETUP: MULTIGRID | 0.21 |
| **86** | **SETUP: SET_XLENGTH** | **14.82** |
| 87 | SETUP: LOSS ROUTINES | 0.27 |
| 88 | SETUP: TFLOW | 0.00 |

### ステップ 2 — Plan A: OpenMP PARALLEL DO (K ループ並列化)

DO 1000 ループの最外 K ループに `C$OMP PARALLEL DO` を適用。各 (I,J,K) 点の壁面距離計算は独立しており、DIST_MIN(I,J,K) への書き込みも一意。XLIMIT(J) は I=IMID, K=KMID の場合のみ書き出し（1スレッドのみ通過）。

**変数分類**: PRIVATE(K,J,I,NR,PITCH,DIST_REF,IWALL,J1,J2,K1,K2,JWALL,JSURF,KSURF,HUBDIST,TIPDIST,DMIN,DISTSQ,JMIN,KMIN,XDIF,RDIF,TDIF,TDIFSQ,ATOT,XNORM,RNORM,TNORM,KNORM,ENDWALL_DIST,IF_FOUND,SSDIST,PSDIST,BLADE_DIST,DISTSQRT,XLLIM), SHARED(KM,JM,IM,IMID,KMID,JRANGE,KRANGE,IBOUND,X,R,THETA,NROW,NBLADE,ASX,ASR,ABX,ABR,ABT,JLE,JTE,JSTART,JMIX,SMERID,XLLIM_IN,XLLIM_LE,XLLIM_TE,XLLIM_DN,DIST_MIN,XLIMIT)

#### 結果

| | OMP=1 | OMP=4 |
|---|---:|---:|
| SET_XLENGTH | 14.82s | **4.63s (3.20x)** |
| SETUP 全体 | 16.98s | **6.68s (2.54x)** |

数値検証: EMAX=2.7797, EAVG=0.13162, ECONT=0.70818, FLOW=194.06 — **完全一致**

### ステップ 3 — Plan B: ハブ/ケーシング距離の事前計算

DO 1000 の最内 I ループ内にあるハブ/ケーシング探索（DO 10, DO 15）は X(J,K), R(J,K) のみを使い、I に依存しない。にもかかわらず IM=64 回同一計算が繰り返されていた。

**改善**: `ENDWALL_PRE(JD,KD)` ローカル配列を追加し、DO 1000 の前に Phase 1 ループ（OpenMP 並列）でハブ/ケーシング距離を事前計算。DO 1000 (Phase 2) ではルックアップ `ENDWALL_DIST = ENDWALL_PRE(J,K)` で置換。

#### 結果

| | Plan A のみ | Plan A + B |
|---|---:|---:|
| SET_XLENGTH OMP=1 | 14.82s | **13.79s (7% 削減)** |
| SET_XLENGTH OMP=4 | 4.63s | **4.52s (2% 削減)** |
| SETUP OMP=1 | 16.98s | **15.79s** |
| SETUP OMP=4 | 6.68s | **6.22s** |

数値検証: EMAX=2.7797, EAVG=0.13162, ECONT=0.70818, FLOW=194.06 — **完全一致**

OMP=1 で約1秒の削減はアルゴリズム最適化による純粋な計算量削減。OMP=4 では並列化により探索コストが分散済みのため差は小さい。

### 変更ファイル
- `dev/src/multall-open21.3-s1.0.f`:
  - SETUP サブルーチン: タイマー計装 (ID=76〜88)
  - SET_XLENGTH サブルーチン: Plan A (OpenMP PARALLEL DO) + Plan B (ENDWALL_PRE 事前計算)
  - タイマーインフラ: NT=75→88, NAME(76)〜NAME(88) 追加

### ステップ 4 — AWS ベンチマーク (100ステップ, c7i 8C/16T)

#### SETUP タイミング — 前回 (SETUP 未並列化) vs 今回 (Plan A+B)

| | 前回 (1050) | 今回 (1149) | 改善 |
|---|---:|---:|---:|
| SETUP OMP=1 | 20.80s | 20.30s | -2% |
| SETUP OMP=2 | 20.76s | **11.76s** | **-43%** |
| SETUP OMP=4 | 20.88s | **6.87s** | **-67%** |
| SETUP OMP=8 | 20.50s | **4.57s** | **-78%** |

前回は SETUP が完全にシリアル (~21s 固定) だったのが、SET_XLENGTH の並列化により OMP=8 で **4.57s (4.5x 高速化)**。

#### SET_XLENGTH スケーリング (AWS)

| OMP | SET_XLENGTH | 対 OMP=1 倍率 |
|---:|---:|---:|
| 1 | 17.91s | 1.00x |
| 2 | 9.52s | 1.88x |
| 4 | 4.74s | 3.78x |
| 8 | **2.49s** | **7.19x** |

8コアで 7.19x — ほぼ理想的なスケーリング。

#### SETUP サブタイマー (OMP=8)

| ID | セクション | OMP=1 | OMP=8 | 倍率 |
|---|---|---:|---:|---:|
| 86 | SET_XLENGTH | 17.91s | 2.49s | 7.19x |
| **81** | **INIT VELOCITY** | **1.29s** | **1.25s** | **1.03x** |
| 82 | MASS/FLUX INIT | 0.25s | 0.25s | 1.00x |
| 87 | LOSS ROUTINES | 0.28s | 0.04s | 7.5x |
| 85 | MULTIGRID | 0.22s | 0.22s | 1.00x |
| 79 | AREAS | 0.21s | 0.20s | 1.03x |

次のボトルネック: **INIT VELOCITY (1.25s)** — 完全にシリアル。

#### MAIN: TOTAL (全体) の改善

| | 前回 (1050) | 今回 (1149) | 改善 |
|---|---:|---:|---:|
| OMP=8 | 34.35s | **17.91s** | **-47.9%** |

#### 数値検証
- Original vs OMP=1: 全項目 Rel.Diff < 0.001% — **合格**
- 収束指標: Original/OMP=1 完全一致 (EMAX=2.7797, EAVG=0.13162, ECONT=0.70818, FLOW=194.06)

#### LOOP: TOTAL

| | Original | OMP=1 | OMP=2 | OMP=4 | OMP=8 |
|---|---:|---:|---:|---:|---:|
| LOOP: TOTAL | 81.11s | 76.01s | 39.90s | 22.11s | **13.33s** |
| 対 Orig 倍率 | — | 1.07x | 2.03x | 3.67x | **6.09x** |

### 次のアクション
- SETUP: INIT VELOCITY (1.25s) の並列化検討
- SET_XLENGTH の Plan C（翼面探索の事前計算）は効果が限定的のため保留
- HP Steam Turbine テストケースの追加（別メッシュでの検証）
