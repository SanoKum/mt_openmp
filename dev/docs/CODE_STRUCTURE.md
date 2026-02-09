# MULTALL-OPEN-21.3 コード構造解析（行番号付き・再分析）

対象ソース: [../src/multall-open-21.3.f](../src/multall-open-21.3.f)

> 行番号は 21.3 版の現行ソースに基づきます。改変で変動します。

---

## 📋 プログラム概要

**MULTALL-OPEN-21.3** は、多段ターボ機械の 3 次元定常流れを、非定常の連続・運動量・エネルギー方程式を時間積分して定常化する Fortran77 プログラムです。計算制御・格子・物性・境界条件は `commall-open-21.3` の巨大 COMMON に格納されます。

---

## 🧭 実行フロー（行番号）

- MAIN PROGRAM: [L1-L152](../src/multall-open-21.3.f#L1-L152)
    - 入力形式を `intype` で判定 → `NEW_READIN` / `OLD_READIN` を選択
    - `SETUP` → `LOOP` の順に実行

- NEW_READIN: [L153-L2256](../src/multall-open-21.3.f#L153-L2256)
- OLD_READIN: [L2257-L3761](../src/multall-open-21.3.f#L2257-L3761)
- LOOP: [L3762-L6343](../src/multall-open-21.3.f#L3762-L6343)
    - 初期化（冷却/ブリード設定、タイミング開始）[src/multall-open-21.3.f](../src/multall-open-21.3.f#L3820-L3842)
    - 逆設計の入力読み込みと前処理（inverse.in）[src/multall-open-21.3.f](../src/multall-open-21.3.f#L3846-L4030)
    - 反復ループ開始・収束/停止判定 [src/multall-open-21.3.f](../src/multall-open-21.3.f#L5000-L5060)
    - スムージング/ダンピング係数の更新 [src/multall-open-21.3.f](../src/multall-open-21.3.f#L5200-L5555)
    - 速度場の算出（`ROVX/ROVR/RORVT` → `VX/VR/VT`）[src/multall-open-21.3.f](../src/multall-open-21.3.f#L7440-L7485)
    - 圧力・温度の更新（理想気体/温度依存/テーブル）[src/multall-open-21.3.f](../src/multall-open-21.3.f#L5860-L5955)
    - 粘性/乱流モデルの更新（`LOSS`/`NEW_LOSS`/`SPAL_LOSS`）[src/multall-open-21.3.f](../src/multall-open-21.3.f#L5970-L6035)
    - 質量流束の計算（`FLOWX`/`FLOWT`/`FLOWR`）[src/multall-open-21.3.f](../src/multall-open-21.3.f#L5000-L5065)
    - 逆設計の圧力強制・形状更新（`TFLUX` と `GEOM_MOD`）[src/multall-open-21.3.f](../src/multall-open-21.3.f#L4454-L4770)
    - 境界/漏れ/冷却のフラックス調整 [src/multall-open-21.3.f](../src/multall-open-21.3.f#L4780-L5560)
    - `TSTEP` による保存量更新（密度/運動量/エネルギー）[src/multall-open-21.3.f](../src/multall-open-21.3.f#L5565-L5635)
    - SAモデル時の乱流粘性更新（追加 `TSTEP`）[src/multall-open-21.3.f](../src/multall-open-21.3.f#L5985-L6045)
- TSTEP: [L6344-L6809](../src/multall-open-21.3.f#L6344-L6809)
    - マルチグリッド変化量の初期化 [src/multall-open-21.3.f](../src/multall-open-21.3.f#L6344-L6375)
    - チップギャップのフラックス整合 [src/multall-open-21.3.f](../src/multall-open-21.3.f#L6376-L6395)
    - ミキシングプレーン上流面のフラックス外挿（FEXTRAP）[src/multall-open-21.3.f](../src/multall-open-21.3.f#L6398-L6475)
    - 残差（`DELTA`）計算と `STORE/DIFF` 更新 [src/multall-open-21.3.f](../src/multall-open-21.3.f#L6478-L6515)
    - ミキシングプレーンでのピッチ方向平均化（変更量）[src/multall-open-21.3.f](../src/multall-open-21.3.f#L6518-L6565)
    - マルチグリッド集約（`B1CHG/B2CHG/SBCHG`）[src/multall-open-21.3.f](../src/multall-open-21.3.f#L6568-L6608)
    - ブロック補正・時間刻み反映（`STEP/STEP1/STEP2`）[src/multall-open-21.3.f](../src/multall-open-21.3.f#L6610-L6638)
    - 残差スムージング（`SMOOTH_RESID`）と負帰還ダンピング [src/multall-open-21.3.f](../src/multall-open-21.3.f#L6640-L6725)
    - SAモデル時の特別処理（粘性更新・平滑化）[src/multall-open-21.3.f](../src/multall-open-21.3.f#L6728-L6765)
    - 変更量の分配更新（セル→格子点）[src/multall-open-21.3.f](../src/multall-open-21.3.f#L6768-L6798)
    - `SMOOTH_VAR` による変数平滑化 [src/multall-open-21.3.f](../src/multall-open-21.3.f#L6768-L6785)
    - 周期境界・ミキシングプレーンの最終平均化 [src/multall-open-21.3.f](../src/multall-open-21.3.f#L6786-L6809)
- OUTPUT: [L6810-L7155](../src/multall-open-21.3.f#L6810-L7155)
- PRINT: [L7156-L7224](../src/multall-open-21.3.f#L7156-L7224)
- SETUP: [L7225-L8579](../src/multall-open-21.3.f#L7225-L8579)

---

## 📂 入出力ファイル（主要）

- 入力
    - `intype`: 新旧入力形式の指定（N/O） [L25-L52](../src/multall-open-21.3.f#L25-L52)
    - 物性テーブル: `props_table.dat`（IFGAS=3 時）[L18859-L19064](../src/multall-open-21.3.f#L18859-L19064)

- 出力
    - `stage.log` [L17](../src/multall-open-21.3.f#L17)
    - `flow_out`（unformatted）[L18](../src/multall-open-21.3.f#L18)
    - `global.plt`（unformatted）[L19](../src/multall-open-21.3.f#L19)
    - `results.out` [L20](../src/multall-open-21.3.f#L20)
    - `stopit`（停止フラグ）[L21-L24](../src/multall-open-21.3.f#L21-L24)
    - `grid_out`（unformatted）[L22](../src/multall-open-21.3.f#L22)

---

## ⚙️ 主要サブルーチン一覧（行番号）

| サブルーチン | 行範囲 |
|------------|--------|
| NEW_INTPOL | [L8580-L8700](../src/multall-open-21.3.f#L8580-L8700) |
| OLD_INTPOL | [L8701-L8887](../src/multall-open-21.3.f#L8701-L8887) |
| INTP | [L8888-L8926](../src/multall-open-21.3.f#L8888-L8926) |
| LININT | [L8927-L8959](../src/multall-open-21.3.f#L8927-L8959) |
| SUMFLX | [L8960-L9012](../src/multall-open-21.3.f#L8960-L9012) |
| STEPUP | [L9013-L9103](../src/multall-open-21.3.f#L9013-L9103) |
| SETFLO | [L9104-L9155](../src/multall-open-21.3.f#L9104-L9155) |
| LOSS | [L9156-L10192](../src/multall-open-21.3.f#L9156-L10192) |
| INPINT | [L10193-L10345](../src/multall-open-21.3.f#L10193-L10345) |
| GRID_DOWN | [L10346-L10577](../src/multall-open-21.3.f#L10346-L10577) |
| GRID_UP | [L10578-L10657](../src/multall-open-21.3.f#L10578-L10657) |
| SOLVE | [L10658-L10692](../src/multall-open-21.3.f#L10658-L10692) |
| NEWGRID | [L10693-L11058](../src/multall-open-21.3.f#L10693-L11058) |
| SHROUDFLOW | [L11059-L11387](../src/multall-open-21.3.f#L11059-L11387) |
| SHROUDFLUX | [L11388-L11416](../src/multall-open-21.3.f#L11388-L11416) |
| COOLIN_1 | [L11417-L11747](../src/multall-open-21.3.f#L11417-L11747) |
| BLEEDOUT | [L11748-L11881](../src/multall-open-21.3.f#L11748-L11881) |
| EFICOOL | [L11882-L13027](../src/multall-open-21.3.f#L11882-L13027) |
| SMOOTH | [L13028-L13052](../src/multall-open-21.3.f#L13028-L13052) |
| SET_COEFFS | [L13053-L13197](../src/multall-open-21.3.f#L13053-L13197) |
| MASS_AVG | [L13198-L13234](../src/multall-open-21.3.f#L13198-L13234) |
| RESTAGG | [L13235-L13330](../src/multall-open-21.3.f#L13235-L13330) |
| LEAN | [L13331-L13374](../src/multall-open-21.3.f#L13331-L13374) |
| MIX_BCONDS | [L13375-L13553](../src/multall-open-21.3.f#L13375-L13553) |
| NEWBCONDS | [L13554-L13586](../src/multall-open-21.3.f#L13554-L13586) |
| CELL_TO_NODE | [L13587-L13662](../src/multall-open-21.3.f#L13587-L13662) |
| SET_XLENGTH | [L13663-L13990](../src/multall-open-21.3.f#L13663-L13990) |
| NEW_LOSS | [L13991-L14936](../src/multall-open-21.3.f#L13991-L14936) |
| GRADVEL | [L14937-L14984](../src/multall-open-21.3.f#L14937-L14984) |
| GRADCELL | [L14985-L15054](../src/multall-open-21.3.f#L14985-L15054) |
| SPAL_LOSS | [L15055-L16307](../src/multall-open-21.3.f#L15055-L16307) |
| SMOOTH_RESID | [L16308-L16396](../src/multall-open-21.3.f#L16308-L16396) |
| NEW_MIXPLAN | [L16397-L16766](../src/multall-open-21.3.f#L16397-L16766) |
| INSECT | [L16767-L16861](../src/multall-open-21.3.f#L16767-L16861) |
| SMOOTH_VAR | [L16862-L17109](../src/multall-open-21.3.f#L16862-L17109) |
| RE_DESIGN | [L17110-L17331](../src/multall-open-21.3.f#L17110-L17331) |
| SET_SSTHICK | [L17332-L17427](../src/multall-open-21.3.f#L17332-L17427) |
| RESTAGGER | [L17428-L17515](../src/multall-open-21.3.f#L17428-L17515) |
| WALLFUN | [L17516-L17587](../src/multall-open-21.3.f#L17516-L17587) |
| SET_PWALLGRAD | [L17588-L17654](../src/multall-open-21.3.f#L17588-L17654) |
| COOL_INPUT | [L17655-L17845](../src/multall-open-21.3.f#L17655-L17845) |
| COOLIN_2 | [L17846-L18223](../src/multall-open-21.3.f#L17846-L18223) |
| GEOM_MOD | [L18224-L18858](../src/multall-open-21.3.f#L18224-L18858) |
| READ_TABLE | [L18859-L19064](../src/multall-open-21.3.f#L18859-L19064) |
| CHECK_TAB | [L19065-L19089](../src/multall-open-21.3.f#L19065-L19089) |
| TABSEARCH | [L19090-L19208](../src/multall-open-21.3.f#L19090-L19208) |
| FINDIT | [L19209-L19228](../src/multall-open-21.3.f#L19209-L19228) |

---

## 🔧 主要処理の再分析（要点）

### MAIN PROGRAM
- `intype` を読み込み、`NEW_READIN`/`OLD_READIN` を切替 [L25-L61](../src/multall-open-21.3.f#L25-L61)
- その後 `SETUP` → `LOOP` を順に実行 [L83-L89](../src/multall-open-21.3.f#L83-L89)

### NEW_READIN / OLD_READIN
- 計算条件、格子、境界条件、モデル設定などを読み込み
- 物性設定：
    - 理想気体（IFGAS=0）
    - 温度依存比熱（IFGAS=1）
    - 物性テーブル（IFGAS=3）→ `READ_TABLE` [L18859-L19064](../src/multall-open-21.3.f#L18859-L19064)
- 時間積分方式（ITIMST=3/4/5/6）と係数を設定

### SETUP
- 格子・ブロック・補間の準備、初期流れ場構築
- マルチグリッドレベルの設定

### LOOP（メイン反復）
- 収束判定を行いながら時間ステップを反復
- 速度場、圧力、温度、粘性/乱流の更新
- 流束計算と `TSTEP` の複数回呼び出し

### TSTEP（保存量の更新）
- 流束差分・残差計算
- マルチグリッド集約と補正
- 残差スムージング、ダンピング、変数更新

### OUTPUT / PRINT
- 指定ステップで結果出力
- `flow_out` / `grid_out` / `global.plt` / `results.out` の生成

---

## 🧪 物理モデル・オプション（要点）

- 乱流/粘性モデル
    - `LOSS`（薄いせん断層モデル）
    - `NEW_LOSS`（改良混合距離モデル）
    - `SPAL_LOSS`（Spalart–Allmaras）

- ミキシングプレーン
    - `NEW_MIXPLAN` で周方向平均化と接続

- 物性
    - `READ_TABLE` によるテーブル参照（`props_table.dat`）

---

## 🔁 逆設計（Inverse Design）の処理概要

- 逆設計モードの入力を `inverse.in` から読み込み、指定圧力分布や制御係数、更新ステップ範囲を設定 [src/multall-open-21.3.f](../src/multall-open-21.3.f#L3846-L4030)
- 指定圧力分布と厚み補正を用意し、基準量（`P_AVGG` など）を準備 [src/multall-open-21.3.f](../src/multall-open-21.3.f#L3870-L4030)
- NINV_START / NINV_END で翼面圧力分布をファイル出力（initial_inverse.in / final_inverse.in）[src/multall-open-21.3.f](../src/multall-open-21.3.f#L4454-L4520)
- 逆設計区間では、指定圧力との差から面上ソース（`TFLUX`）を計算し、翼面の質量フラックスを調整 [src/multall-open-21.3.f](../src/multall-open-21.3.f#L4520-L4705)
- 指定ステップごと（NINV_UPP）に `GEOM_MOD` で翼形状を更新 [src/multall-open-21.3.f](../src/multall-open-21.3.f#L4760-L4770)
- 逆設計区間外では翼面フラックスをゼロに戻す [src/multall-open-21.3.f](../src/multall-open-21.3.f#L4784-L4810)

---

## 🔍 並列化のポイント（概略）

- LOOP: [L3762-L6343](../src/multall-open-21.3.f#L3762-L6343)
    - 速度・圧力・温度、流束計算は独立ループが中心
- TSTEP: [L6344-L6809](../src/multall-open-21.3.f#L6344-L6809)
    - マルチグリッド集約部は競合に注意
- SMOOTH_VAR: [L16862-L17109](../src/multall-open-21.3.f#L16862-L17109)
    - 方向別スイープで分割しやすい

---

## ⏱️ Wall time 計測（2026-02-08）

### 計測方法
- `SYSTEM_CLOCK` による wall time で計測
- `## 実行フロー` の主要処理ごとに `TIMER_START/STOP` を挿入
- `TSTEP` 内の細分処理も計測
- 計測結果は `stage.log` 末尾の `WALL TIME SUMMARY (s)` に出力

### 実行条件
- 実行場所: original/test_cases
- 入力: two-stg-LP-ST+steam.dat
- 最大ステップ: 100
- コンパイル: gfortran (`-O2 -std=legacy -ffixed-line-length-none -mcmodel=large`)

### 計測結果（wall time, s）

#### 大枠（1,2,3,4,41）

| ID | 処理 | 時間 (s) |
|---:|---|---:|
| 1 | MAIN: NEW_READIN | 0.011 |
| 2 | MAIN: OLD_READIN | 0.000 |
| 3 | MAIN: SETUP | 16.329 |
| 4 | LOOP: TOTAL | 43.009 |
| 41 | MAIN: TOTAL WALL TIME | 59.349 |

#### 細分化（5〜40,48〜52）

| ID | 処理 | 時間 (s) | 行範囲 |
|---:|---|---:|---|
| 9 | LOOP: PRESSURE/TEMP | 7.612 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L4241-L4330) |
| 29 | TSTEP: SMOOTH_VAR | 7.649 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L6888-L6890) |
| 26 | TSTEP: STEP/DAMP | 3.142 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L6674-L6766) |
| 40 | LOOP: MOMENTUM FLUX BUILD | 4.438 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L5760-L5849) / [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L5859-L5943) / [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L5952-L6056) |
| 19 | TSTEP: MOMENTUM-R | 3.750 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L6057-L6059) |
| 16 | TSTEP: ENERGY | 3.781 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L5244-L5246) |
| 17 | TSTEP: MOMENTUM-X | 3.745 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L5850-L5852) |
| 14 | TSTEP: DENSITY | 3.728 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L5016-L5023) |
| 18 | TSTEP: MOMENTUM-T | 3.771 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L5944-L5946) |
| 28 | TSTEP: CELL->NODE | 3.451 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L6865-L6882) |
| 10 | LOOP: VISCOUS/TURB MODEL | 3.015 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L4337-L4355) |
| 23 | TSTEP: DELTA/STORE | 2.654 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L6580-L6593) |
| 11 | LOOP: MASS FLUX | 1.687 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L4366-L4412) |
| 25 | TSTEP: MG AGG | 1.601 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L6639-L6666) |
| 39 | LOOP: ENERGY FLUX/SOURCE | 1.096 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L5132-L5243) |
| 8 | LOOP: VELOCITY UPDATE | 0.680 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L4193-L4205) |
| 33 | LOOP: MASS FLUX RFLUX | 0.490 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L4889-L4908) |
| 30 | TSTEP: FINAL AVG | 0.198 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L6900-L6971) |
| 48 | LOOP: VRMS/VMAX (E5) | 0.084 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L4219-L4246) |
| 22 | TSTEP: FEXTRAP | 0.021 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L6533-L6575) |
| 51 | LOOP: NO-INV SURF ZERO | 0.084 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L4850-L4867) |
| 31 | TSTEP: PITCH AVG POST-MG | 0.012 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L6820-L6855) |
| 24 | TSTEP: PITCH AVG | 0.011 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L6601-L6633) |
| 13 | LOOP: BOUNDARY/LEAK/COOL | 0.012 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L5256-L5687) |
| 20 | TSTEP: MG INIT | 0.034 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L6498-L6513) |
| 35 | LOOP: ZERO MASS HUB/CASING | 0.002 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L4934-L4943) |
| 21 | TSTEP: TIP GAP | 0.000 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L6517-L6527) |
| 5 | LOOP: COOL/BLEED INIT | 0.000 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L3874-L3891) |
| 6 | LOOP: INVERSE INPUT | 0.000 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L3905-L4089) |
| 7 | LOOP: SMOOTH/DAMP | 0.000 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L4151-L4186) |
| 12 | LOOP: INVERSE FORCE/GEOM | 0.000 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L4593-L4844) |
| 15 | TSTEP: SA VISC | 0.000 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L5106-L5108) |
| 27 | TSTEP: SA SPECIAL | 0.000 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L6769-L6806) |
| 32 | LOOP: SA FLUX SETUP | 0.000 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L5049-L5105) / [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L5109-L5125) |
| 34 | LOOP: Q3D BODY FORCE | 0.000 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L4913-L4928) |
| 36 | LOOP: BLEED HUB/CASING | 0.000 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L4951-L4962) |
| 37 | LOOP: COOLANT MASS FLUX | 0.000 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L4968-L4979) / [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L4988-L4999) |
| 38 | LOOP: SHROUD MASS FLUX | 0.000 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L5006-L5009) |
| 49 | LOOP: TFLOW ADDITION | 0.001 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L4413-L4520) |
| 50 | LOOP: INV START/END OUT | 0.000 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L4525-L4592) |
| 52 | LOOP: BLEED SURF FLUX | 0.000 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L4874-L4886) |
| 53 | LOOP: CELL AVG DENSITY | 0.000 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L5049-L5059) |

#### SMOOTH_VAR 内訳（42,44,45,46,47）

| ID | 処理 | 時間 (s) | 行範囲 |
|---:|---|---:|---|
| 46 | SMOOTH_VAR: PITCH/SPANWISE | 5.278 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L17157-L17242) |
| 42 | SMOOTH_VAR: STREAMWISE CORE | 1.406 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L17057-L17100) |
| 45 | SMOOTH_VAR: RESET D | 0.931 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L17132-L17142) |
| 47 | SMOOTH_VAR: EXIT FLOW | 0.025 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L17256-L17288) |
| 44 | SMOOTH_VAR: LEADING EDGE | 0.005 | [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L17109-L17128) |

### 所見（ボトルネック候補）
- `TSTEP: SMOOTH_VAR` と `TSTEP: STEP/DAMP` が支配的
- 次点で `LOOP: PRESSURE/TEMP` と各 `TSTEP: MOMENTUM-*` が重い
- 今後の OpenMP は `SMOOTH_VAR`/`TSTEP` 内ループから着手が効果的

---

## ✅ 変更履歴

- 旧 `CODE_STRUCTURE.md` を廃止し、本ファイルへ統合
- 行番号を維持しつつ、主要処理と入出力、モデル設定を再整理

---

## 追記: original 計測と比較 (2026-02-10)

参照ログ: [dev/test_cases/stage.log.original](../test_cases/stage.log.original), [dev/test_cases/stage.log.omp1](../test_cases/stage.log.omp1), [dev/test_cases/stage.log.omp2](../test_cases/stage.log.omp2)

| 項目                         | original (s) | OMP1 (s) | OMP2 (s) | OMP2-Orig (s) | OMP2/Orig |
|:----------------------------|------------:|---------:|---------:|-------------:|----------:|
| MAIN: TOTAL WALL TIME       |      59.349 |   63.450 |   58.802 |       -0.547 |     1.009 |
| TSTEP: SMOOTH_VAR           |       7.649 |    8.507 |    7.447 |       -0.202 |     1.027 |
| SMOOTH_VAR: STREAMWISE CORE |       1.406 |    1.541 |    1.340 |       -0.066 |     1.049 |
| SMOOTH_VAR: PITCH/SPANWISE  |       5.278 |    5.929 |    5.215 |       -0.063 |     1.012 |
| LOOP: PRESSURE/TEMP         |       7.612 |    8.289 |    7.930 |       +0.318 |     0.960 |
| TSTEP: STEP/DAMP            |       3.142 |    3.662 |    3.199 |       +0.057 |     0.982 |
