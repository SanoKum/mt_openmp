# OpenMP並列化方針（ID 9: LOOP: PRESSURE/TEMP）

対象: [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L4241-L4330)（現行は [../src/multall-open21.3-s1.0.f](../src/multall-open21.3-s1.0.f#L4241-L4330)）

## 1) 並列化の基本方針
- 3重ループ（`K/J/I`）を OpenMP で並列化（`collapse(3)` + `schedule(static)`）。
- `RONOW, EKE, TSTATIC, ESTAT, UNOW, PO_RO, HNOW, PNOW, TNOW, ENTPY_NOW, GA_PV_NOW, DRYNOW` は **private**。
- `RO, ROE, VR, VT, VX, P, HO, T_STATIC, RGAS_NOW, GA_PV, PHEXP, WET, H_TO_T, ENTPY` など配列は **shared**。
- `IFGAS`/`ITIMST` による分岐はループ外で残し、**同じループを条件分岐で再利用しない**（分岐によるベクトル化阻害を回避）。

## 2) IFGAS=3（TABSEARCH）の注意点
- `TABSEARCH` が **スレッドセーフ** か要確認（COMMON の作業領域や `SAVE` 変数がある場合は競合）。
- 競合がある場合は、
  - 作業領域を `THREADPRIVATE` にする、または
  - 作業配列を引数で渡して **ローカル化** する。
- 共有テーブル参照のみで副作用がなければ、通常は並列化可能。

## 3) 想定される挿入位置（例）
- `IF(ITIMST.LT.5)` の `K/J/I` ループ
- `IF(ITIMST.EQ.5.OR.ITIMST.EQ.6)` の `K/J/I` ループ

## 4) 性能見積り（粗い推定）
- 本区間は `LOOP` 全体の約 19%（11.592 / 59.991）を占める。
- `8` スレッドで効率 60–80% と仮定すると、**本区間は 3–5 倍** 程度が目安。
- Amdahl の式で全体速度を評価すると、
  - $S_{overall} \approx 1 / ((1-0.19) + 0.19/4) \approx 1.17$（本区間 4倍）
  - $S_{overall} \approx 1 / ((1-0.19) + 0.19/5) \approx 1.20$（本区間 5倍）
- **全体では 15–20% 程度の短縮**が現実的（他区間も並列化すればさらに改善）。

## 5) 実施した修正（2026-02-09）
- `FINDIT5` の補間係数（`HALF*DFDI`/`HALF*DFDJ` 相当）を事前テーブル化。
- `READ_TABLE` で `P/T/ENT/GA/DRY` の係数テーブルを作成し、`TABSEARCH -> FINDIT5` で参照。
- テーブル範囲のクランプ（`ROMINN/ROMAXX/UMINN/UMAXX`）は既存処理を維持。
- `TABSEARCH` の探索高速化（補間探索/近傍探索/二分探索）は効果が小さく、**現行は元の探索手順に戻した**。

関連箇所:
- [../src/commall-open-21.3](../src/commall-open-21.3)
- [../src/multall-open21.3-s1.0.f](../src/multall-open21.3-s1.0.f)

## 6) 計測結果（OMP_NUM_THREADS=1）
比較対象: [../test_cases/stage.log.pre_findit5](../test_cases/stage.log.pre_findit5)

- `LOOP: PRESSURE/TEMP`: 9.211 s -> 7.864 s（約 1.17x、-14.6%）
- `LOOP: TOTAL`: 57.433 s -> 55.059 s（約 1.04x、-4.1%）
- `MAIN: TOTAL WALL TIME`: 73.925 s -> 71.100 s（約 1.04x、-3.8%）

最新ログ: [../test_cases/stage.log](../test_cases/stage.log)
