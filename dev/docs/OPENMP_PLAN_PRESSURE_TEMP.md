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

## 7) TABSEARCHのベクトル化作戦（PRESSURE/TEMPのみ）
対象: [../src/multall-open21.3-s1.0.f](../src/multall-open21.3-s1.0.f#L4265-L4320)

方針:
- **探索（インデックス決定）と補間（FINDIT5相当）を分離**する。
- 既存の `TABSEARCH` は他所でも使われるため **維持**し、
  新しい関数を追加して段階的に置き換える。
- 探索はスカラーのまま維持し、**補間だけを配列化してSIMD化**する。
- 3重ループのうち **`I` 方向をバッチ化**（固定サイズのバッファ）し、`J/K` ループ内で繰り返す。

手順（案）:
1) 新規関数を用意:
  - `TABSEARCH_IDX(RHOIN,UIN,IFOUND,JFOUND,DIFRO,DIFU)`
  - `TABINTERP5(IFOUND,JFOUND,DIFRO,DIFU,P,T,ENT,GA,DRY)`
2) `I` 方向を小さなバッチ（例: 16 or 32）に分割。
3) バッチ内で `RONOW/UNOW` を配列に溜め、`TABSEARCH_IDX` で `IFOUND/JFOUND` と `DIFRO/DIFU` を配列に格納。
4) `TABINTERP5` の式
   - `V = V00 + DX*HDI + DY*HDJ`
   を **配列ループ化**し、`!$OMP SIMD`（または `DO CONCURRENT`）でベクトル化。
5) 補間結果を既存配列（`P/T/ENT/GA/DRY`）に書き戻し。

関数インターフェース案（案）:
```fortran
  SUBROUTINE TABSEARCH_IDX(RHOIN,UIN,IFOUND,JFOUND,DIFRO,DIFU)
  REAL RHOIN,UIN,DIFRO,DIFU
  INTEGER IFOUND,JFOUND
```

```fortran
  SUBROUTINE TABINTERP5(IFOUND,JFOUND,DIFRO,DIFU,
     &                      PNOW,TNOW,ENTNOW,GA_PV_NOW,DRYNOW)
  REAL DIFRO,DIFU
  INTEGER IFOUND,JFOUND
```

バッチ補間の擬似コード（I方向バッチ）:
```fortran
  DO K=1,KM
  DO J=1,JM
  DO IB=1,IM,IBATCH
    N = MIN(IBATCH, IM-IB+1)
    DO II=1,N
      I = IB + II - 1
      RONOW_A(II) = RO(I,J,K)
      UNOW_A(II)  = ROE(I,J,K)/RONOW_A(II) - EKE(I,J,K)
      CALL TABSEARCH_IDX(RONOW_A(II),UNOW_A(II),
     &                               IFOUND_A(II),JFOUND_A(II),
     &                               DIFRO_A(II),DIFU_A(II))
    END DO
    !$OMP SIMD
    DO II=1,N
      CALL TABINTERP5(IFOUND_A(II),JFOUND_A(II),
     &                             DIFRO_A(II),DIFU_A(II),
     &                             PNOW_A(II),TNOW_A(II),ENT_A(II),
     &                             GA_A(II),DRY_A(II))
    END DO
    DO II=1,N
      I = IB + II - 1
      P(I,J,K)        = PNOW_A(II)
      T_STATIC(I,J,K) = TNOW_A(II)
      ENTPY(I,J,K)    = ENT_A(II)
      GA_PV(I,J,K)    = GA_A(II)
      WET(I,J,K)      = 1.0 - DRY_A(II)
    END DO
  END DO
  END DO
  END DO
```

期待効果:
- `TABSEARCH` の探索は分岐が多くSIMDに不向きだが、
  **補間は分岐が少ないためSIMDの恩恵を受けやすい**。
- 現行の「補間係数事前テーブル化」と相性が良い。

注意点:
- バッチサイズは **L1キャッシュに収まる**範囲で調整。
- `TABSEARCH` はスカラーのため、バッチを大きくし過ぎると
  探索部が律速になる可能性がある。

## 8) 2026-02-09 試行結果と撤回
このチャットで実施したベクトル化の試行は、最終的に速度改善が
見込めないと判断して撤回した。

実施した試行:
- `TABSEARCH` を `TABSEARCH_IDX` と `TABINTERP5` に分割し、
  `I` 方向バッチで `TABINTERP5_BATCH` を追加。
- `TABINTERP5_BATCH` に `!$OMP SIMD` を付けて補間のみ SIMD 化を狙った。
- 探索/補間の内訳を測るため、TABSEARCH/TABINTERP の計測を追加。

あまりうまくいかなかった点:
- `TABINTERP5_BATCH` は間接参照 (IFOUND/JFOUND 経由) のため、
  gfortran のベクトル化レポートでは「not vectorized」となった。
- 1スレッド時の内訳は探索側が支配的で、補間がボトルネックではなかった。
- 2スレッドでも TABSEARCH の短縮が小さく、OpenMP スケールが弱かった。
- 既存の PRESSURE/TEMP 全体でも、改善は限定的だった。

結論:
- ベクトル化/バッチ化の効果が限定的だったため、
  本試行のコード変更はコミット前に元へ戻した。
