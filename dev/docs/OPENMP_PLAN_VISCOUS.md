# OpenMP 並列化計画: VISCOUS/TURB MODEL (SUBROUTINE LOSS)

## 1. 現状分析

### 1.1 タイミング

| 条件 | VISCOUS/TURB | LOOP TOTAL | 割合 |
|------|-------------|------------|------|
| OMP=1 | 2.92s | 36.84s | 7.9% |
| OMP=4 | 2.93s | 25.75s | 11.4% |

- **完全シリアル**（スケーリング 1.0x）
- NLOS=5 なので 100 ステップ中 20 回呼ばれる（1回あたり ~0.15s）
- ILOS=10 → `SUBROUTINE LOSS`（mixing length model）が使用される
- **YPLUSWALL=0.0**（テストケース固有）→ WALLFUN/SET_PWALLGRAD は呼ばれない
- WALLFUN は引数経由の入出力のみで COMMON への書き込みなし → 呼ばれても PRIVATE 変数で受ければスレッドセーフ

### 1.2 サブルーチン構造（LOSS）

```
LOSS
├── [1] セットアップ（NSTEP<=NLOS のみ）: FILAM, FITURB, FKLAM, FKTURB 計算
│       → 初回のみ。並列化不要。
│
├── [2] DO 50 J=2,JM — ストリームワイズ面（K=const）の粘性応力
│   └── DO 40 I=1,IMM1
│       ├── DO 30 K=1,KM      — 局所配列に平均速度等を計算
│       ├── 壁面せん断応力計算（Hub: TWALLK1, Casing: TWALLKM）
│       ├── Y_PLUS 更新（Hub/Casing 側）
│       ├── DO 35 K=2,KMM1    — VLAM(K), VTURB(K) 計算
│       ├── 遷移チェック（Hub/Casing 別）
│       ├── DO 41 K=2,KMM1    — 応力計算、VISC_RAT(I,J,K) = VTURB/VLAM
│       ├── DO 45 K=1,KMM1    — XFORCE/RFORCE/TFORCE/QSOURCE 更新
│       └── DO 57 K=1,KMM1    — エントロピー生成率（IFPRINT/IFEND のみ）
│
├── [3] DO 100 J=2,JM — ブレードワイズ面（I=const）の粘性応力
│   └── DO 90 K=1,KMM1
│       ├── DO 60 I=1,IM      — 局所配列に平均速度等を計算
│       ├── 翼面せん断応力計算（Lower: TWALLI1, Upper: TWALLIM）
│       ├── Y_PLUS 更新（翼面側）
│       ├── DO 55 I=2,IMM1    — VLAM(I), VTURB(I) 計算
│       ├── 遷移チェック（Lower/Upper 別）
│       ├── DO 76 I=2,IMM1    — 応力計算、VISC_RAT += VTURB/VLAM
│       ├── チップギャップ/ウェイク/上流域の処理（GO TO 65/75）
│       ├── DO 80 I=1,IMM1    — XFORCE/RFORCE/TFORCE/QSOURCE 更新
│       └── DO 91 I=1,IMM1    — エントロピー生成率（IFPRINT/IFEND のみ）
│
└── [4] CELL_TO_NODE + VISC_RAT 境界ゼロ化（IFPRINT/IFEND のみ）
```

### 1.3 ループサイズ

- DO 50: J=2..JM（350 反復）× I=1..IMM1（63 反復）= ~22,050 (I,J) セル
- DO 100: J=2..JM（350 反復）× K=1..KMM1（63 反復）= ~22,050 (J,K) セル
- 各反復で独立した (I,J) or (J,K) の出力 → **Jループ並列化が最適**

## 2. 並列化の課題と対策

### 2.1 VLAM/VTURB が COMMON/BKLOSS/ 内にある問題

**問題**: `VLAM(MAXKI)` と `VTURB(MAXKI)` は COMMON ブロック内の共有配列。
各スレッドが作業配列として使用するため、並列化すると競合する。

**対策**: サブルーチン内にローカル配列 `VLAM_L(MAXKI)`, `VTURB_L(MAXKI)` を新設し、
COMMON の VLAM/VTURB の代わりに使用する。PRIVATE 指定で各スレッドが独立コピーを持つ。

```fortran
      DIMENSION VLAM_L(MAXKI), VTURB_L(MAXKI)
```

- VLAM/VTURB は LOSS 内でのみ書き込まれ、外部からの参照はない（確認済み）
- 配列サイズ: 82 要素 × 8 bytes × 2 = 1.3KB/スレッド → 問題なし

### 2.2 ローカル配列の PRIVATE 化

サブルーチン内で DIMENSION 宣言されている配列（16 個）:
```
VXAVG(MAXKI), VRAVG(MAXKI), WTAVG(MAXKI), WABS(MAXKI),
ROAVG(MAXKI), XSTRES(MAXKI), RAVG(MAXKI), RSTRES(MAXKI),
TSTRES(MAXKI), AREA(MAXKI), WBOUND(MAXKI), WTB(MAXKI),
VTAVG(MAXKI), TAVG(MAXKI), WVISC(MAXKI), QFLOW(MAXKI)
```

- 合計: 16 × 82 × 8 = ~10KB/スレッド → スタック許容範囲内
- すべて J 反復内で書き込み・使用完結 → PRIVATE で安全

### 2.3 VISCOSY/REYNOLDS/ROVEXIT の書き込み

DO 50 内で条件付き書き込み:
```fortran
      IF(I.EQ.IMID.AND.J.EQ.JTEDGE) VISCOSY(NRW) = VISLAM
      IF(I.EQ.IMID.AND.J.EQ.JTEDGE) REYNOLDS(NRW) = ...
```

- 各 NRW（ブレード列）に対して 1 つの J 値のみが書き込む
- 異なるブレード列の JTEDGE は異なるため、実質衝突なし
- **SHARED のまま安全**（異なるメモリアドレスへの書き込み）

### 2.4 VISC_RAT の書き込み順序

- DO 50: `VISC_RAT(I,J,K) = VTURB(K)/VLAM(K)` → **代入**
- DO 100: `VISC_RAT(I,J,K) = VISC_RAT(I,J,K) + VTURB(I)/VLAM(I)` → **加算**

→ **DO 50 と DO 100 は別々の PARALLEL DO にする必要がある**（BARRIER 必要）

### 2.5 WALLFUN / SET_PWALLGRAD の並列安全性

**方針**: YPLUSWALL の値に関わらず、WALLFUN が呼ばれても安全な並列化を行う。

- `WALLFUN(I,J,K,NWALL,PERP,DPDS,DENSITY,TWALL,YPLUS_OLD,WREL_WALL,YPLUS_NEW)`:
  - 入力: I,J,K,NWALL,PERP,DPDS,DENSITY,YPLUS_OLD,WREL_WALL（すべてスカラー）
  - 出力: TWALL, YPLUS_NEW（スカラー、呼び出し元のローカル変数へ返す）
  - COMMON からの読み取り: YPLUSWALL, VISLAM のみ（変更なし）
  - → **スレッドセーフ**。PRIVATE 変数で受ければ問題なし。

- `YPLUS_K1(I,J)`, `YPLUS_KM(I,J)`, `YPLUS_I1(J,K)`, `YPLUS_IM(J,K)`:
  - WALLFUN 呼び出し前に OLD として読み取り、呼び出し後に NEW で上書き
  - J ループ並列化では各スレッドが異なる J を処理するため衝突なし
  - → **SHARED のまま安全**

- `SET_PWALLGRAD` (YPLUSWALL < -10.0 の場合):
  - DO 50/100 の前に呼ばれる（並列領域の外）
  - DPDS_CELL を更新する → 並列領域では読み取りのみ
  - → **並列化への影響なし**

- WALLFUN 関連のスカラー変数を PRIVATE リストに含める:
  `PERPK1, PERPKM, PERPI1, PERPIM, YPLUS_OLD, YPLUS_NEW`

### 2.6 GO TO 文

すべて **反復内のフロー制御**（345, 355, 555, 365, 375, 51, 65, 75, 78）。
ループ境界を跨がないため並列化に支障なし。

### 2.6 スカラー変数の分類

**PRIVATE にすべきスカラー**（各スレッドで独立計算）:
```
NRW, J1, JROW, JTRHUB, JTRTIP, JLEDGE, JTEDGE, WREL,
TWALLK1, TWALLKM, TWALLI1, TWALLIM, FMULT, PERP, RE, RELOG,
CF, CFLAM, ROUGH, REKSQ, REK, A1, A2, A3, VSTAR,
YPLSK1, YPLSKM, YPLSI1, YPLSIM, YPLSP, VISTOT, RMAX, RATVIS,
DPERP, FYPLUS, XFAC, WABSQ, RF_VIS1, WREL_CELL,
FACJ, FBLEND, ISUB, IM1, IP1, KP1, JM1,
PERPK1, PERPKM, PERPI1, PERPIM, YPLUS_OLD, YPLUS_NEW,
PLUSK, RELOG10, FUNCN, CF_FULL_ROUGH, IMAX,
XGEN, RGEN, TGEN, TAVGG, QGEN, SGEN_NOW,
DELP_BLADE, TAU_REF, RHO_REF, VEL_REF, PITCH, SREF
```

**SHARED にすべき変数**（COMMON ブロック内、読み取り専用 or 出力配列）:
```
VX, VR, VT, WT, RO, T_STATIC, PEFF (読み取り)
ASX, ASR, ABX, ABR, ABT, AQX, AQR (面積、読み取り)
VOL, R, X, RAVG_CELL (格子、読み取り)
XFORCE, RFORCE, TFORCE, QSOURCE (出力、各Jで独立)
VISC_RAT, Y_PLUS, SGEN, TEMP4 (出力、各Jで独立)
VISCOSY, REYNOLDS, ROVEXIT (出力、条件付き)
DWALLSQ, DPDS_CELL (読み取り)
FILAM, FITURB, FIWAKE, FIUP, FKLAM, FKTURB (係数、読み取り)
FP, FR, NBLADE, NROW, JSTART, JLE, JTE, JMIX (格子パラメータ)
WHUB, WTIP, WRAD (回転速度)
VISLAM, TCOND, FTCOND, CFWALL, FMIXUP (粘性パラメータ)
IM, IMM1, KM, KMM1, JM, IMID, KMID (インデックス)
IBOUND, IFPRINT, IFEND, NSTEP (フラグ)
YPLUSWALL, YPLAM, YPTURB, RF_VIS (モデルパラメータ)
KTIPS, KTIPE, IND (チップギャップ)
```

## 3. 実装計画

### Phase 1: DO 50 の並列化（ストリームワイズ面）

```fortran
C     ローカル作業配列を宣言（COMMON の VLAM/VTURB の代替）
      DIMENSION VLAM_L(MAXKI), VTURB_L(MAXKI)
      ...
C$OMP PARALLEL DO DEFAULT(NONE)
C$OMP&PRIVATE(NRW,J1,JROW,JTRHUB,JTRTIP,JLEDGE,JTEDGE,WREL,
C$OMP&  TWALLK1,TWALLKM,FMULT,PERP,RE,RELOG,CF,CFLAM,ROUGH,
C$OMP&  REKSQ,REK,A1,A2,A3,VSTAR,DPERP,FYPLUS,XFAC,
C$OMP&  YPLSK1,YPLSKM,YPLSP,VISTOT,RMAX,RATVIS,WABSQ,
C$OMP&  PLUSK,RELOG10,FUNCN,CF_FULL_ROUGH,I,K,
C$OMP&  XGEN,RGEN,TGEN,TAVGG,QGEN,
C$OMP&  VXAVG,VRAVG,WTAVG,WABS,ROAVG,XSTRES,RAVG,RSTRES,
C$OMP&  TSTRES,AREA,WBOUND,WTB,VTAVG,TAVG,WVISC,QFLOW,
C$OMP&  VLAM_L,VTURB_L)
C$OMP&SHARED(... 読み取り専用配列・パラメータ ...)
      DO 50 J=2,JM
      ...（VLAM → VLAM_L, VTURB → VTURB_L に置換）
   50 CONTINUE
C$OMP END PARALLEL DO
```

### Phase 2: DO 100 の並列化（ブレードワイズ面）

DO 50 完了後（暗黙的 BARRIER）に実行。同様の PARALLEL DO を適用。

```fortran
C$OMP PARALLEL DO DEFAULT(NONE)
C$OMP&PRIVATE(NRW,J1,JROW,JTRLOW,JTRUP,JLEDGE,JTEDGE,WREL,
C$OMP&  TWALLI1,TWALLIM,FMULT,PERP,RE,RELOG,CF,CFLAM,ROUGH,
C$OMP&  REKSQ,REK,A1,A2,A3,VSTAR,DPERP,FYPLUS,XFAC,
C$OMP&  YPLSI1,YPLSIM,YPLSP,RMAX,RATVIS,WABSQ,IMAX,
C$OMP&  FACJ,FBLEND,ISUB,IM1,IP1,I,K,KP1,JM1,
C$OMP&  PLUSK,RELOG10,FUNCN,CF_FULL_ROUGH,
C$OMP&  XGEN,RGEN,TGEN,TAVGG,QGEN,SGEN_NOW,
C$OMP&  DELP_BLADE,TAU_REF,RHO_REF,VEL_REF,PITCH,SREF,
C$OMP&  VXAVG,VRAVG,WTAVG,WABS,ROAVG,XSTRES,RAVG,RSTRES,
C$OMP&  TSTRES,AREA,WBOUND,WTB,VTAVG,TAVG,WVISC,QFLOW,
C$OMP&  VLAM_L,VTURB_L)
C$OMP&SHARED(... 読み取り専用配列・パラメータ ...)
      DO 100 J=2,JM
      ...（VLAM → VLAM_L, VTURB → VTURB_L に置換）
  100 CONTINUE
C$OMP END PARALLEL DO
```

### Phase 3: IFPRINT/IFEND ブロック（末尾）

CELL_TO_NODE 呼び出しと VISC_RAT 境界ゼロ化は呼ばれる頻度が低い（出力時のみ）。
シリアルのまま維持する。

## 4. 期待される性能改善

| 条件 | 現在 | 目標 | 改善量 |
|------|------|------|--------|
| OMP=1 | 2.93s | 2.93s（変化なし） | 0s |
| OMP=2 | 2.93s | ~1.6-1.8s | ~1.1-1.3s |
| OMP=4 | 2.93s | ~0.9-1.2s | ~1.7-2.0s |

- J=350 反復でバランスが取れているため、良好なスケーリングが期待できる
- Fork/join オーバーヘッド: 2 回の PARALLEL DO × 20 回/100 ステップ = 40 回
  → ~0.5ms × 40 = ~0.02s、無視可能
- PRIVATE 配列のスタック割り当て: ~12KB/スレッド、問題なし

**LOOP TOTAL への影響予測**:
- OMP=4: 25.75s → ~24.0-24.5s（VISCOUS 2.93→0.9-1.2s）
- 生産環境（10000 ステップ）での VISCOUS 改善: ~29.3s → ~9-12s

## 5. リスク評価

| リスク | 影響度 | 対策 |
|--------|--------|------|
| VLAM/VTURB の COMMON 問題 | 高 | ローカル配列 VLAM_L/VTURB_L で回避 |
| スカラー変数の漏れ | 中 | DEFAULT(NONE) で強制チェック |
| GO TO 文の影響 | 低 | 反復内フロー制御のみ、問題なし |
| TEMPP(JD) 配列 | 低 | DO 100 では未使用。DO 50 にもない |
| WALLFUN 呼び出し | 低 | YPLUSWALL=0.0 では呼ばれない。呼ばれる場合も引数経由の入出力のみで COMMON 書き込みなし → PRIVATE 変数で受ければスレッドセーフ |
| エントロピー生成（SGEN） | 低 | IFPRINT/IFEND 時のみ、各(I,J,K)独立 |

## 6. 検証手順

1. `make build` でコンパイル確認
2. `OMP_NUM_THREADS=1` でシリアル実行 → stage.log が既存と一致することを確認
3. `OMP_NUM_THREADS=2` で並列実行 → 数値一致 + 速度向上確認
4. `OMP_NUM_THREADS=4` で並列実行 → スケーリング確認
5. stage.log の数値差異を diff_stage_logs.sh で確認

## 7. 実装の順序

1. VLAM_L, VTURB_L ローカル配列を DIMENSION に追加
2. DO 50 内の VLAM→VLAM_L, VTURB→VTURB_L 置換
3. DO 50 に C$OMP PARALLEL DO を追加
4. ビルド・テスト（Phase 1 単独）
5. DO 100 内の VLAM→VLAM_L, VTURB→VTURB_L 置換
6. DO 100 に C$OMP PARALLEL DO を追加
7. ビルド・テスト（Phase 1+2）
8. 性能測定（OMP=1/2/4）
