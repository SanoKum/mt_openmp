# OpenMP 並列化方針: TSTEP（DENSITY / ENERGY / MOMENTUM 3成分）

## 概要

`SUBROUTINE TSTEP(D,DIFF,NCALL)` は全保存変数（RO, ROE, ROVX, RORVT, ROVR）の
時間更新を行う共通ルーチンで、5回呼び出される。各呼び出しの計測時間は以下の通り:

| ID | 区間名 | 時間 (s) | 変数 | NCALL |
|---|---|---|---|---|
| 14 | TSTEP: DENSITY | 3.007 | RO (or ROSUB) / DRO | 1 |
| 16 | TSTEP: ENERGY | 2.982 | ROE / DROE | 3 |
| 17 | TSTEP: MOMENTUM-X | 2.960 | ROVX / DROVX | 4 |
| 18 | TSTEP: MOMENTUM-T | 3.003 | RORVT / DRORVT | 5 |
| 19 | TSTEP: MOMENTUM-R | 2.924 | ROVR / DROVR | 6 |
| | **合計** | **14.876** | | |

LOOP: TOTAL = 33.766 s のうち **44.0%** を占める最大のボトルネック群。

---

## 1. TSTEP 内部の構造と個別区間の時間配分

TSTEP 内部は以下のサブ区間に分かれる（5回呼び出し合計の値）:

| ID | 区間名 | 時間 (s) | 割合 |
|---|---|---|---|
| 20 | MG INIT | 0.027 | 0.2% |
| 21 | TIP GAP | 0.000 | 0.0% |
| 22 | FEXTRAP | 0.023 | 0.2% |
| 23 | DELTA/STORE | 2.593 | 17.4% |
| 24 | PITCH AVG | 0.015 | 0.1% |
| 25 | MG AGG | 1.622 | 10.9% |
| 26 | STEP/DAMP | 3.008 | 20.2% |
| 28 | CELL->NODE | 3.329 | 22.4% |
| 29 | SMOOTH_VAR | 4.085 | 27.5% |
| 30 | FINAL AVG | 0.162 | 1.1% |
| 31 | PITCH AVG POST-MG | 0.010 | 0.1% |

**主要ターゲット**: CELL->NODE (22.4%), STEP/DAMP (20.2%), DELTA/STORE (17.4%), MG AGG (10.9%)

（SMOOTH_VAR は既に別途並列化済み）

---

## 2. 並列化戦略

### 2.1 基本方針: TSTEP 内部のループ単位で並列化

TSTEP は `CALL TSTEP(D, DIFF, NCALL)` として呼び出され、引数 `D` と `DIFF` は
呼び出しごとに異なる配列を受け取る。TSTEP 内部の各ループは独立しているため、
**ループ単位で `C$OMP PARALLEL DO` を適用**する。

TSTEP は 1 タイムステップ中に 5 回呼ばれるため、内部ループの並列化が
5 回分すべてに効く。

### 2.2 並列化不可の区間

以下の区間は並列化を見送る:

- **TIP GAP (DO 520/521)**: 時間 0.000s、効果なし
- **FEXTRAP (DO 4100-4140)**: 時間 0.023s、混合面での FLOWDIRN 依存分岐あり
- **PITCH AVG (DO 1750-1790)**: 時間 0.015s、I方向の和→再分配パターン
- **PITCH AVG POST-MG (DO 1850-1890)**: 時間 0.010s、同上
- **FINAL AVG (DO 8000, DO 510 等)**: 時間 0.162s、周期境界・混合面処理

---

## 3. 各ターゲットループの並列化プラン

### 3.1 DELTA/STORE (DO 1000) — 2.593s

```fortran
C     ソースコード（L6637-6649）:
      DO 1000 K=1,KMM1
      DO 1000 J=2,JM
      RATPITCH = FLOAT(NBLADE(J-1))/NBLADE(J)
      DO 1000 I=1,IMM1
      DELTA        = XFLUX(I,J,K) - XFLUX(I,J-1,K)*RATPITCH
     &             + RFLUX(I,J,K) - RFLUX(I,J,K+1)
     &             + TFLUX(I,J,K) - TFLUX(I+1,J,K)
     &             - SOURCE(I,J,K)
      STORE(I,J,K)  = F1*DELTA + F2*DIFF(I,J,K)
      DIFF(I,J,K)   = DELTA    + F3*DIFF(I,J,K)
 1000 CONTINUE
```

**依存性分析**:
- `XFLUX`, `RFLUX`, `TFLUX`, `SOURCE` は読み出しのみ → SHARED
- `STORE(I,J,K)` と `DIFF(I,J,K)` はセル `(I,J,K)` のみ書く → 競合なし
- `RATPITCH`, `DELTA` はスカラー一時変数 → PRIVATE
- `NBLADE(J-1)`, `NBLADE(J)` は読み出しのみ → SHARED

**提案**:
```fortran
C$OMP PARALLEL DO DEFAULT(NONE) COLLAPSE(3)
C$OMP&PRIVATE(I,J,K,RATPITCH,DELTA)
C$OMP&SHARED(XFLUX,TFLUX,RFLUX,SOURCE,STORE,DIFF,
C$OMP&NBLADE,F1,F2,F3,IMM1,JM,KMM1)
C$OMP&SCHEDULE(STATIC)
      DO 1000 K=1,KMM1
      DO 1000 J=2,JM
      DO 1000 I=1,IMM1
      RATPITCH = FLOAT(NBLADE(J-1))/NBLADE(J)
      DELTA        = XFLUX(I,J,K) - XFLUX(I,J-1,K)*RATPITCH
     &             + RFLUX(I,J,K) - RFLUX(I,J,K+1)
     &             + TFLUX(I,J,K) - TFLUX(I+1,J,K)
     &             - SOURCE(I,J,K)
      STORE(I,J,K)  = F1*DELTA + F2*DIFF(I,J,K)
      DIFF(I,J,K)   = DELTA    + F3*DIFF(I,J,K)
 1000 CONTINUE
C$OMP END PARALLEL DO
```

**リスク**: 低。完全に要素独立な計算。

---

### 3.2 MG AGG (DO 700) — 1.622s

```fortran
C     ソースコード（L6707-6720）:
      DO 700 K=1,KMM1
      K1 = KB1(K)
      K2 = KB2(K)
      DO 700 J=2,JM
      JSB= JSBLK(J)
      J1 = JB1(J)
      J2 = JB2(J)
      DO 700 I=1,IMM1
      I1 = IB1(I)
      I2 = IB2(I)
      DELTA = STORE(I,J,K)
      B1CHG(I1,J1,K1) = B1CHG(I1,J1,K1) + DELTA
      B2CHG(I2,J2,K2) = B2CHG(I2,J2,K2) + DELTA
      SBCHG(JSB)      = SBCHG(JSB)      + DELTA
  700 CONTINUE
```

**依存性分析**:
- 複数の `(I,J,K)` が同じ `(I1,J1,K1)` にマップされる → **書き込み競合**
- `B1CHG`, `B2CHG`, `SBCHG` への加算はリダクション的だが、配列要素が不定
- マッピング `KB1(K)`, `JB1(J)`, `IB1(I)` は粗視化（coarsening）で、
  複数の fine セルが 1 つの coarse セルに対応

**提案**: **ATOMIC を使った並列化**
```fortran
C$OMP PARALLEL DO DEFAULT(NONE) COLLAPSE(3)
C$OMP&PRIVATE(I,J,K,I1,I2,J1,J2,K1,K2,JSB,DELTA)
C$OMP&SHARED(STORE,B1CHG,B2CHG,SBCHG,
C$OMP&KB1,KB2,JB1,JB2,IB1,IB2,JSBLK,IMM1,JM,KMM1)
C$OMP&SCHEDULE(STATIC)
      DO 700 K=1,KMM1
      DO 700 J=2,JM
      DO 700 I=1,IMM1
      K1 = KB1(K)
      K2 = KB2(K)
      JSB= JSBLK(J)
      J1 = JB1(J)
      J2 = JB2(J)
      I1 = IB1(I)
      I2 = IB2(I)
      DELTA = STORE(I,J,K)
C$OMP ATOMIC
      B1CHG(I1,J1,K1) = B1CHG(I1,J1,K1) + DELTA
C$OMP ATOMIC
      B2CHG(I2,J2,K2) = B2CHG(I2,J2,K2) + DELTA
C$OMP ATOMIC
      SBCHG(JSB)      = SBCHG(JSB)      + DELTA
  700 CONTINUE
C$OMP END PARALLEL DO
```

**リスク**: 中。ATOMIC のオーバーヘッド vs 並列化ゲインのトレードオフ。
粗視化配列は小さい（`IG1*JG1*KG1` 等）ため衝突は少ないと予想。
効果が不十分なら、J 方向の行ごとに分割（J 方向は `JSBLK` で異なるブロックに
対応しやすい）してローカル集計 → グローバル加算に変更を検討。

**代替案**: ローカルバッファ + 後でマージ
```fortran
C     各スレッドがローカルの B1CHG_LOCAL 等に集計し、
C     PARALLEL 領域終了後にマージ（CRITICAL で加算）。
C     ただしローカル配列のサイズが大きい場合はメモリ圧迫に注意。
```

---

### 3.3 STEP/DAMP (DO 1500 + DO 1501/1502 + DO 1525) — 3.008s

この区間は 3 つのループで構成される:

#### 3.3.1 DO 1500 — マルチグリッド変化量の適用

```fortran
      DO 1500 K=1,KMM1
      K1 = KB1(K)
      K2 = KB2(K)
      DO 1500 J=2,JM
      JSB= JSBLK(J)
      J1 = JB1(J)
      J2 = JB2(J)
      DO 1500 I=1,IMM1
      I1 = IB1(I)
      I2 = IB2(I)
      DELTA = STORE(I,J,K)*STEP(I,J,K)
     &      + (B1CHG(I1,J1,K1)*STEP1(I1,J1,K1)
     &      + B2CHG(I2,J2,K2)*STEP2(I2,J2,K2)
     &      + SBCHG(JSB)*STEPSBK(JSB))*RSTEP(I,J,K)
      STORE(I,J,K) = DELTA
 1500 CONTINUE
```

**依存性分析**: `B1CHG` 等は読み出しのみ。`STORE(I,J,K)` は要素独立 → **安全**

**提案**:
```fortran
C$OMP PARALLEL DO DEFAULT(NONE) COLLAPSE(3)
C$OMP&PRIVATE(I,J,K,I1,I2,J1,J2,K1,K2,JSB,DELTA)
C$OMP&SHARED(STORE,STEP,RSTEP,B1CHG,B2CHG,SBCHG,
C$OMP&STEP1,STEP2,STEPSBK,
C$OMP&KB1,KB2,JB1,JB2,IB1,IB2,JSBLK,IMM1,JM,KMM1)
C$OMP&SCHEDULE(STATIC)
      DO 1500 K=1,KMM1
      DO 1500 J=2,JM
      DO 1500 I=1,IMM1
      ...
 1500 CONTINUE
C$OMP END PARALLEL DO
```

#### 3.3.2 DO 1501/1502 — ブレード行ごとの平均変化量の計算

```fortran
      DO 1501 NR = 1,NROWS
      ...
      DO 1502 K=1,KMM1
      DO 1502 J = JST, JEN
      DO 1502 I=1,IMM1
      SUMCHG = SUMCHG + ABS(STORE(I,J,K))
 1502 CONTINUE
      AVG_CHG(NR) = SUMCHG/NSUM
 1501 CONTINUE
```

**依存性分析**: 外側ループ `NR` でリダクション `SUMCHG`。
`NROWS` は通常 2〜4 程度で小さいため、内側 K/J/I ループを並列化。

**提案**: NROWS が小さい場合は NR ループの内側（K/J/I）で REDUCTION
```fortran
      DO 1501 NR = 1,NROWS
      SUMCHG = 0.0
      JST = JSTART(NR) + 1
      JEN = JMIX(NR)   - 1
      JCHANGE = JEN - JST + 1
      NSUM = IMM1*KMM1*JCHANGE
C$OMP PARALLEL DO DEFAULT(NONE) COLLAPSE(3)
C$OMP&PRIVATE(I,J,K)
C$OMP&SHARED(STORE,KMM1,IMM1,JST,JEN)
C$OMP&REDUCTION(+:SUMCHG) SCHEDULE(STATIC)
      DO 1502 K=1,KMM1
      DO 1502 J = JST, JEN
      DO 1502 I=1,IMM1
      SUMCHG = SUMCHG + ABS(STORE(I,J,K))
 1502 CONTINUE
C$OMP END PARALLEL DO
      AVG_CHG(NR) = SUMCHG/NSUM
 1501 CONTINUE
```

#### 3.3.3 DO 1525 — ネガティブフィードバックの適用

```fortran
      DO 1525 K=1,KMM1
      DO 1525 J=2,JM
      NR     = NROW(J)
      DO 1525 I=1,IMM1
      DELTA  = STORE(I,J,K)
      ABSCHG = ABS(DELTA)
      FDAMP  = ABSCHG/AVG_BLK(NR)
      STORE(I,J,K) = DELTA/(1. + FDAMP/DAMP )
 1525 CONTINUE
```

**依存性分析**: 各 `(I,J,K)` で独立 → **安全**

**提案**:
```fortran
C$OMP PARALLEL DO DEFAULT(NONE) COLLAPSE(3)
C$OMP&PRIVATE(I,J,K,NR,DELTA,ABSCHG,FDAMP)
C$OMP&SHARED(STORE,NROW,AVG_BLK,DAMP,IMM1,JM,KMM1)
C$OMP&SCHEDULE(STATIC)
      DO 1525 K=1,KMM1
      DO 1525 J=2,JM
      DO 1525 I=1,IMM1
      NR     = NROW(J)
      ...
 1525 CONTINUE
C$OMP END PARALLEL DO
```

---

### 3.4 CELL->NODE (DO 1100) — 3.329s 【最重要・最難関】

```fortran
      DO 1100 K=1,KMM1
      DO 1100 J=2,JM
      DO 1100 I=1,IMM1
      ADD = STORE(I,J,K)
      D(I,J,K)      =  D(I,J,K)       + ADD*FBL(I,K)*FACDWN(J)
      D(I+1,J,K)    =  D(I+1,J,K)     + ADD*FBR(I,K)*FACDWN(J)
      D(I,J,K+1)    =  D(I,J,K+1)     + ADD*FTL(I,K)*FACDWN(J)
      D(I+1,J,K+1)  =  D(I+1,J,K+1)   + ADD*FTR(I,K)*FACDWN(J)
      D(I,J-1,K)    =  D(I,J-1,K)     + ADD*FBL(I,K)*FACUP(J)
      D(I+1,J-1,K)  =  D(I+1,J-1,K)   + ADD*FBR(I,K)*FACUP(J)
      D(I,J-1,K+1)  =  D(I,J-1,K+1)   + ADD*FTL(I,K)*FACUP(J)
      D(I+1,J-1,K+1)=  D(I+1,J-1,K+1) + ADD*FTR(I,K)*FACUP(J)
 1100 CONTINUE
```

**依存性分析**:
- セル `(I,J,K)` が隣接ノードに書き込む scatter パターン
- `D(I,J,K)` にはセル `(I,J,K)`, `(I-1,J,K)`, `(I,J,K-1)`, `(I-1,J,K-1)`,
  `(I,J+1,K)`, `(I-1,J+1,K)`, `(I,J+1,K-1)`, `(I-1,J+1,K-1)` からの寄与がある
- **I 方向**: `D(I,...)` と `D(I+1,...)` が衝突 → I 方向は並列化不可（ATOMIC 必要）
- **K 方向**: `D(...,K)` と `D(...,K+1)` が衝突 → K 方向も並列化不可
- **J 方向**: `D(...,J,...)` と `D(...,J-1,...)` が衝突 → J 方向も並列化不可

**戦略A: ATOMIC を使った並列化（シンプル）**

```fortran
C$OMP PARALLEL DO DEFAULT(NONE) COLLAPSE(3)
C$OMP&PRIVATE(I,J,K,ADD,FACDN,FACUP_J,
C$OMP&ADD_BL,ADD_BR,ADD_TL,ADD_TR)
C$OMP&SHARED(D,STORE,FBL,FBR,FTL,FTR,FACDWN,FACUP,
C$OMP&IMM1,JM,KMM1) SCHEDULE(STATIC)
      DO 1100 K=1,KMM1
      DO 1100 J=2,JM
      DO 1100 I=1,IMM1
      ADD = STORE(I,J,K)
      FACDN = FACDWN(J)
      FACUP_J = FACUP(J)
      ADD_BL = ADD*FBL(I,K)
      ADD_BR = ADD*FBR(I,K)
      ADD_TL = ADD*FTL(I,K)
      ADD_TR = ADD*FTR(I,K)
C$OMP ATOMIC
      D(I,J,K)      =  D(I,J,K)       + ADD_BL*FACDN
C$OMP ATOMIC
      D(I+1,J,K)    =  D(I+1,J,K)     + ADD_BR*FACDN
C$OMP ATOMIC
      D(I,J,K+1)    =  D(I,J,K+1)     + ADD_TL*FACDN
C$OMP ATOMIC
      D(I+1,J,K+1)  =  D(I+1,J,K+1)   + ADD_TR*FACDN
C$OMP ATOMIC
      D(I,J-1,K)    =  D(I,J-1,K)     + ADD_BL*FACUP_J
C$OMP ATOMIC
      D(I+1,J-1,K)  =  D(I+1,J-1,K)   + ADD_BR*FACUP_J
C$OMP ATOMIC
      D(I,J-1,K+1)  =  D(I,J-1,K+1)   + ADD_TL*FACUP_J
C$OMP ATOMIC
      D(I+1,J-1,K+1)=  D(I+1,J-1,K+1) + ADD_TR*FACUP_J
 1100 CONTINUE
C$OMP END PARALLEL DO
```

**リスク**: 高。8 個の ATOMIC 演算は大きなオーバーヘッドになる可能性。

**戦略B: scatter → gather 変換（推奨）**

scatter パターンを gather パターンに書き換え、各ノード `(I,J,K)` が
周囲 8 つのセルからの寄与を読み集める形にする:

```fortran
C     ノード (I,J,K) には以下のセルから寄与がある:
C     セル (I,J,K)   → FBL(I,K)*FACDWN(J)    の重み
C     セル (I-1,J,K) → FBR(I-1,K)*FACDWN(J)  の重み
C     セル (I,J,K-1) → FTL(I,K-1)*FACDWN(J)  の重み
C     セル (I-1,J,K-1)→ FTR(I-1,K-1)*FACDWN(J) の重み
C     セル (I,J+1,K)  → FBL(I,K)*FACUP(J+1)  の重み
C     セル (I-1,J+1,K)→ FBR(I-1,K)*FACUP(J+1) の重み
C     セル (I,J+1,K-1)→ FTL(I,K-1)*FACUP(J+1) の重み
C     セル (I-1,J+1,K-1)→ FTR(I-1,K-1)*FACUP(J+1)の重み
C
C     D(I,J,K) = D(I,J,K) + sum of contributions

C$OMP PARALLEL DO DEFAULT(NONE) COLLAPSE(3)
C$OMP&PRIVATE(I,J,K,GATHER_SUM)
C$OMP&SHARED(D,STORE,FBL,FBR,FTL,FTR,FACDWN,FACUP,
C$OMP&IM,JM,KM,IMM1,KMM1) SCHEDULE(STATIC)
      DO K=1,KM
      DO J=1,JM
      DO I=1,IM
      GATHER_SUM = 0.0
C     セル(I,J,K)の寄与: I<=IMM1, J>=2, K<=KMM1
      IF(I.LE.IMM1.AND.J.GE.2.AND.K.LE.KMM1)
     &  GATHER_SUM = GATHER_SUM
     &  + STORE(I,J,K)*FBL(I,K)*FACDWN(J)
C     セル(I-1,J,K)の寄与
      IF(I.GE.2.AND.J.GE.2.AND.K.LE.KMM1)
     &  GATHER_SUM = GATHER_SUM
     &  + STORE(I-1,J,K)*FBR(I-1,K)*FACDWN(J)
C     セル(I,J,K-1)の寄与
      IF(I.LE.IMM1.AND.J.GE.2.AND.K.GE.2)
     &  GATHER_SUM = GATHER_SUM
     &  + STORE(I,J,K-1)*FTL(I,K-1)*FACDWN(J)
C     セル(I-1,J,K-1)の寄与
      IF(I.GE.2.AND.J.GE.2.AND.K.GE.2)
     &  GATHER_SUM = GATHER_SUM
     &  + STORE(I-1,J,K-1)*FTR(I-1,K-1)*FACDWN(J)
C     セル(I,J+1,K)の寄与: J+1 <= JM → J <= JM-1
      IF(I.LE.IMM1.AND.J.LE.JM-1.AND.K.LE.KMM1)
     &  GATHER_SUM = GATHER_SUM
     &  + STORE(I,J+1,K)*FBL(I,K)*FACUP(J+1)
C     セル(I-1,J+1,K)の寄与
      IF(I.GE.2.AND.J.LE.JM-1.AND.K.LE.KMM1)
     &  GATHER_SUM = GATHER_SUM
     &  + STORE(I-1,J+1,K)*FBR(I-1,K)*FACUP(J+1)
C     セル(I,J+1,K-1)の寄与
      IF(I.LE.IMM1.AND.J.LE.JM-1.AND.K.GE.2)
     &  GATHER_SUM = GATHER_SUM
     &  + STORE(I,J+1,K-1)*FTL(I,K-1)*FACUP(J+1)
C     セル(I-1,J+1,K-1)の寄与
      IF(I.GE.2.AND.J.LE.JM-1.AND.K.GE.2)
     &  GATHER_SUM = GATHER_SUM
     &  + STORE(I-1,J+1,K-1)*FTR(I-1,K-1)*FACUP(J+1)
C
      D(I,J,K) = D(I,J,K) + GATHER_SUM
      ENDDO
      ENDDO
      ENDDO
C$OMP END PARALLEL DO
```

**メリット**: 各ノード `(I,J,K)` は自分のみに書き込む → 完全に競合なし。
**デメリット**: 境界チェックの IF 文が多くベクトル化が難しい。
STORE の参照パターンが複雑で L1 キャッシュヒット率が低下する可能性。

**戦略C: カラーリング（奇偶分割）**

I 方向と K 方向で奇偶を分けて 2 パスで実行する:
- パス1: (I+K) が偶数のセルのみ処理
- パス2: (I+K) が奇数のセルのみ処理

J 方向の衝突は `D(I,J,K)` と `D(I,J-1,K)` だが、J 方向にも奇偶分割すると
4 パスまたは 8 パスに増え、オーバーヘッドが大きい。

**推奨**: まず **戦略A（ATOMIC）** で実装し、効果を計測。
ATOMIC のオーバーヘッドが大きければ **戦略B（gather 変換）** を試す。

---

### 3.5 MG INIT (DO 110/210/310) — 0.027s

時間が小さいため並列化の優先度は低いが、簡単なので適用可能。

```fortran
C$OMP PARALLEL DO DEFAULT(NONE) COLLAPSE(3)
C$OMP&PRIVATE(I,J,K) SHARED(B1CHG,NIB1,NJB1,NKB1)
C$OMP&SCHEDULE(STATIC)
      DO 110 K=1,NKB1
      DO 110 J=1,NJB1+1
      DO 110 I=1,NIB1
      B1CHG(I,J,K) = 0.0
  110 CONTINUE
C$OMP END PARALLEL DO
```

---

### 3.6 SMOOTH_RESID — 間接的影響

`SMOOTH_RESID` は `NRSMTH > 0` の場合に呼ばれる。内部に J/I/K 方向の
逐次スムージング（1 方向ずつ `TEMPVAR` で平滑化）が含まれる。
`TEMPVAR` がローカル配列のため、**各方向のスムージングは並列化が難しい**
（1 方向の処理が前の結果に依存するため）。

ただし `NR`（ブレード行）間は独立であり、NR ループの並列化は可能。
効果は `NROWS` の数に依存する（2〜4 程度では大きな効果は期待できない）。

---

## 4. 実装順序と優先度

| 優先度 | ターゲット | 時間 (s) | 難易度 | 期待短縮 |
|---|---|---|---|---|
| 1 | DELTA/STORE (DO 1000) | 2.593 | 低 | 高 |
| 2 | STEP (DO 1500) | (一部) | 低 | 中 |
| 3 | DAMP (DO 1525) | (一部) | 低 | 中 |
| 4 | DAMP REDUCTION (DO 1502) | (一部) | 低 | 低〜中 |
| 5 | CELL->NODE (DO 1100) | 3.329 | 高 | 高 |
| 6 | MG AGG (DO 700) | 1.622 | 中 | 中 |

### 実装手順

1. **フェーズ 1**: DO 1000, DO 1500, DO 1525 を `C$OMP PARALLEL DO` で並列化
   - 全て要素独立の計算のため安全
   - DO 1502 のリダクションも同時に実装
   - 期待: STEP/DAMP 区間 3.0s のうち大部分を短縮

2. **フェーズ 2**: DO 1100 (CELL->NODE) を ATOMIC アプローチで並列化
   - 効果計測後、必要に応じて gather 変換を検討

3. **フェーズ 3**: DO 700 (MG AGG) を ATOMIC アプローチで並列化

4. **フェーズ 4**: 効果計測・チューニング
   - `OMP_NUM_THREADS=1` と `2` で比較
   - `stage.log.original` との数値差異確認

---

## 5. 性能見積り

### 2 スレッドの場合

TSTEP の主要ループ合計: DELTA/STORE (2.6s) + STEP/DAMP (3.0s) + CELL->NODE (3.3s) + MG AGG (1.6s) = 10.5s

- 効率 70% と仮定: $10.5 \times (1 - 0.7/2) = 10.5 \times 0.65 = 6.8s$（約 3.7s 短縮）
- TSTEP 全体 14.9s → 約 11.2s（25% 短縮）
- LOOP: TOTAL 33.8s → 約 30.1s

### フェーズ1のみ（安全なループのみ）の場合

対象: DELTA/STORE (2.6s) + STEP/DAMP (3.0s) = 5.6s
- 効率 80%: $5.6 \times 0.6 = 3.4s$（約 2.2s 短縮）
- LOOP: TOTAL 33.8s → 約 31.6s

---

## 6. 注意事項

1. **NCALL=2（SA 乱流粘性）の特殊処理**: TSTEP は `NCALL=2` の場合に
   SA モデル専用の処理（DO 1600, `SMOOTH_RESID` 呼び出し、GOTO 8700）がある。
   今回のテストケースでは `SA VISC = 0.0s` のため影響なし。
   将来 SA モデル使用時には `NCALL=2` パスの並列化も必要。

2. **DIFF 配列の更新**: DO 1000 内で `DIFF(I,J,K)` が更新されるが、
   各 `(I,J,K)` で独立しているため安全。ただし `DIFF` は COMMON ブロックでなく
   引数渡しの配列であることに注意。

3. **SMOOTH_VAR 内の並列化との干渉**: SMOOTH_VAR は既に並列化済み。
   TSTEP 内で SMOOTH_VAR を呼ぶ前に `C$OMP END PARALLEL DO` で
   並列領域を閉じておく必要がある（TSTEP 全体を 1 つの PARALLEL 領域に
   するアプローチは SMOOTH_VAR 内のネスト並列で問題が起きる可能性）。

4. **ループ単位の PARALLEL DO** を使うため、スレッド生成/破棄の
   オーバーヘッドが 5（変数）× 複数ループ 分生じる。
   見通しが立ったら、TSTEP 内部全体を 1 つの `C$OMP PARALLEL` 領域として
   個別ループを `C$OMP DO` にするリファクタリングを検討。
