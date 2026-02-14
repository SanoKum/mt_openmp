# OpenMP 並列化プラン: CONV CHECK 5STP (Timer ID 68)

## 1. 対象コード

- **ファイル**: `dev/src/multall-open21.3-s1.0.f`
- **行**: L6445〜L6704（8000 CONTINUE ブロック全体）
- **タイマー**: `T_LOOP_CONV5STP` (ID=68)
- **OMP=1 実測時間**: 3.50s（LOOP TOTAL の約 5.8%）
- **実行頻度**: 5ステップに1回（100ステップで20回実行）

## 2. 現状コード

### 2a. DO 5675 — BLADE_FLOW / CHECK_FLOW 計算 (L6458-6466)

```fortran
      DO 5675 J=1,JM
      SUMAS = 0
      NB = NBLADE(J)
      DO 5665 I=1,IMM1
      DO 5665 K=1,KMM1
      SUMAS = SUMAS  - FLOWX(I,J,K)
 5665 CONTINUE
      BLADE_FLOW(J) =  SUMAS*NB
      CHECK_FLOW(J)  = (SUMAS + SHRDFLOW(J))*NB+ SUMBLEED(J)- SUMCWL(J)
 5675 CONTINUE
```

### 2b. DO 5677 — ECONT 最大値 (L6467-6474)

```fortran
      ECONT    = 0.0
      DO 5677 J=1,JM
      RATIO = CHECK_FLOW(J)/CHECK_FLOW(1)
      EMASS = ABS(1.-RATIO)
      IF(EMASS.GT.ECONT) THEN
          ECONT = EMASS
          JCONT = J
      END IF
 5677 CONTINUE
```

### 2c. DO 8500 — EMAX/EAVG/STORE 計算 (L6494-6519) ★メイン

```fortran
      EMAX=0.0
      EAVG=0.0
      VREF = SQRT(VRMS*RIJKM)
      RVEF = 100./VREF
      DO 8500 K=1,KM
      DO 8510 J=2,JMM1
      XD = DX(J,K)
      RD = DR(J,K)
      SD = DS(J,K)
      DO 8520 I=1,IM
      VM_START = (XD*VX(I,J,K)  + RD*VR(I,J,K))/SD
      VM_END   = (XD*ROVX(I,J,K)
     &         +  RD*ROVR(I,J,K))/SD/RO(I,J,K)
      DVMER  = (VM_END - VM_START)*RVEF
      STORE(I,J,K) = DVMER
      EAVG   = EAVG + ABS(DVMER)
      IF(ABS(DVMER).GT.ABS(EMAX)) THEN
           EMAX = DVMER
           IMAX = I
           JMAX = J
           KMAX = K
      ENDIF
 8520 CONTINUE
 8510 CONTINUE
 8500 CONTINUE
      EAVG=EAVG/(IM*JMM1*KM)
```

## 3. データ依存性分析

### DO 5675 (BLADE_FLOW/CHECK_FLOW)

- **読み込み**: FLOWX(I,J,K), NBLADE(J), SHRDFLOW(J), SUMBLEED(J), SUMCWL(J)
- **書き込み**: BLADE_FLOW(J), CHECK_FLOW(J) — J ごとに独立
- **内部依存**: SUMAS は J ループ内の I,K 二重ループで累積 → J を並列化すれば SUMAS は PRIVATE で安全
- **安全な並列化軸**: J（各 J は独立して SUMAS を累積し、BLADE_FLOW(J) に書き込む）

### DO 5677 (ECONT)

- JM ループのみ（〜350 回）、かつ CHECK_FLOW(1) を参照 → DO 5675 の結果に依存
- 計算量が小さいため並列化不要

### DO 8500 (EMAX/EAVG/STORE) ★

- **読み込み**: VX(I,J,K), VR(I,J,K), ROVX(I,J,K), ROVR(I,J,K), RO(I,J,K), DX(J,K), DR(J,K), DS(J,K)
- **書き込み**: STORE(I,J,K) — 各点独立
- **累積**: EAVG — 全点の |DVMER| の総和 → `REDUCTION(+:EAVG)`
- **最大値追跡**: EMAX + IMAX/JMAX/KMAX — MACH CHECK と同じパターン → CRITICAL
- **安全な並列化軸**: K（最外ループ、異なるKスライスは完全独立）

## 4. 並列化方針

### 4a. DO 5675 — PARALLEL DO on J

```fortran
C$OMP PARALLEL DO DEFAULT(NONE)
C$OMP&PRIVATE(J,I,K,SUMAS,NB)
C$OMP&SHARED(JM,IMM1,KMM1,FLOWX,NBLADE,SHRDFLOW,SUMBLEED,SUMCWL,
C$OMP&BLADE_FLOW,CHECK_FLOW)
C$OMP&SCHEDULE(STATIC)
      DO 5675 J=1,JM
      SUMAS = 0
      NB = NBLADE(J)
      DO 5665 I=1,IMM1
      DO 5665 K=1,KMM1
      SUMAS = SUMAS  - FLOWX(I,J,K)
 5665 CONTINUE
      BLADE_FLOW(J) =  SUMAS*NB
      CHECK_FLOW(J)  = (SUMAS + SHRDFLOW(J))*NB+ SUMBLEED(J)- SUMCWL(J)
 5675 CONTINUE
C$OMP END PARALLEL DO
```

### 4b. DO 8500 — PARALLEL + CRITICAL + REDUCTION

```fortran
      EMAX=0.0
      EAVG=0.0
      VREF = SQRT(VRMS*RIJKM)
      RVEF = 100./VREF
C$OMP PARALLEL DEFAULT(NONE)
C$OMP&PRIVATE(I,J,K,XD,RD,SD,VM_START,VM_END,DVMER,
C$OMP&T_EMAX,T_IMAX,T_JMAX,T_KMAX)
C$OMP&SHARED(KM,JMM1,IM,DX,DR,DS,VX,VR,ROVX,ROVR,RO,RVEF,STORE)
C$OMP&REDUCTION(+:EAVG)
C$OMP&SHARED(EMAX,IMAX,JMAX,KMAX)
      T_EMAX = 0.0
      T_IMAX = 0
      T_JMAX = 0
      T_KMAX = 0
C$OMP DO SCHEDULE(STATIC)
      DO 8500 K=1,KM
      DO 8510 J=2,JMM1
      XD = DX(J,K)
      RD = DR(J,K)
      SD = DS(J,K)
      DO 8520 I=1,IM
      VM_START = (XD*VX(I,J,K)  + RD*VR(I,J,K))/SD
      VM_END   = (XD*ROVX(I,J,K)
     &         +  RD*ROVR(I,J,K))/SD/RO(I,J,K)
      DVMER  = (VM_END - VM_START)*RVEF
      STORE(I,J,K) = DVMER
      EAVG   = EAVG + ABS(DVMER)
      IF(ABS(DVMER).GT.ABS(T_EMAX)) THEN
           T_EMAX = DVMER
           T_IMAX = I
           T_JMAX = J
           T_KMAX = K
      ENDIF
 8520 CONTINUE
 8510 CONTINUE
 8500 CONTINUE
C$OMP END DO
C$OMP CRITICAL
      IF(ABS(T_EMAX).GT.ABS(EMAX)) THEN
         EMAX = T_EMAX
         IMAX = T_IMAX
         JMAX = T_JMAX
         KMAX = T_KMAX
      END IF
C$OMP END CRITICAL
C$OMP END PARALLEL
      EAVG=EAVG/(IM*JMM1*KM)
```

### 変数分類

| 変数 | 分類 | 理由 |
|---|---|---|
| I, J, K | PRIVATE | ループカウンタ |
| XD, RD, SD | PRIVATE | J,K ごとの一時変数 |
| VM_START, VM_END, DVMER | PRIVATE | ループ内一時変数 |
| T_EMAX, T_IMAX, T_JMAX, T_KMAX | PRIVATE | スレッドローカル最大値追跡 |
| EAVG | REDUCTION(+) | 全点累積 |
| KM, JMM1, IM | SHARED | ループ上限 |
| DX, DR, DS | SHARED | 定数配列 |
| VX, VR, ROVX, ROVR, RO | SHARED | 入力配列 |
| RVEF | SHARED | 定数 |
| STORE | SHARED | 出力配列（各点独立書き込み） |
| EMAX, IMAX, JMAX, KMAX | SHARED | CRITICAL で保護して更新 |

### 4c. DO 5675 の SUMAS/NB

| 変数 | 分類 | 理由 |
|---|---|---|
| J, I, K | PRIVATE | ループカウンタ |
| SUMAS | PRIVATE | J ループ内の累積変数 |
| NB | PRIVATE | J ごとの NBLADE 値 |
| JM, IMM1, KMM1 | SHARED | ループ上限 |
| FLOWX | SHARED | 入力配列 |
| NBLADE, SHRDFLOW, SUMBLEED, SUMCWL | SHARED | 定数配列 |
| BLADE_FLOW, CHECK_FLOW | SHARED | 出力配列（J ごとに独立書き込み） |

## 5. リスク評価

- **DO 8500**: リスク低。MACH CHECK と同じパターン（CRITICAL for MAX, REDUCTION for SUM）。
  - EMAX の比較は `ABS(DVMER).GT.ABS(EMAX)` → `ABS(T_EMAX)` に変更必要（EMAX は符号付き）。
  - CRITICAL 競合は最大スレッド数回のみ。
- **DO 5675**: リスク低。各 J は完全独立。JM=151〜351 なので4スレッドで十分な粒度。
- **DO 5677**: 並列化しない。CHECK_FLOW(1) への依存あり、かつ計算量が極小。

## 6. 期待効果

- DO 8500 が支配的と仮定すると、OMP=4 で 3.50s → 1.0〜1.5s（2.3〜3.5x）を期待。
- LOOP TOTAL に対して 2.0〜2.5s の短縮。
- 5ステップに1回の実行なので、1ステップあたりの改善は限定的だが、OMP=4 で LOOP TOTAL の約 7〜8% 削減に相当。
