# OPENMP_PLAN_STEPUP — STEPUP サブルーチン並列化

## 1. 対象コード

| 項目 | 値 |
|---|---|
| ファイル | `multall-open21.3-s1.0.f` |
| サブルーチン | `STEPUP(RELAX)` (L9754〜) |
| 呼び出し元 | 5ステップブロック内 `IF(ITIMST.GE.3) CALL STEPUP(0.25)` |
| タイマーID | 71 (`5STP: STEPUP`) |
| OMP=1 実測時間 | 2.21s（100ステップ、LOOP TOTAL の約 3.7%） |
| 呼び出し回数 | 20回/100ステップ（5ステップごとに1回） |

## 2. 現状コード

### DO 1100 — TEMP1 計算（全格子点ループ）

```fortran
      DO 1100 K=1,KM        ! K=1..64
      DO 1100 J=1,JM        ! J=1..151-351
      DO 1100 I=1,IM        ! I=1..64
C     各格子点で:
C     - VSQ = VX^2 + VT^2 + VR^2
C     - HSTAT = HO - 0.5*VSQ
C     - GAMNOW = f(IFGAS, GA, GA_PV, TFROMH...)
C     - WSQ = VSQ - (VT^2 - WTREL^2), clamp to VLIM
C     - V_SONIC = SQRT(GAMNOW*P/RO) or VSOUND
C     - TEMP1(I,J,K) = VSOUND / (SQRT(WSQ) + V_SONIC)
 1100 CONTINUE
```

### DO 10 — STEP/RSTEP 更新（セル中心ループ）

```fortran
      DO 10 I=1,IMM1        ! I=1..63
      IP1=I+1
      DO 10 K=1,KMM1        ! K=1..63
      KP1=K+1
C     AMACHP = 4点平均 TEMP1(I:IP1, J=1, K:KP1)
      DO 10 J=2,JM          ! J=2..151-351
C     AMACHL = 4点平均 TEMP1(I:IP1, J, K:KP1)
C     AVMACH = 0.125*(AMACHL + AMACHP)  [8点平均]
C     STEP(I,J,K) = RELAX*BSTEP*AVMACH + (1-RELAX)*STEP
C     RSTEP(I,J,K) = STEP/BSTEP
C     AMACHP = AMACHL  [J方向の carried dependency]
   10 CONTINUE
```

### DO 101/201 — 混合面タイムステップ設定（小規模）

```fortran
      DO 101 NR=1,NRWSM1    ! NR=1..3 (blade rows - 1)
      DO 201 K=1,KMM1
      DO 201 I=1,IMM1
      STEP(I,JP1,K) = STEP(I,JP2,K)
      RSTEP(I,JP1,K) = 0.0
  201 CONTINUE
  101 CONTINUE
```

## 3. データ依存性分析

### DO 1100 — TEMP1 計算

| 変数 | R/W | 分類 | 備考 |
|---|---|---|---|
| K, J, I | - | PRIVATE | ループカウンタ |
| VSQ, HSTAT, GAMNOW | RW | PRIVATE | ローカル計算 |
| TSTATIC, CPNOW | RW | PRIVATE | IFGAS=1 時のみ |
| WTREL, WSQ | RW | PRIVATE | ローカル |
| V_SONIC, VPLUSC | RW | PRIVATE | ローカル |
| VX, VT, VR, HO | R | SHARED | 流れ場配列 |
| P, RO, GA_PV, WT | R | SHARED | 流れ場配列 |
| TEMP1 | W | SHARED | 出力（各点独立） |
| GA, VSOUND | R | SHARED | 定数 |
| IFGAS, ITIMST | R | SHARED | フラグ |
| TREF, HREF, HT1-4 | R | SHARED | 関数パラメータ |
| CP1, CP2, CP3, RGAS | R | SHARED | 熱力学定数 |
| TLIM, VLIM | R | SHARED | リミッタ |

**依存性**: なし。各(I,J,K)が完全に独立。TFROMH は純粋関数。

### DO 10 — STEP 更新

| 変数 | R/W | 分類 | 備考 |
|---|---|---|---|
| I, IP1 | - | PRIVATE | 外側ループ |
| K, KP1, J | - | PRIVATE | ループカウンタ |
| AMACHP | RW | PRIVATE | **J方向 carried dependency** |
| AMACHL, AVMACH | RW | PRIVATE | ローカル |
| STEPNEW | RW | PRIVATE | ローカル |
| TEMP1 | R | SHARED | 入力（DO 1100 で計算済み） |
| BSTEP | R | SHARED | 基本タイムステップ |
| STEP, RSTEP | RW | SHARED | 出力（I軸並列なら各点独立） |
| RELAX, RELAX1 | R | SHARED | 定数 |
| IMM1, KMM1, JM | R | SHARED | ループ上限 |

**依存性**: AMACHP が J 方向に carried dependency を持つ。I 軸または K 軸での並列化は安全。

## 4. 並列化方針

### DO 1100 — K 軸 PARALLEL DO

```fortran
C$OMP PARALLEL DO DEFAULT(NONE)
C$OMP&PRIVATE(K,J,I,VSQ,HSTAT,GAMNOW,TSTATIC,CPNOW,
C$OMP&WTREL,WSQ,V_SONIC,VPLUSC)
C$OMP&SHARED(KM,JM,IM,VX,VT,VR,HO,P,RO,GA_PV,WT,
C$OMP&TEMP1,IFGAS,ITIMST,GA,VSOUND,
C$OMP&TREF,HREF,HT1,HT2,HT3,HT4,
C$OMP&CP1,CP2,CP3,RGAS,TLIM,VLIM)
C$OMP&SCHEDULE(STATIC)
      DO 1100 K=1,KM
      DO 1101 J=1,JM
      DO 1102 I=1,IM
      ...
 1102 CONTINUE
 1101 CONTINUE
 1100 CONTINUE
C$OMP END PARALLEL DO
```

- KM=64 なので 4 スレッドで 16 反復/スレッド（十分な粒度）
- 最内ループ I が連続メモリ方向
- 共有 DO 終端ラベルを分離（OMP 構造化要件）

### DO 10 — I 軸 PARALLEL DO

```fortran
C$OMP PARALLEL DO DEFAULT(NONE)
C$OMP&PRIVATE(I,IP1,K,KP1,J,AMACHP,AMACHL,AVMACH,STEPNEW)
C$OMP&SHARED(IMM1,KMM1,JM,TEMP1,BSTEP,STEP,RSTEP,
C$OMP&RELAX,RELAX1)
C$OMP&SCHEDULE(STATIC)
      DO 10 I=1,IMM1
      IP1=I+1
      DO 11 K=1,KMM1
      KP1=K+1
      ...
   12 CONTINUE
   11 CONTINUE
   10 CONTINUE
C$OMP END PARALLEL DO
```

- IMM1=63 で 4 スレッドに十分
- J 方向の carried dependency (AMACHP) は各 I スレッドで独立に保持
- K 軸並列化も可能だが、I 軸のほうが STEP/RSTEP の書き込みがストライド 1 で有利

### DO 101/201 — 並列化なし

- NRWSM1=3 でループ回数が極小。オーバーヘッドが勝つ。

## 5. ベクトル化分析

gfortran -O3 -ffast-math -march=native でのアセンブリ解析結果:

| ループ | packed (ymm) 命令数 | scalar 命令数 | ベクトル化率 | 原因 |
|---|---|---|---|---|
| DO 1100 | 220 | 222 | **部分的** | IFGAS 条件分岐で一部スカラ化。SQRT/条件分岐あり |
| DO 10 | 7 | 108 | **ほぼ未ベクトル化** | I 軸が最外ループ（OMP軸）のため内側 K×J に対してベクトル化が効きにくい |

### DO 10 のベクトル化が難しい理由

- I 軸で並列化しているため、内側の K 軸・J 軸がベクトル化対象
- しかし TEMP1 の参照パターン `TEMP1(I,J,K) + TEMP1(IP1,J,K)` が I 方向のストライドアクセス
- AMACHP が J 方向に carried dependency を持つため、J 軸のベクトル化も困難
- K 軸で並列化すれば J 軸のベクトル化の可能性があるが、STEP/RSTEP の書き込みストライドが悪化

### 改善の方向性

1. **DO 1100 に `C$OMP SIMD` を内側ループに追加**: I 軸のベクトル化を明示的に指示
2. **DO 10 のループ並列化軸を K に変更**: 内側 J ループの AMACHP 依存を解消するため、J の初期値を毎回計算する方式に変更すればベクトル化の余地が生まれるが、コード変更量が大きい

## 6. リスク評価

| リスク | 評価 |
|---|---|
| データ競合 | **低** — 各ループで (I,J,K) の書き込みは独立 |
| 数値差異 | **低** — 浮動小数点の演算順序変更による last-digit 差のみ |
| 並列粒度 | **中** — DO 1100: K=64 で十分。DO 10: I=63 で十分だが、内側のベクトル化が弱い |
| TFROMH 関数呼び出し | **低** — 純粋関数（引数のみ使用、副作用なし） |

## 7. 実測結果

### 並列化後のベンチマーク（ローカル i5-12400F, 100ステップ）

| | OMP=1 | OMP=2 | OMP=4 | OMP=4 倍率 |
|---|---|---|---|---|
| 5STP: STEPUP | 2.208s | 1.538s | 1.506s | 1.47x |
| LOOP: EVERY 5 STEPS | 3.566s | 2.714s | 2.554s | 1.40x |
| LOOP: TOTAL | 59.23s | 40.25s | 35.71s | 1.66x |

### 数値検証

| 値 | 前 (OMP=1) | 後 (OMP=1) | 後 (OMP=4) |
|---|---|---|---|
| EMAX | 2.7796 | 2.7797 | 2.7796 |
| EAVG | 0.1316 | 0.1316 | 0.1318 |
| ECONT | 0.7082 | 0.7082 | 0.7082 |
| FLOW | 194.0615 | 194.0616 | 194.0616 |

last-digit の浮動小数点差のみ。数値的に問題なし。

### スケーリングが 1.47x で頭打ちの原因

- DO 10 のベクトル化不足（scalar 108 vs packed 7）
- DO 1100 は部分的にベクトル化されているが、条件分岐（IFGAS）で効率低下
- スレッド生成オーバーヘッド（20回呼び出し × 2 PARALLEL DO = 40回のフォーク/ジョイン）
