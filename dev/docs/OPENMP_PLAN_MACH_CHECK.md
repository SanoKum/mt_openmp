# OpenMP 並列化プラン: MACH CHECK (Timer ID 65)

## 1. 対象コード

- **ファイル**: `dev/src/multall-open21.3-s1.0.f`
- **行**: L6288〜L6323
- **タイマー**: `T_LOOP_MACHCHECK` (ID=65)
- **OMP=1 実測時間**: 1.04s（LOOP TOTAL の約 2%）

## 2. 現状コード

```fortran
      REL_M_MAX = 0.0
      GANOW     = GA
      DO 7100 K=1,KM
      DO 7100 J=1,JM
      DO 7100 I=1,IM
      IF(IFGAS.EQ.3) GANOW = GA_PV(I,J,K)
      IF(RO(I,J,K).LT.ROLIM) RO(I,J,K) = ROLIM
      RONOW       = RO(I,J,K)
      WRELSQ      = VX(I,J,K)*VX(I,J,K) + VR(I,J,K)*VR(I,J,K)
     &            + WT(I,J,K)*WT(I,J,K)
      WREL        = SQRT(WRELSQ)
      WSOUND      = SQRT(GANOW*P(I,J,K)/RONOW)
      RELMACH     = WREL/WSOUND
      IF(RELMACH.GT.REL_M_MAX) THEN
         REL_M_MAX = RELMACH
         IMACH  = I
         JMACH  = J
         KMACH  = K
      END IF
      FACSAFE     = MACHLIM/RELMACH
      IF(FACSAFE.LT.1.0) THEN
           ROVX(I,J,K) = RONOW*VX(I,J,K) * FACSAFE
           ROVR(I,J,K) = RONOW*VR(I,J,K) * FACSAFE
           ROVTREL     = RONOW*WT(I,J,K) * FACSAFE
           ROWT(I,J,K) = ROVTREL
           ROVT(I,J,K) = ROVTREL + RONOW*UBLADE(J,K)
           RORVT(I,J,K)= ROVT(I,J,K)*R(J,K)
      END IF
 7100 CONTINUE
```

## 3. データ依存性分析

### 読み込み
- `RO(I,J,K)`, `VX(I,J,K)`, `VR(I,J,K)`, `WT(I,J,K)`, `P(I,J,K)` — 各点独立
- `GA_PV(I,J,K)` — 条件付き読み出し
- `UBLADE(J,K)`, `R(J,K)` — 定数配列
- `ROLIM`, `GA`, `MACHLIM`, `IFGAS` — スカラー定数

### 書き込み
- `RO(I,J,K)` — 条件付き下限クリップ（各点独立）
- `ROVX(I,J,K)`, `ROVR(I,J,K)`, `ROWT(I,J,K)`, `ROVT(I,J,K)`, `RORVT(I,J,K)` — 条件付き更新（各点独立）
- `REL_M_MAX`, `IMACH`, `JMACH`, `KMACH` — **グローバル最大値追跡（REDUCTION が必要）**

### 依存性
- **各点の RO/ROVX 等の更新は完全独立** — (I,J,K) のみに依存。
- **REL_M_MAX の更新は全点でのMAX競合** — REDUCTION(MAX) が必要。
- ただし `IMACH/JMACH/KMACH`（最大値の位置）の追跡は標準の REDUCTION では不可能。

## 4. 並列化方針

### 課題: IMACH/JMACH/KMACH の並列追跡

OpenMP の `REDUCTION(MAX:REL_M_MAX)` では最大値そのものは取得できるが、
その位置（IMACH,JMACH,KMACH）は取得できない。

#### 解決策: 2パス方式

1. **パス1（並列）**: 各点の RO クリップ + マッハ数制限を適用 + スレッドローカルで最大値を追跡
2. **パス2（MASTER）**: スレッドローカル最大値を比較して REL_M_MAX と位置を決定

ただし REL_M_MAX/IMACH/JMACH/KMACH は **5ステップに1度の WRITE文でのみ使用** される
（コンバージェンスモニタ出力）。並列化の正確性よりも性能を優先し、
**手動 CRITICAL セクション**で最大値+位置を更新する方式が簡潔。

### 実装: パターン A（PARALLEL DO + CRITICAL for MAX tracking）

```fortran
      REL_M_MAX = 0.0
      GANOW     = GA
C$OMP PARALLEL DEFAULT(NONE)
C$OMP&PRIVATE(I,J,K,GANOW,RONOW,WRELSQ,WREL,WSOUND,RELMACH,
C$OMP&FACSAFE,ROVTREL,T_REL_M_MAX,T_IMACH,T_JMACH,T_KMACH)
C$OMP&SHARED(KM,JM,IM,IFGAS,GA,GA_PV,RO,ROLIM,
C$OMP&VX,VR,WT,P,MACHLIM,ROVX,ROVR,ROWT,ROVT,RORVT,UBLADE,R)
C$OMP&SHARED(REL_M_MAX,IMACH,JMACH,KMACH)
      T_REL_M_MAX = 0.0
      T_IMACH = 0
      T_JMACH = 0
      T_KMACH = 0
      GANOW = GA
C$OMP DO SCHEDULE(STATIC)
      DO 7100 K=1,KM
      DO 7100 J=1,JM
      DO 7100 I=1,IM
      IF(IFGAS.EQ.3) GANOW = GA_PV(I,J,K)
      IF(RO(I,J,K).LT.ROLIM) RO(I,J,K) = ROLIM
      RONOW       = RO(I,J,K)
      WRELSQ      = VX(I,J,K)*VX(I,J,K) + VR(I,J,K)*VR(I,J,K)
     &            + WT(I,J,K)*WT(I,J,K)
      WREL        = SQRT(WRELSQ)
      WSOUND      = SQRT(GANOW*P(I,J,K)/RONOW)
      RELMACH     = WREL/WSOUND
      IF(RELMACH.GT.T_REL_M_MAX) THEN
         T_REL_M_MAX = RELMACH
         T_IMACH  = I
         T_JMACH  = J
         T_KMACH  = K
      END IF
      FACSAFE     = MACHLIM/RELMACH
      IF(FACSAFE.LT.1.0) THEN
           ROVX(I,J,K) = RONOW*VX(I,J,K) * FACSAFE
           ROVR(I,J,K) = RONOW*VR(I,J,K) * FACSAFE
           ROVTREL     = RONOW*WT(I,J,K) * FACSAFE
           ROWT(I,J,K) = ROVTREL
           ROVT(I,J,K) = ROVTREL + RONOW*UBLADE(J,K)
           RORVT(I,J,K)= ROVT(I,J,K)*R(J,K)
      END IF
 7100 CONTINUE
C$OMP END DO
C$OMP CRITICAL
      IF(T_REL_M_MAX.GT.REL_M_MAX) THEN
         REL_M_MAX = T_REL_M_MAX
         IMACH = T_IMACH
         JMACH = T_JMACH
         KMACH = T_KMACH
      END IF
C$OMP END CRITICAL
C$OMP END PARALLEL
```

### 変数分類

| 変数 | 分類 | 理由 |
|---|---|---|
| I, J, K | PRIVATE | ループカウンタ |
| GANOW, RONOW | PRIVATE | ループ内一時変数 |
| WRELSQ, WREL, WSOUND | PRIVATE | ループ内一時変数 |
| RELMACH, FACSAFE, ROVTREL | PRIVATE | ループ内一時変数 |
| T_REL_M_MAX | PRIVATE | スレッドローカル最大値 |
| T_IMACH, T_JMACH, T_KMACH | PRIVATE | スレッドローカル最大位置 |
| KM, JM, IM | SHARED | ループ上限 |
| IFGAS, GA, MACHLIM, ROLIM | SHARED | 定数 |
| RO, VX, VR, WT, P, GA_PV | SHARED | 入力配列（ROは点独立で更新） |
| ROVX, ROVR, ROWT, ROVT, RORVT | SHARED | 出力配列（点独立） |
| UBLADE, R | SHARED | 定数配列 |
| REL_M_MAX, IMACH, JMACH, KMACH | SHARED | CRITICAL で保護して更新 |

## 5. SIMD について

- ループ内に `SQRT` 2回 + 条件分岐あり → SIMD ベクトル化は難しい
- `IF(FACSAFE.LT.1.0)` はほとんど成立しないため、分岐予測で速い
- SIMD 適用は見送り（PARALLEL DO のみで十分）

## 6. GANOW の扱い

- `IFGAS.EQ.3` の場合のみ `GA_PV(I,J,K)` を読む
- IFGAS は翼列内で一定なので、条件が成立しなければ `GANOW = GA`（初期値固定）
- `GANOW` を PRIVATE にし、ループ先頭で `GANOW = GA` で初期化すれば安全

## 7. リスク評価

- **リスク: 低** — 各 (I,J,K) の更新は完全独立、REL_M_MAX は CRITICAL で保護
- CRITICAL の競合回数は最大スレッド数（4〜8）回のみ → オーバーヘッド無視可
- KM=64 で最外K並列 → 4スレッドで 16K/スレッド、十分な粒度

## 8. 期待効果

- OMP=4 で 1.04s → ~0.30s（70%削減）程度を期待。
- LOOP TOTAL に対して約 0.7s の短縮。
