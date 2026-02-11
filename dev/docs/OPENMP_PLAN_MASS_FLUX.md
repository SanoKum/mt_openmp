# OpenMP Plan: MASS FLUX 並列化

## 1. 現状分析

### 1.1 タイミング
- **MASS FLUX**: 1.70s（OMP=4、LOOP TOTAL 24.71s の 6.9%）
- **MASS FLUX RFLUX**: 0.48s（同、1.9%）
- 合計 **2.18s**（8.8%）、完全シリアル（スケーリング 1.0x）

### 1.2 コード位置
- メイン: L4409〜L4455（タイマーID 11: `T_LOOP_MASSFLUX`）
- RFLUX: L4941〜L4959（タイマーID 33: `T_LOOP_RFLUX`）

### 1.3 ループ構造

MASS FLUX セクション（ID 11）には **4つのループ** がある：

#### ループ1: DO 5020 — FLOWX 計算（L4411〜4420）
```fortran
DO 5020 K=1,KMM1      ! 63
DO 5020 J=1,JM         ! 351
DO 5020 I=1,IMM1       ! 63
  AVGRVX = ROVX(I,J,K)+ROVX(I,J,K+1)+ROVX(I+1,J,K)+ROVX(I+1,J,K+1)
  AVGRVR = ROVR(I,J,K)+ROVR(I,J,K+1)+ROVR(I+1,J,K)+ROVR(I+1,J,K+1)
  FLOWX(I,J,K) = 0.25*(AVGRVX*AQX(I,J,K) + AVGRVR*AQR(I,J,K))
  SOURCE(I,J,K) = 0.0
  XFLUX(I,J,K) = FLOWX(I,J,K)
5020 CONTINUE
```
- 反復回数: 63×351×63 ≈ **1.39M**
- 読み込み: ROVX, ROVR, AQX, AQR（各4点ステンシル）
- 書き込み: FLOWX, SOURCE, XFLUX（独立要素）
- **データ依存なし** → 完全並列化可能

#### ループ2: DO 5025 — ROWT/WT 計算（L4424〜4430）
```fortran
DO 5025 K=1,KM         ! 64
DO 5025 J=1,JM         ! 351
DO 5025 I=1,IM         ! 64
  ROWT(I,J,K) = ROVT(I,J,K) - UBLADE(J,K)*RO(I,J,K)
  WT(I,J,K)   = ROWT(I,J,K)/RO(I,J,K)
5025 CONTINUE
```
- 反復回数: 64×351×64 ≈ **1.44M**
- 点ごとに独立（ステンシルなし）
- **データ依存なし** → 完全並列化可能

#### ループ3: DO 5030 — FLOWT 計算（L4432〜4441）
```fortran
DO 5030 K=1,KMM1      ! 63
DO 5030 J=2,JM         ! 350
DO 5030 I=1,IM         ! 64
  AVGRVX = ROVX(I,J,K)+...  (4点ステンシル)
  AVGRVT = ROWT(I,J,K)+...  (4点ステンシル) ← ループ2の結果を読む
  AVGRVR = ROVR(I,J,K)+...  (4点ステンシル)
  AVGRO  = RO(I,J,K)+...    (4点ステンシル)
  FLOWT(I,J,K) = (AVGRVX*ABX(I,J,K) + AVGRVT*ABT(J,K)
                +  AVGRVR*ABR(I,J,K))*0.25
  TFLUX(I,J,K) = FLOWT(I,J,K)
5030 CONTINUE
```
- 反復回数: 63×350×64 ≈ **1.41M**
- ループ2の ROWT を読み込むため、ループ2の後に実行する必要あり
- 各要素の書き込みは独立
- **データ依存なし（ループ2完了後なら）** → 並列化可能

#### ループ4: DO 5050/5051 — Cusp バランス（L4444〜4453）
```fortran
DO 5050 J=2,JM
  IF(IND(J).EQ.1) GO TO 5050
  DO 5051 K=1,KMM1
    AVGFLUX = 0.5*(FLOWT(1,J,K) + FLOWT(IM,J,K))
    FLOWT(1,J,K) = AVGFLUX
    FLOWT(IM,J,K) = AVGFLUX
    TFLUX(1,J,K) = AVGFLUX
    TFLUX(IM,J,K) = AVGFLUX
  5051 CONTINUE
5050 CONTINUE
```
- I=1 と I=IM のみ書き込み → J ごとに独立
- `IF(IND(J).EQ.1) GO TO 5050` → 翼列内部の J のみ処理
- ループ3の FLOWT を読み込むため、ループ3の後に実行する必要あり
- **J 方向並列化可能**（各 J の I=1,IM は異なる J で衝突しない）

#### RFLUX セクション: DO 5040（L4952〜4958）
```fortran
DO 5040 K=K1,K2        ! 62 (or 2 for Q3D)
DO 5040 J=2,JM         ! 350
DO 5040 I=1,IMM1       ! 63
  AVGRVX = ROVX(I,J,K)+ROVX(I,J-1,K)+ROVX(I+1,J-1,K)+ROVX(I+1,J,K)
  AVGRVR = ROVR(I,J,K)+ROVR(I,J-1,K)+ROVR(I+1,J-1,K)+ROVR(I+1,J,K)
  FLOWR(I,J,K) = 0.25*(AVGRVX*ASX(I,J,K)+AVGRVR*ASR(I,J,K))
  RFLUX(I,J,K) = FLOWR(I,J,K)
5040 CONTINUE
```
- 反復回数: 62×350×63 ≈ **1.37M**
- **データ依存なし** → 完全並列化可能

---

## 2. 並列化の課題

### 2.1 データ依存関係

```
DO 5020 (FLOWX)  ── 独立 ──┐
DO 5025 (ROWT/WT) ── 独立 ──┤  並列実行可能
                            │
            barrier ────────┘
                            │
DO 5030 (FLOWT)  ── ROWT読む ┐
                             │
            barrier ─────────┘
                             │
DO 5050 (Cusp)   ── FLOWT読む
```

- DO 5020 と DO 5025 は**相互に独立** → 同一 PARALLEL 領域内で並列OK
- DO 5030 は DO 5025 の ROWT が必要 → barrier 後に実行
- DO 5050 は DO 5030 の FLOWT が必要 → barrier 後に実行
- DO 5040 (RFLUX) はタイマー上別だが、いずれとも独立

### 2.2 スカラー変数の分類

すべてのスカラー（AVGRVX, AVGRVR, AVGRVT, AVGRO, AVGFLUX）は
ループ本体内で代入→使用の完結パターンで、**PRIVATE** にすれば安全。

### 2.3 GO TO の処理

DO 5050 の `IF(IND(J).EQ.1) GO TO 5050` は `CYCLE` と同等。
OpenMP DO ループ内での CYCLE は合法なので問題なし。

### 2.4 ABT(JD,KD) の次元

`ABT(J,K)` は 2D 配列（I 次元なし）。DO 5030 で `ABT(J,K)` は読み取り専用で
全スレッドが同じ値を参照 → SHARED で問題なし。

---

## 3. 実装方針

### 3.1 アプローチ: 統一 PARALLEL リージョン

VISCOUS の教訓を活かし、fork/join を最小化するため **1つの PARALLEL 領域** に
4つのループを統合する。

```fortran
C$OMP PARALLEL DEFAULT(NONE)
C$OMP&PRIVATE(I,J,K,AVGRVX,AVGRVR,AVGRVT,AVGRO,AVGFLUX)
C$OMP&PRIVATE(K1,K2)
C$OMP&SHARED(KM,KMM1,JM,IM,IMM1)
C$OMP&SHARED(ROVX,ROVR,ROVT,RO,AQX,AQR,ABX,ABT,ABR)
C$OMP&SHARED(FLOWX,FLOWT,FLOWR,XFLUX,TFLUX,RFLUX)
C$OMP&SHARED(SOURCE,ROWT,WT,UBLADE,IND)
C$OMP&SHARED(ASX,ASR)

C$OMP DO SCHEDULE(STATIC)
      DO 5020 ...    ! FLOWX (独立)
C$OMP END DO NOWAIT

C$OMP DO SCHEDULE(STATIC)
      DO 5025 ...    ! ROWT/WT (独立、5020と並行可能)
C$OMP END DO
C     ↑ implicit barrier: ROWT が 5030 で必要

C$OMP DO SCHEDULE(STATIC)
      DO 5030 ...    ! FLOWT (ROWT を読む)
C$OMP END DO
C     ↑ implicit barrier: FLOWT が 5050 で必要

C$OMP DO SCHEDULE(STATIC)
      DO 5050 ...    ! Cusp balance
C$OMP END DO NOWAIT

C$OMP END PARALLEL
```

**ポイント:**
- DO 5020 と DO 5025 は `NOWAIT` で barrier を省略し、並行実行を許可
- DO 5025 の END DO は implicit barrier を残す（DO 5030 が ROWT を必要とするため）
- DO 5030 の END DO も implicit barrier を残す（DO 5050 が FLOWT を必要とするため）
- DO 5050 は `NOWAIT` で最後の barrier を省略

### 3.2 RFLUX セクション（DO 5040）

タイマー上は別（ID 33）なので、別の PARALLEL 領域にするか、
統一領域に含めるか検討が必要。

- 間に他のタイマー区間（TFLOW, NO-INV SURF ZERO, etc.）があるため、
  統一は困難 → 独立した `C$OMP PARALLEL DO` とする。
- 呼び出し頻度: 毎ステップ（100回/100ステップ）→ fork/join 1回あたり
  0.48s/100 = 4.8ms → overhead は約1.5ms × 100 = 0.15s で許容範囲内。

### 3.3 DO 5050 の GO TO 処理

`IF(IND(J).EQ.1) GO TO 5050` は、OpenMP DO ではラベル付き DO の
GO TO CONTINUE パターンが CYCLE 相当として機能する。
ただし安全のため、以下のように書き換えることを推奨：

```fortran
      DO 5050 J=2,JM
      IF(IND(J).NE.1) THEN
        DO 5051 K=1,KMM1
          ...
        5051 CONTINUE
      END IF
 5050 CONTINUE
```

### 3.4 collapsed ループ順序

DO 5020, 5025, 5030 はすべて K-J-I の3重ループ。
最外ループ K の反復数は 63〜64 で、4スレッドに対して十分。
COLLAPSE は前回の教訓から使わず、最外 K ループの並列化のみとする。

---

## 4. 期待される性能

| セクション | 現在(s) | 期待(s) | 削減 |
|---|---|---|---|
| MASS FLUX | 1.70 | ~0.85 | -0.85s (2.0x) |
| RFLUX | 0.48 | ~0.30 | -0.18s (1.6x) |
| **合計** | **2.18** | **~1.15** | **-1.03s** |

- 4ループ×20 fork/join を 1 fork/join に → overhead 削減
- メモリバウンドの可能性あり（大量の3D配列読み書き）ので 2x が上限の目安

---

## 5. リスク

1. **メモリバウンド**: 各ループは単純な演算（加算+乗算）で帯域律速の可能性
   → MOM FLUX BUILD と同様にスケーリングが 1.3x 程度に留まる可能性
2. **DO 5050 の IND(J) スキップ**: 翼列内部の J のみ処理するため、
   負荷不均衡の可能性 → SCHEDULE(DYNAMIC) を検討
3. **NOWAIT の安全性**: DO 5020 と DO 5025 の NOWAIT は、
   両者が互いのデータを読み書きしないことが前提

---

## 6. 検証手順

1. OMP=1 でビルド・実行し、numerical output が baseline と完全一致することを確認
2. OMP=4 でビルド・実行し、性能を測定
3. stage.log を保存して比較

---

## 7. 実装順序

1. DO 5020, 5025, 5030, 5050 を統一 PARALLEL に格納
2. ビルド・テスト（OMP=4）
3. 効果を確認
4. 効果があれば DO 5040 (RFLUX) も独立 PARALLEL DO で並列化
5. 最終コミット
