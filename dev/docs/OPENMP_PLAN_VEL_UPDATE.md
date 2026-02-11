# OpenMP 並列化計画: VELOCITY UPDATE

## 1. 概要

ROVX, ROVR, RORVT（保存変数）から VX, VR, VT（物理速度）を計算するセクション。
毎タイムステップの SMOOTH/DAMP 後、FLUX 構築前に呼ばれる。

- **タイマー**: `T_LOOP_VEL` (ID=8)
- **コード位置**: L4205〜L4217
- **変更前の性能**:
  - ローカル 100stp: OMP=1: 0.62s, OMP=4: 0.64s（完全シリアル）
- **目標**: PARALLEL DO で並列化

## 2. コード構造

単一のトリプルネストループ:

```fortran
      DO 7450 K=1,KM
      DO 7450 J=2,JM
           RRNOW  = 1/R(J,K)
      DO 7450 I=1,IM
           RECIP       = 1.0/RO(I,J,K)
           VX(I,J,K)   = ROVX(I,J,K)*RECIP
           VR(I,J,K)   = ROVR(I,J,K)*RECIP
           ROVT(I,J,K) = RORVT(I,J,K)*RRNOW
           VT(I,J,K)   = ROVT(I,J,K)*RECIP
 7450 CONTINUE
```

### 依存関係分析

- **ループ間依存**: なし。各 (I,J,K) は独立。
- **ROVT の write-then-read**: 同一イテレーション内で書いて即読み — 安全。
- **RRNOW**: J ループで計算、I ループでは不変 — PRIVATE にすれば安全。
  注意: `RRNOW = 1/R(J,K)` は K-J ループの間にあり、I ループに入る前に計算される。
  K ループで並列化する場合、RRNOW は内側の J ループ内で毎回再計算されるため PRIVATE で安全。

## 3. 変数分類

### PRIVATE
```
I, J, K    — ループカウンタ
RRNOW      — 1/R(J,K)、J ごとのローカルスカラ
RECIP      — 1/RO(I,J,K)、I ごとのローカルスカラ
```

### SHARED
```
入力配列:  R (BKGEOM), RO, ROVX, ROVR, RORVT (BKPRIM)
出力配列:  VX, VR, VT (BKSEC), ROVT (BKPRIM)
定数:      IM, JM, KM
```

## 4. 並列化方針

### 手法: PARALLEL DO

```fortran
C$OMP PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)
C$OMP&PRIVATE(I,J,K,RRNOW,RECIP)
C$OMP&SHARED(R,RO,ROVX,ROVR,RORVT,ROVT,VX,VR,VT,IM,JM,KM)
      DO 7450 K=1,KM
      DO 7450 J=2,JM
           RRNOW  = 1/R(J,K)
      DO 7450 I=1,IM
           RECIP       = 1.0/RO(I,J,K)
           VX(I,J,K)   = ROVX(I,J,K)*RECIP
           VR(I,J,K)   = ROVR(I,J,K)*RECIP
           ROVT(I,J,K) = RORVT(I,J,K)*RRNOW
           VT(I,J,K)   = ROVT(I,J,K)*RECIP
 7450 CONTINUE
C$OMP END PARALLEL DO
```

### 設計判断

- **PARALLEL DO（個別）**: ループが1つだけなので統合 PARALLEL 領域は不要。
- **SCHEDULE(STATIC)**: K=1..KM(64) を均等分割。4T で 16 iterations/thread。
- **COLLAPSE 不使用**: 最内 I ループのベクトル化を維持するため K のみで並列化。

## 5. 期待効果

- セクション単体: 0.64s → 0.3〜0.4s（メモリバンド幅律速のため 1.5〜2x 程度）
- LOOP TOTAL: 0.2〜0.3s の改善
- fork/join オーバーヘッド: 1回 × ~10μs × 100steps = ~0.001s（無視可能）

## 6. リスク

- リスクは極めて低い。単一ループ、条件分岐なし、全セル独立。
- メモリバンド幅律速のため、flux 系と同様にスケーリングは 1.2〜1.5x に留まる可能性。

## 7. 検証手順

1. `make build` でコンパイル
2. `OMP_NUM_THREADS=1` で実行 → 数値一致確認
3. `OMP_NUM_THREADS=2,4` で実行 → VELOCITY UPDATE タイマー比較
4. vec.log で最内 I ループのベクトル化維持を確認

## 8. ステータス

- [x] コード解析・依存関係分析
- [ ] 実装
- [ ] ビルド成功
- [ ] OMP=1 数値検証
- [ ] OMP=2/4 性能確認
