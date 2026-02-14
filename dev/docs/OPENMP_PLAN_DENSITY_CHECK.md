# OpenMP 並列化プラン: DENSITY CHECK (Timer ID 64)

## 1. 対象コード

- **ファイル**: `dev/src/multall-open21.3-s1.0.f`
- **行**: L6275〜L6283
- **タイマー**: `T_LOOP_DNSCHECK` (ID=64)
- **OMP=1 実測時間**: 0.29s（LOOP TOTAL の約 0.5%）

## 2. 現状コード

```fortran
      DO 7200 K=1,KM
      DO 7200 J=3,JM-3
      DO 7200 I=1,IM
      ROAVG = 0.25*(RO(I,J-2,K)+RO(I,J-1,K)+RO(I,J+1,K)+RO(I,J+2,K))
      IF(RO(I,J,K).LT.0.6*ROAVG) RO(I,J,K) = 0.5*(RO(I,J,K)+0.6*ROAVG)
      IF(RO(I,J,K).GT.1.5*ROAVG) RO(I,J,K) = 0.5*(RO(I,J,K)+1.5*ROAVG)
      IF(RO(I,J,K).LT.ROLIM) RO(I,J,K) = 0.5*(RO(I,J,K) + ROLIM)
 7200 CONTINUE
```

## 3. データ依存性分析

### 読み込み
- `RO(I,J-2,K)`, `RO(I,J-1,K)`, `RO(I,J+1,K)`, `RO(I,J+2,K)` — J方向ステンシル（幅5）
- `ROLIM` — スカラー定数

### 書き込み
- `RO(I,J,K)` — 条件付き更新

### 依存性
- **J 方向に依存あり**: `RO(I,J,K)` の更新が `RO(I,J+1,K)` や `RO(I,J+2,K)` の読み出しに影響する。
- ただし、J はストリーム方向で、密度勾配の「なだらかな制限」処理。実用上 J 方向の連鎖更新は発生しにくいが、**厳密にはデータ競合の可能性がある**。
- **K 方向は完全独立**: 異なる K では RO の読み書きが重ならない。
- **I 方向も完全独立**: 各 I は自身の RO(I,J,K) のみ更新。

### 安全な並列化軸
- **K（最外ループ）で並列化** → 完全にデータ独立。安全。
- J で並列化 → J方向ステンシルで隣接J間の競合あり。**不可**。
- I で並列化 → SIMD が適切。

## 4. 並列化方針

### パターン A（PARALLEL DO on K）

```fortran
C$OMP PARALLEL DO DEFAULT(NONE)
C$OMP&PRIVATE(I,J,K,ROAVG)
C$OMP&SHARED(KM,JM,IM,RO,ROLIM)
C$OMP&SCHEDULE(STATIC)
      DO 7200 K=1,KM
      DO 7200 J=3,JM-3
C$OMP SIMD
      DO 7200 I=1,IM
      ROAVG = 0.25*(RO(I,J-2,K)+RO(I,J-1,K)+RO(I,J+1,K)+RO(I,J+2,K))
      IF(RO(I,J,K).LT.0.6*ROAVG) RO(I,J,K) = 0.5*(RO(I,J,K)+0.6*ROAVG)
      IF(RO(I,J,K).GT.1.5*ROAVG) RO(I,J,K) = 0.5*(RO(I,J,K)+1.5*ROAVG)
      IF(RO(I,J,K).LT.ROLIM) RO(I,J,K) = 0.5*(RO(I,J,K) + ROLIM)
 7200 CONTINUE
C$OMP END PARALLEL DO
```

### 変数分類
| 変数 | 分類 | 理由 |
|---|---|---|
| I, J, K | PRIVATE | ループカウンタ |
| ROAVG | PRIVATE | ループ内一時変数 |
| KM, JM, IM | SHARED | ループ上限（定数） |
| RO | SHARED | 共有配列（K方向で分割、各スレッドは異なるKスライスを更新） |
| ROLIM | SHARED | 定数 |

## 5. リスク評価

- **リスク: 低** — K 方向のみの並列化でデータ競合なし。
- KM=64 なので4スレッドで 16K/スレッド → 十分な並列粒度。
- SIMD は IF 分岐があるが、条件が稀にしか成立しないためマスク付きで効率よくベクトル化される。

## 6. 期待効果

- OMP=4 で 0.29s → ~0.08s（70%削減）程度を期待。
- 絶対値は小さいが、実装が単純で安全。
