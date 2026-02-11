# Daily Progress — 2026-02-12

## ENERGY FLUX/SOURCE 並列化

### 対象
- セクション: `T_LOOP_ENERGY_FLUX` (ID=39), L5212〜L5258
- 変更前: 完全シリアル（OMP=4 でも 1.07s）

### 変更内容
- 4つの主要ループ (DO 5501/5505/5502/5503) を統合 `C$OMP PARALLEL` 領域で並列化
- 各ループに `C$OMP DO SCHEDULE(STATIC)` + `NOWAIT` を配置
- 4ループは異なる配列 (XFLUX/SOURCE/RFLUX/TFLUX) に書き込むため NOWAIT が安全
- `DO 5506` (TFLOW ADDITION) は `END PARALLEL` 後に移動（5502/5503 は SOURCE を読まない）
- 後続の小ループ (bleed/coolant/periodic BC/SHROUDFLUX) はシリアルのまま

### 性能結果（ローカル i5-12400F, 100ステップ）

#### ENERGY FLUX/SOURCE セクション

| スレッド | 変更前 | 変更後 | 改善 |
|---------|--------|--------|------|
| 1T | 1.03s | 1.04s | (誤差) |
| 2T | 1.03s | 0.89s | -0.14s |
| 4T | 1.07s | 0.84s | -0.23s |

#### LOOP TOTAL

| スレッド | 変更前 | 変更後 | 改善 |
|---------|--------|--------|------|
| 1T | 38.43s | 39.55s | (誤差) |
| 2T | 29.88s | 28.10s | **-1.78s** |
| 4T | 26.28s | 24.14s | **-2.14s** |

### ベクトル化状況
- DO 5501 (XFLUX): AVX2 自動ベクトル化済み ✅
- DO 5505 (SOURCE): memcpy clobbers で未ベクトル化（影響小）
- DO 5502 (RFLUX): AVX2 自動ベクトル化済み ✅
- DO 5503 (TFLUX): AVX2 自動ベクトル化済み ✅
- `C$OMP SIMD` 追加の効果なし（メモリバンド幅律速）

### flux 系セクション並列効率の横断比較

| セクション | 1T | 4T | 4Tスケーリング | 律速要因 |
|---|---|---|---|---|
| ENERGY FLUX | 1.04s | 0.84s | 1.24x | メモリバンド幅 |
| MOM FLUX BUILD | 4.25s | 3.44s | 1.24x | メモリバンド幅 |
| MASS FLUX | 1.68s | 1.32s | 1.27x | メモリバンド幅 |
| MASS FLUX RFLUX | 0.47s | 0.36s | 1.31x | メモリバンド幅 |
| VISCOUS/TURB | 3.00s | 1.35s | 2.22x | 演算律速（良好） |

flux 系は全て計算強度が低く（4-5配列 READ + 1配列 WRITE、演算は加減乗のみ）、
メモリバンド幅律速で 1.2〜1.3x が構造的な上限。

### 作成ファイル
- `dev/docs/OPENMP_PLAN_ENERGY_FLUX.md` — 並列化プラン（後追い記録）
