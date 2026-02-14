# Daily Progress — 2026-02-14

## CELL->NODE Phase2 境界面処理の最適化と並列化

### 対象
- セクション: `T_TSTEP_CELLNODE` (ID=28), TSTEP 内の CELL->NODE gather 処理
- Phase 1 (interior): L7163〜L7180 — `C$OMP DO` 並列化済み
- Phase 2 (boundary): L7191〜L7290 — `C$OMP SINGLE` で逐次実行（ボトルネック）

### 変更内容

#### 1. Phase1/Phase2 サブタイマー追加 (ID=56, 57)
- `T_TSTEP_CN_PH1` (ID=56): interior ループ計測
- `T_TSTEP_CN_PH2` (ID=57): boundary ループ計測
- NT を 55→57 に拡張

#### 2. 境界ループの IF 除去・面分割
Phase 2 の3つのループ（K面, I面, J面）をそれぞれ2つに分割し、不要な IF 分岐を除去:

- **K面**: `DO K=1,KM,KM-1` → K=1面（FBL/FBR のみ）+ K=KM面（FTL/FTR のみ）
- **I面**: `DO I=1,IM,IM-1` → I=1面（FBL/FTL のみ）+ I=IM面（FBR/FTR のみ）
- **J面**: `DO J=1,JM,JM-1` → J=1面（FACUP のみ）+ J=JM面（FACDWN のみ）

効果: PH2 が 1.50s → 1.07s（OMP=1, 29%改善）

#### 3. Phase 2 の並列化（SINGLE→DO）
`C$OMP SINGLE` を除去し、6つの境界面ループに `C$OMP DO SCHEDULE(STATIC)` を付加。

**安全性**: 各面の D 書き込み先は完全に非重複:
- K=1面: D(I,J,**1**), K=KM面: D(I,J,**KM**)
- I=1面: D(**1**,J,K) K∈[2,KMM1], I=IM面: D(**IM**,J,K) K∈[2,KMM1]
- J=1面: D(I,**1**,K) I∈[2,IMM1] K∈[2,KMM1], J=JM面: D(I,**JM**,K) 同範囲

隅・辺は K 面が優先カバーし、I 面は K interior のみ、J 面は I・K ともに interior のみ。

#### 4. Phase1/Phase2 のインデント整理

### 性能結果（ローカル i5-12400F, 100ステップ）

#### 段階的な改善効果

| 段階 | PH2 (OMP=1) | CELL->NODE (OMP=1) |
|---|---|---|
| 変更前 (SINGLE) | 1.50s | 4.16s |
| IF 除去後 | 1.07s (-29%) | 3.32s (-20%) |
| 並列化後 | 1.07s (±0) | 3.29s (±0) |

#### スレッド別最終結果

| | OMP=1 | OMP=2 | OMP=4 |
|---|---|---|---|
| PH1 INTERIOR | 2.21s | 1.78s | 1.42s |
| PH2 BOUNDARY | 1.07s | 0.78s | 0.64s |
| CELL->NODE 合計 | 3.29s | 2.56s | 2.06s |
| LOOP: TOTAL | 58.80s | 42.55s | 31.91s |

#### 変更前（SINGLE 版）との比較

| | OMP=1 | OMP=2 | OMP=4 |
|---|---|---|---|
| PH2 変更前 | 1.50s | 1.40s | 1.49s |
| PH2 変更後 | 1.07s | 0.78s | 0.64s |
| PH2 改善 | -0.43s | -0.62s | **-0.85s** |

### 数値検証
- OMP=1 の STEP 100 収束値: EMAX=0.27796E+01, EAVG=0.13162E+00, ECONT=0.70818E+00, FLOW=0.19406E+03
- 変更前と完全一致

### 次のアクション
- AWS ベンチマーク（8コア）で CELL->NODE のスケーリングを確認
- 他の TSTEP サブセクション（MG AGG 4.63s, SMOOTH_VAR 14.75s）の最適化検討

---

## DENSITY CHECK / MACH CHECK の並列化

### 対象
- DENSITY CHECK: `T_LOOP_DNSCHECK` (ID=64), L6275〜L6283, OMP=1: 0.275s
- MACH CHECK: `T_LOOP_MACHCHECK` (ID=65), L6288〜L6323, OMP=1: 1.038s

### 変更内容

#### 1. DENSITY CHECK (ID=64) の並列化
- K ループを `C$OMP PARALLEL DO` で並列化、I ループに `C$OMP SIMD` を付加
- K 方向は完全独立（J 方向ステンシルだが K 間は非依存）
- 変数分類: PRIVATE(I,J,K,ROAVG), SHARED(KM,JM,IM,RO,ROLIM)

#### 2. MACH CHECK (ID=65) の並列化
- K ループを `C$OMP PARALLEL` + `C$OMP DO` で並列化
- REL_M_MAX/IMACH/JMACH/KMACH のグローバル最大値追跡 → スレッドローカル変数 (T_REL_M_MAX, T_IMACH, T_JMACH, T_KMACH) + `C$OMP CRITICAL` で統合
- ローカル変数 4 つを宣言追加

#### 3. OPENMP_PLAN 文書作成
- `dev/docs/OPENMP_PLAN_DENSITY_CHECK.md` — データ依存性分析・変数分類・リスク評価
- `dev/docs/OPENMP_PLAN_MACH_CHECK.md` — CRITICAL パターン・GANOW の扱い・SIMD 見送り理由

### 性能結果（ローカル i5-12400F, 100ステップ）

| セクション | OMP=1 | OMP=2 | OMP=4 | スケーリング (1→4) |
|---|---|---|---|---|
| DENSITY CHECK | 0.275s | 0.175s | 0.117s | 2.35x |
| MACH CHECK | 1.038s | 0.724s | 0.527s | 1.97x |

### 数値検証
- OMP=1/2/4 すべてで EMAX/EAVG/ECONT/FLOW が変更前と完全一致

---

## 追加タイマー (ID=66〜68) の導入

### 対象
- ID=66: `LOOP: MIXPLAN` — 混合面処理
- ID=67: `LOOP: TE SEPARATION` — TE 分離処理
- ID=68: `LOOP: CONV CHECK (5STP)` — 5 ステップごとの収束判定（8000 CONTINUE ブロック）

### 結果 (OMP=1)
- CONV CHECK (5STP): 3.27s（LOOP TOTAL の約 5.8%）→ 次の並列化候補

---

## copilot-instructions.md の改訂

### 変更内容
- §3: タイトルを「コンパイル・実行の受け入れ条件」→「コンパイル・実行の基本設定」に変更。OMP_WAIT_POLICY=PASSIVE 等の実行時設定を追加
- §4: **全面差し替え** — 旧「コード変更後の検証手順」を「並列化ワークフロー」（8ステップ）に統合
  - ステップ 1: 計算時間の細分化計測（MASTER+BARRIER パターン明記）
  - ステップ 2: OPENMP_PLAN テンプレート構造の標準化（6セクション構成）
  - ステップ 3: ユーザによる方針決定（ブラッシュアップ / 確定 / 深掘り）
  - ステップ 4: コード修正
  - ステップ 5: 計算実行と数値検証（OMP=1 の EMAX/EAVG 一致確認を明示）
  - ステップ 6: 区間別の計算時間比較
  - ステップ 7: 効果分析と次のアクション提案（revert も選択肢として明記）
  - ステップ 8: ユーザの指示
- §6: OPENMP_PLAN_*.md をファイル構成に追加

### 次のアクション
- MOM FLUX BUILD のループ融合最適化（最大残ボトルネック、OMP=4 で 5.0s）
- CONV CHECK (5STP) の並列化（3.27s @ OMP=1）
- AWS ベンチマークでの全体スケーリング確認
