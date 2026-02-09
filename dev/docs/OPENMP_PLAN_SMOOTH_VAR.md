# OpenMP並列化方針（ID 29: TSTEP: SMOOTH_VAR）

対象: [../src/multall-open-21.3.f](../src/multall-open-21.3.f#L16862-L17109)（現行は [../src/multall-open21.3-s1.0.f](../src/multall-open21.3-s1.0.f#L16862-L17109)）

## 1) 並列化の基本方針
- 方向別スイープ（streamwise / pitch / spanwise）は**依存が強い**ため、**1方向ずつ独立に並列化**する（方向間は逐次）。
- 更新は「読み取り配列」→「一時配列（作業領域）」→「書き戻し」の**2段階**で競合を回避。
- 並列化は外側 2 次元 (`K/J` など) を主対象にし、内側 1 次元は連続アクセスを維持。
- 境界セル・特異点（LE/TE 付近など）処理は別ループに分離して分岐コストと競合を削減。

## 2) 方向別の推奨並列化粒度
- **Streamwise core**: `J/K` を `collapse(2)` で並列化し、`I` を内側に残す。
- **Pitch/Spanwise sweep**: 対象方向に直交する 2 次元を並列化し、更新方向の 1 次元は逐次。
- **Leading edge / Exit flow / Reset D** の局所処理は**並列化対象外**または専用の小ループで `static`。

## 3) 共有/私有の扱い
- ループインデックス、近傍値、一時係数は **private**。
- 物理量配列（例: `P, T, RO, VX, VR, VT` など）は **shared**。
- 一時配列は **threadprivate** またはスレッドごとに独立確保（領域競合回避）。

## 4) 競合・依存の注意点
- スイープ方向の近傍参照（`i±1` / `j±1` / `k±1`）がある場合、同一方向の同時更新は競合を起こしやすい。
- その方向は**二重バッファ（old/new）方式**にするか、**赤黒（odd/even）分割**で依存を崩す。
- `IF` 分岐で物性や境界条件が切替わる場合、**分岐ごとに別ループ**へ分割してベクトル化・SIMD を維持。

## 5) スケジューリング指針
- 基本は `schedule(static)`。
- ブロックサイズが不均一 or マスクが多い場合は `guided` を検討。

## 6) 性能見積り（粗い推定）
- 本区間は `TSTEP` 内で 11.340 s（`LOOP` 全体の約 19%）を占める。
- `8` スレッドで効率 60–80% と仮定すると、**本区間は 3–5 倍**が目安。
- Amdahl の式で全体速度を評価すると、
  - $S_{overall} \approx 1 / ((1-0.19) + 0.19/4) \approx 1.17$
  - $S_{overall} \approx 1 / ((1-0.19) + 0.19/5) \approx 1.20$
- **全体で 15–20% 程度の短縮**が現実的（他区間の並列化でさらに改善）。

## 7) 実装優先順位（推奨）
1. Streamwise core の 2D 並列化（最も支配的）
2. Pitch/Spanwise sweep の 2D 並列化
3. Reset/LE/Exit の局所処理

## 8) 実施内容と結果（2026-02-09）
- 対象: [../src/multall-open21.3-s1.0.f](../src/multall-open21.3-s1.0.f)
- 変更内容:
  - `SMOOTH_VAR` 内の Streamwise core / Pitch-Spanwise sweep などに OpenMP 指令を追加。
  - 固定形式Fortranの制約に合わせて、外側のラベル付き DO を `END DO` 形式へ変更。
  - 既存のタイマ計測点は保持。
- 実行結果（two-stg-LP-ST+steam.dat）:
  - 1スレッド: MAIN TOTAL 71.158 s、`SMOOTH_VAR` 11.253 s。
  - 2スレッド（バインド無し）: MAIN TOTAL 74.690 s、`SMOOTH_VAR` 11.778 s。
  - 2スレッド（OMP_PLACES=cores, OMP_PROC_BIND=close）: MAIN TOTAL 73.925 s、`SMOOTH_VAR` 11.502 s。
- 結論: 速度向上は確認できず、全体/`SMOOTH_VAR` ともに**改善がほぼ出ない、または悪化**したため「今回の修正はうまくいかなかった」と判断。

## 9) ベクトル化の検証（2026-02-10）
対象: [../src/multall-open21.3-s1.0.f](../src/multall-open21.3-s1.0.f#L17078-L17373)

### 9.1 Streamwise smoothing (core + boundary)
範囲: [SMOOTH_VAR: STREAMWISE CORE](../src/multall-open21.3-s1.0.f#L17102-L17157)
- ループ本体(3900): `STORE(I,J,K)` へ書き出し、`D` を読み取りのみ。I 方向に独立で依存なし。
- SIMD可否: **可**。`I` を最内ループに保持したまま `!$OMP SIMD` などで明示すると効果が出やすい。
- 注意: `collapse(3)` は I 方向の連続アクセスを崩す可能性があるため、SIMD目的なら I を最内に固定した方がよい。
- 境界補正(3905/3910): いずれも `STORE` 更新のみで依存なし。I 方向 SIMDは可だが処理量は小さい。

### 9.2 Leading edge smoothing
範囲: [SMOOTH_VAR: LEADING EDGE](../src/multall-open21.3-s1.0.f#L17166-L17193)
- `I=1,IM` のみ更新。ベクトル化しても実効が小さい。
- SIMD可否: **実用上は効果薄** (処理点が少ない)。

### 9.3 Reset D (STORE -> D)
範囲: [SMOOTH_VAR: RESET D](../src/multall-open21.3-s1.0.f#L17197-L17211)
- 単純なコピーで依存なし。
- SIMD可否: **可**。I 方向にベクトル化しやすいがメモリ帯域支配。

### 9.4 Pitchwise smoothing
範囲: [SMOOTH_VAR: PITCH/SPANWISE](../src/multall-open21.3-s1.0.f#L17226-L17319)
- ループ(9110/9120/9130): `AVG/CURVE/SCURVE` を使った計算で I 方向に依存なし。
- SIMD可否: **可**。I 方向の SIMD 付与が最も素直。
- 注意: `IND/INDLE` 分岐はループ外にあり、SIMD阻害は限定的。

### 9.5 Spanwise smoothing
範囲: [SMOOTH_VAR: PITCH/SPANWISE](../src/multall-open21.3-s1.0.f#L17226-L17319)
- ループ(9200/9210/9230): 依存はなく SIMD は論理的に可能。
- ただし `K` 方向はメモリが大きくストライドし、SIMD効率は低下しやすい。
- SIMD可否: **可(効果は限定的)**。I 方向の並列が主、SIMD は補助的。

### 9.6 Corner smoothing
範囲: [SMOOTH_VAR: PITCH/SPANWISE](../src/multall-open21.3-s1.0.f#L17226-L17319)
- 4点のみ更新。SIMDの意味なし。

### 9.7 Exit flow smoothing
範囲: [SMOOTH_VAR: EXIT FLOW](../src/multall-open21.3-s1.0.f#L17333-L17373)
- I 方向スムージング(8501): `D(I,J,K)` を **同一配列へ逐次更新**し、`D(I-1,J,K)` を参照するため **ループキャリー依存**あり。
- K 方向スムージング(8601)も同様に `D(I,J,K-1)` を参照し依存が発生。
- SIMD可否: **不可(現状)**。SIMD 化するには `D` を一時配列に退避してから書き戻す2段階化が必要。

### 9.8 まとめ
- SIMD 有望: streamwise core、reset D、pitchwise smoothing。
- SIMD 効果が薄い: leading edge、corner smoothing、spanwise smoothing(ストライド大)。
- SIMD 不可: exit flow smoothing(インプレース更新による依存)。
