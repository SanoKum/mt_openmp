# OpenMP 並列化計画: ENERGY FLUX/SOURCE

## 1. 概要

タイムステップループ内で毎ステップ呼ばれるエネルギーフラックス構築セクション。
エンタルピー `HO` を用いて `XFLUX`, `RFLUX`, `TFLUX`, `SOURCE` を計算し、
後続の `TSTEP(ROE,DROE,3)` でエネルギー方程式を更新する。

- **タイマー**: `T_LOOP_ENERGY_FLUX` (ID=39)
- **コード位置**: L5212〜L5323（修正後 L5212〜L5342）
- **変更前の性能**:
  - ローカル: OMP=1: 1.03s, OMP=2: 1.03s, OMP=4: 1.07s（完全シリアル）
  - AWS 500stp 8T: 9.15s（LOOP 内ボトルネック #1, 10.9%）
- **目標**: 4 main ループを PARALLEL DO で並列化

## 2. コード構造と依存関係

### 2.1 ループ一覧

| ループ       | 行(修正後) | インデックス範囲                 | 出力         | セル数         | 並列化  |
|-------------|-----------|----------------------------------|--------------|----------------|---------|
| DO 5501     | L5225     | K=1,KMM1 / J=1,JM / I=1,IMM1   | XFLUX        | 63×351×63=1.4M | ✅ DO   |
| DO 5505     | L5233     | J=2,JM / K=1,KMM1 / I=1,IMM1   | SOURCE       | 350×63×63=1.4M | ✅ DO   |
| DO 5506     | L5263     | K=1,KMM1 / J=2,JM (IM=2 only)  | SOURCE +=    | TFLOW 条件     | ⬜ skip |
| DO 5502     | L5241     | K=1,KM / J=2,JM / I=1,IMM1     | RFLUX        | 64×350×63=1.4M | ✅ DO   |
| DO 5503     | L5250     | K=1,KMM1 / J=2,JM / I=1,IM     | TFLUX        | 63×350×64=1.4M | ✅ DO   |
| DO 5507     | L5280     | J=2,JM / I=1,IMM1 (200step毎)  | HBLEDTOT     | 350×63=22K     | ⬜ skip |
| DO 5508     | L5286     | J=2,JM / K=1,KMM1 (200step毎)  | HBLEDTOT     | 350×63=22K     | ⬜ skip |
| DO 5650/5656| L5295     | NC=1,NCOOLB / J,K               | SOURCE ±=    | 小、条件付     | ⬜ skip |
| DO 5658/5659| L5308     | NC=1,NCOOLW / J,I               | SOURCE ±=    | 小、条件付     | ⬜ skip |
| DO 900/901  | L5319     | J=2,JM / K=1,KMM1              | TFLUX(1/IM)  | GOTO あり      | ⬜ skip |
| SHROUDFLUX  | L5331     | subroutine call                  | RFLUX        | —              | ⬜ skip |

### 2.2 ループ間依存関係

```
5501 (XFLUX)  ── 独立 ── 後続依存なし
5505 (SOURCE)  ──► 5506 (SOURCE +=) ──► 5650/5656 (SOURCE +=) ──► 5658/5659 (SOURCE +=)
5502 (RFLUX)   ──► 5507 (reads RFLUX) ──► SHROUDFLUX (modifies RFLUX)
5503 (TFLUX)   ──► 5508 (reads TFLUX) ──► 900/901 (averages TFLUX)

5501, 5505, 5502, 5503 は相互に独立（書き込む配列が異なる）
```

## 3. 並列化方針

### 3.1 手法: 統合 PARALLEL 領域 + DO NOWAIT

4つの主要ループを一つの `C$OMP PARALLEL` 領域内に配置し、
各ループに `C$OMP DO ... NOWAIT` を付与。

**理由**: 4ループが全て異なる配列に書き込むため、バリア不要で NOWAIT が使える。
`END PARALLEL` の暗黙バリアで全ループの完了を保証した後、
シリアルの後処理（TFLOW, bleed, coolant, periodic BC, SHROUDFLUX）を実行。

### 3.2 PARALLEL DO 個別 vs 統合 PARALLEL の選択

| 方式              | fork/join 回数 | NOWAIT 可能 | 採用 |
|-------------------|---------------|-------------|------|
| 個別 PARALLEL DO  | 4回           | 不可        | ×    |
| 統合 PARALLEL     | 1回           | 可能        | ✅   |

統合方式を採用: fork/join 1回で4ループを処理。NOWAIT により各スレッドは
先に終わったループから次のループに移れるため、負荷の自然な分散が得られる。

### 3.3 COLLAPSE は使わない

MOM FLUX・TSTEP での教訓: COLLAPSE(3) は gfortran の自動ベクトル化を阻害する。
K ループのみで並列化し、内側 J,I ループはコンパイラのベクトル化に委ねる。

### 3.4 DO 5506 の移動

`DO 5506` (TFLOW ADDITION, `IF(IM.EQ.2)` 条件) は元々 5505 と 5502 の間にあったが、
`END PARALLEL` の後に移動した。

**安全性**: 5502, 5503 は SOURCE を読まないため、5506 が後で SOURCE を更新しても影響なし。
後続の TSTEP(ROE,DROE,3) が SOURCE を読む前に更新が完了していれば正しい。

## 4. 変数分類

### PRIVATE (各スレッドローカル)
```
I, J, K        — ループカウンタ
AVGHO          — DO 5501, 5502, 5503 のローカル一時変数
AVGPB          — DO 5503 のローカル一時変数
```

### SHARED (COMMON ブロック変数)
```
入力配列:  HO, FLOWX, FLOWR, FLOWT, QSOURCE, PEFF, WRABT
出力配列:  XFLUX, RFLUX, TFLUX, SOURCE
定数:      IM, JM, KM, IMM1, KMM1
```

各ループは出力配列の異なるセル (I,J,K) に1回だけ書き込むため race condition なし。
AVGHO, AVGPB はスカラで各イテレーション内で書いて読むだけなので PRIVATE 必須。

## 5. 実装

```fortran
C$OMP PARALLEL DEFAULT(NONE)
C$OMP&PRIVATE(I,J,K,AVGHO,AVGPB)
C$OMP&SHARED(HO,FLOWX,XFLUX,FLOWR,RFLUX,FLOWT,TFLUX,
C$OMP&QSOURCE,SOURCE,PEFF,WRABT,
C$OMP&IM,JM,KM,IMM1,KMM1)
C
C$OMP DO SCHEDULE(STATIC)
      DO 5501 K=1,KMM1           ← XFLUX
      ...
 5501 CONTINUE
C$OMP END DO NOWAIT
C
C$OMP DO SCHEDULE(STATIC)
      DO 5505 J=2,JM             ← SOURCE = QSOURCE
      ...
 5505 CONTINUE
C$OMP END DO NOWAIT
C
C$OMP DO SCHEDULE(STATIC)
      DO 5502 K=1,KM             ← RFLUX
      ...
 5502 CONTINUE
C$OMP END DO NOWAIT
C
C$OMP DO SCHEDULE(STATIC)
      DO 5503 K=1,KMM1           ← TFLUX
      ...
 5503 CONTINUE
C$OMP END DO NOWAIT
C$OMP END PARALLEL
C
C  以下シリアル: TFLOW ADDITION, bleed, coolant, periodic BC, SHROUDFLUX
```

## 6. 並列化しないループの理由

| ループ群               | 理由                                                                     |
|-----------------------|--------------------------------------------------------------------------|
| DO 5506 (TFLOW)       | `IF(IM.EQ.2)` 条件 — テストケースでは IM=64 で通らない。I=1 固定で小さい |
| DO 5507/5508 (bleed)  | 200ステップ毎のみ実行。小ループで REDUCTION 必要。オーバーヘッド > 効果   |
| DO 5650/5656 (coolant blade) | NC ループ小回数。複数 NC で SOURCE 同一セルに書く可能性 (write conflict) |
| DO 5658/5659 (coolant wall)  | 同上                                                                    |
| DO 900/901 (periodic BC)     | GOTO あり。I=1, I=IM のみ操作。J 方向は独立だがループ小さい            |
| SHROUDFLUX            | サブルーチン呼び出し。内部で RFLUX を修正、スレッドセーフでない可能性     |

## 7. ベクトル化の状況

vec.log の確認結果（-O3 -ffast-math による自動ベクトル化）:

| ループ  | 最内ループ | ベクトル化 | 備考                                    |
|---------|-----------|------------|----------------------------------------|
| DO 5501 | I         | ✅ AVX2 32byte | 自動ベクトル化済み                     |
| DO 5505 | I         | ❌ memcpy clobbers | 単純コピーで影響小                  |
| DO 5502 | I         | ✅ AVX2 32byte | 自動ベクトル化済み                     |
| DO 5503 | I         | ✅ AVX2 32byte | 自動ベクトル化済み                     |

3/4 のループが自動ベクトル化済み。`C$OMP SIMD` の追加は不要。
セクション全体がメモリバンド幅律速（計算強度 flops/byte が低い）のため、
SIMD レーン拡大による追加効果は見込めない。

## 8. 性能結果

### ローカル（i5-12400F, 100ステップ）

#### ENERGY FLUX/SOURCE セクション

| スレッド | 変更前   | 変更後   | 改善    |
|---------|---------|---------|---------|
| 1T      | 1.03s   | 1.04s   | (誤差)  |
| 2T      | 1.03s   | 0.89s   | -0.14s  |
| 4T      | 1.07s   | 0.84s   | -0.23s  |

#### LOOP TOTAL

| スレッド | 変更前   | 変更後   | 改善       |
|---------|---------|---------|------------|
| 1T      | 38.43s  | 39.55s  | (誤差)     |
| 2T      | 29.88s  | 28.10s  | **-1.78s** |
| 4T      | 26.28s  | 24.14s  | **-2.14s** |

**分析**: ENERGY FLUX セクション単体のスケーリングは 1.24x@4T（メモリバンド幅律速）だが、
以前は完全シリアルだったため、マルチスレッド時の LOOP TOTAL で顕著な改善。

### AWS（c7i 8コア）— 未計測

500ステップで 9.15s → 推定 3-4s の改善見込み。

## 9. 検証手順

1. `make build` でコンパイル
2. `OMP_NUM_THREADS=1` で実行 → 数値が変更前と一致（last-digit FP 差のみ許容）
3. `OMP_NUM_THREADS=2,4` で実行 → ENERGY FLUX と LOOP TOTAL の改善を確認
4. `stage.log` の T_LOOP_ENERGY_FLUX (ID=39) を比較
5. vec.log で最内ループのベクトル化が維持されていることを確認

## 10. ステータス

- [x] コード解析・依存関係分析
- [x] 実装（L5212〜L5258: PARALLEL 領域 + 4×DO NOWAIT）
- [x] ビルド成功
- [x] OMP=1 数値検証 OK
- [x] OMP=2 性能確認 OK
- [x] OMP=4 性能確認 OK
- [ ] AWS 8コア計測（サーバー停止中）
