# OpenMP 並列化計画: MOMENTUM FLUX BUILD

## 1. 概要

タイムステップループ内で毎ステップ呼ばれるフラックス構築セクション。
3つの運動量方向（Axial / Tangential / Radial）に対してそれぞれ XFLUX, TFLUX, RFLUX, SOURCE を計算する。

- **現状タイマー**: `T_LOOP_MOM_FLUX` (ID=40)
- **現状性能**: OMP=1: 4.03s, OMP=4: 4.22s（完全シリアル、スレッド増加で微増）
- **目標**: PARALLEL DO で主要ループを並列化し、OMP=4 で 1.5〜2.0s を目指す

## 2. コード位置と構造

3方向それぞれが独立したブロック（TIMER_START/STOP で囲まれている）。

### 2.1 Axial Momentum (ROVX): L5813〜L5902

| ループ       | 行    | 範囲                              | 出力          | セル数          | 並列化            |
|--------------|-------|-----------------------------------|---------------|-----------------|-------------------|
| DO 6100      | L5814 | K=1,KMM1 / J=1,JM / I=1,IMM1    | XFLUX, SOURCE | 63×351×63=1.4M  | ✅ PARALLEL DO    |
| DO 6101      | L5823 | K=1,KMM1 / J=2,JM (IM=2 only)   | SOURCE +=     | TFLOW条件       | ⬜ skip (IM≠2)    |
| DO 6110      | L5833 | K=1,KM / J=2,JM / I=1,IMM1      | RFLUX         | 64×350×63=1.4M  | ✅ PARALLEL DO    |
| DO 6120      | L5841 | K=1,KMM1 / J=2,JM / I=1,IM      | TFLUX         | 63×350×64=1.4M  | ✅ PARALLEL DO    |
| DO 6650/6656 | L5851 | coolant blades                    | SOURCE ±=     | 小、条件付      | ⬜ skip            |
| DO 6658/6659 | L5863 | coolant walls                     | SOURCE ±=     | 小、条件付      | ⬜ skip            |
| DO 6026/6027 | L5876 | periodic boundary                 | TFLUX(1/IM)   | GOTO あり       | ⬜ skip            |
| SHROUDFLUX   | L5893 | subroutine call                   | —             | —               | ⬜ skip            |

### 2.2 Tangential Momentum (RORVT): L5912〜L5996

| ループ       | 行    | 範囲                              | 出力          | セル数 | 並列化            |
|--------------|-------|-----------------------------------|---------------|--------|-------------------|
| DO 6200      | L5913 | K=1,KMM1 / J=1,JM / I=1,IMM1    | XFLUX, SOURCE | 1.4M   | ✅ PARALLEL DO    |
| DO 6201      | L5924 | TFLOW (IM=2 only)                 | SOURCE +=     | TFLOW  | ⬜ skip            |
| DO 6210      | L5932 | K=1,KMM1 / J=2,JM / I=1,IM      | TFLUX         | 1.4M   | ✅ PARALLEL DO    |
| DO 6230      | L5940 | K=1,KM / J=2,JM / I=1,IMM1      | RFLUX         | 1.4M   | ✅ PARALLEL DO    |
| DO 902/903   | L5949 | periodic boundary                 | TFLUX(1/IM)   | GOTO   | ⬜ skip            |
| DO 7750/7756 | L5957 | coolant blades                    | SOURCE ±=     | 小     | ⬜ skip            |
| DO 7758/7759 | L5968 | coolant walls                     | SOURCE ±=     | 小     | ⬜ skip            |
| SHROUDFLUX   | L5990 | subroutine call                   | —             | —      | ⬜ skip            |

### 2.3 Radial Momentum (ROVR): L6005〜L6108

| ループ       | 行    | 範囲                              | 出力      | セル数 | 並列化            |
|--------------|-------|-----------------------------------|-----------|--------|-------------------|
| DO 6300      | L6008 | K=1,KMM1 / J=1,JM / I=1,IMM1    | XFLUX     | 1.4M   | ✅ PARALLEL DO    |
| DO 6310      | L6016 | K=1,KMM1 / J=2,JM / I=1,IM      | TFLUX     | 1.4M   | ✅ PARALLEL DO    |
| DO 6320      | L6023 | K=1,KM / J=2,JM / I=1,IMM1      | RFLUX     | 1.4M   | ✅ PARALLEL DO    |
| DO 6330      | L6035 | K=1,KMM1 / J=2,JM / I=1,IMM1    | SOURCE    | 1.4M   | ✅ PARALLEL DO    |
| DO 6331      | L6047 | TFLOW (IM=2 only)                 | SOURCE += | TFLOW  | ⬜ skip            |
| DO 9650/9656 | L6055 | coolant blades                    | SOURCE ±= | 小     | ⬜ skip            |
| DO 8658/8659 | L6071 | coolant walls                     | SOURCE ±= | 小     | ⬜ skip            |
| DO 6260/6261 | L6083 | periodic boundary                 | TFLUX(1/IM) | GOTO | ⬜ skip            |
| SHROUDFLUX   | L6098 | subroutine call                   | —         | —      | ⬜ skip            |

## 3. 並列化方針

### 3.1 手法: 個別 PARALLEL DO

各ループに独立した `C$OMP PARALLEL DO` を配置する。
理由:
- ループ間に条件分岐・GOTO・サブルーチン呼び出しが挟まるため、統合 PARALLEL 領域は複雑
- TSTEP の PARALLEL 領域の**外側**にあるため、fork/join のオーバーヘッドは許容範囲
- 各ループは 1.4M セル（K=63 で K 分割しても十分な粒度）

### 3.2 対象: 10 ループ

| #  | ループ  | 方向       | K 範囲          |
|----|---------|------------|------------------|
| 1  | DO 6100 | Axial      | K=1,KMM1 (63)   |
| 2  | DO 6110 | Axial      | K=1,KM (64)     |
| 3  | DO 6120 | Axial      | K=1,KMM1 (63)   |
| 4  | DO 6200 | Tangential | K=1,KMM1 (63)   |
| 5  | DO 6210 | Tangential | K=1,KMM1 (63)   |
| 6  | DO 6230 | Tangential | K=1,KM (64)     |
| 7  | DO 6300 | Radial     | K=1,KMM1 (63)   |
| 8  | DO 6310 | Radial     | K=1,KMM1 (63)   |
| 9  | DO 6320 | Radial     | K=1,KM (64)     |
| 10 | DO 6330 | Radial     | K=1,KMM1 (63)   |

### 3.3 COLLAPSE は使わない

TSTEP での教訓: COLLAPSE(3) は gfortran の自動ベクトル化を阻害する。
K ループのみで並列化し、内側 J,I ループはコンパイラのベクトル化に委ねる。

### 3.4 変数分類

**PRIVATE** (各ループのローカル変数):
- 全ループ共通: `I`, `J`, `K`
- DO 6100: `AVGP`, `AVGVX`
- DO 6110: `AVGP`, `AVGVX`
- DO 6120: `AVGVX`, `AVGP`
- DO 6200: `AVGR`, `AVGVT`
- DO 6210: `AVGR`, `AVGVT`, `AVGP`
- DO 6230: `AVGR`, `AVGVT`
- DO 6300: `AVGVR`, `AVGP`
- DO 6310: `AVGVR`, `AVGP`
- DO 6320: `AVGVR`, `AVGP`
- DO 6330: `AVGRVT`, `AVGVT`, `AVGP`

**SHARED** (配列・定数):
- 入力配列: PEFF, VX, VT, VR, FLOWX, FLOWT, FLOWR, ROVT
- 入力配列: AQX, ASX, ABX, AQR, ASR, ABR, ABT, VOLOR
- 入力配列: XFORCE, TFORCE, RFORCE, R, RAVG_CELL
- 出力配列: XFLUX, TFLUX, RFLUX, SOURCE
- 定数: KMM1, KM, JM, IMM1, IM

注: 出力配列は各セル (I,J,K) に1回だけ書き込むので race condition なし。

## 4. 並列化しないループの理由

| ループ群                   | 理由                                                                   |
|---------------------------|------------------------------------------------------------------------|
| TFLOW (6101, 6201, 6331)  | `IF(IM.EQ.2)` 条件でこのテストケースでは通らない。I=1固定で小さい      |
| Coolant (6650等)           | NC ループは小回数、I=1/IM のみ書き込み。条件分岐あり                   |
| Periodic (6026等)          | GOTO あり、I=1/IM のみ操作。J 方向依存の可能性あり                     |
| SHROUDFLUX                 | サブルーチン呼び出し。内部構造の解析が必要                             |

## 5. 実装例

```fortran
C$OMP PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)
C$OMP&PRIVATE(I,J,K,AVGP,AVGVX)
C$OMP&SHARED(KMM1,JM,IMM1,SOURCE,XFORCE,PEFF,VX,
C$OMP&AQX,FLOWX,XFLUX)
      DO 6100 K=1,KMM1
      DO 6100 J=1,JM
      DO 6100 I=1,IMM1
      SOURCE(I,J,K) = XFORCE(I,J,K)
      AVGP  = PEFF(I,J,K)+PEFF(I,J,K+1)+PEFF(I+1,J,K+1)+PEFF(I+1,J,K)
      AVGVX = VX(I,J,K)+VX(I,J,K+1)+VX(I+1,J,K+1)+VX(I+1,J,K)
      XFLUX(I,J,K) = 0.25*(AVGP*AQX(I,J,K) + FLOWX(I,J,K)*AVGVX)
 6100 CONTINUE
C$OMP END PARALLEL DO
```

## 6. 期待効果

- 10 ループ × fork/join ≈ 各3方向に3〜4ループ = 合計約10回の fork/join
- fork/join 1回あたり ~10μs × 100 ステップ × 10 = ~0.1s のオーバーヘッド
- OMP=1 での回帰: fork/join コストのみ（~0.01s）、ベクトル化を阻害しない
- OMP=4 での期待: 4.22s → ~1.5s（60%削減）

## 7. 検証手順

1. `make build` でコンパイル
2. `OMP_NUM_THREADS=1` で実行 → OMP=1 の回帰確認（4.03s と同等か）
3. `OMP_NUM_THREADS=2` / `4` で実行 → 並列効率の確認
4. `stage.log` の T_LOOP_MOM_FLUX の値を比較
5. vec.log で内側ループのベクトル化を確認
