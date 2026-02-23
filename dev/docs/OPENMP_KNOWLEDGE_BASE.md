# Fortran CFD ソルバ OpenMP 並列化 ナレッジベース

本ドキュメントは、Fortran 77 (fixed-form) で書かれた CFD ソルバ（MULTALL）を OpenMP で並列化した際に得られた知見を、類似コードへの適用を想定して体系的にまとめたものである。

---

## 0. 成果サマリー

| 環境 | Original | OMP=8 | 倍率 |
|---|---:|---:|---:|
| AWS c7i (8C, two-stg 100stp) | 81.1s | 13.3s | **6.09x** |
| AWS c7i (8C, two-stg 6976stp 収束) | 5159s | 839s | **6.15x** |
| AWS c7i (8C, HP Steam 5676stp 収束) | 685.7s | 123.3s | **5.56x** |

8 物理コア (Intel Xeon Platinum 8488C) で **5.5〜6.1 倍** の速度向上を達成。並列効率 69〜76%。

---

## 1. fixed-form Fortran における OpenMP の絶対ルール

### 1.1 `!$OMP` は使用禁止 — `C$OMP` のみ使用

**最重要**: fixed-form Fortran (`.f`) では `!` はコメント扱い。`!$OMP` ディレクティブは**コンパイラに完全に無視される**。

```fortran
C     ✗ 絶対禁止（ディレクティブが無効になる）
!$OMP PARALLEL DO
      DO I = 1, N
        ...
      ENDDO

C     ○ 正しい記法（列1から C$OMP）
C$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I) SHARED(N,A)
      DO I = 1, N
        ...
      ENDDO
```

**実例**: 本プロジェクトの初期段階で、全 OpenMP ディレクティブが `!$OMP` で記述されていたため、**並列化が一切機能していなかった**。`C$OMP` に全置換したことで初めて並列化が有効になり、4スレッドで PRESSURE/TEMP が 7.9s → 1.95s に改善した。

### 1.2 継続行の書き方

```fortran
C$OMP PARALLEL DEFAULT(NONE)
C$OMP&PRIVATE(I,J,K) SHARED(A,B,C)
```

列 1-3: `C$O`, 列 4-5: `MP`, 列 6: 継続文字 `&`

---

## 2. 修正項目一覧と効果

以下に、実施した全修正項目を「効果が高い順」にまとめる。

### 2.1 false sharing の除去

| 項目 | 効果 |
|---|---|
| **対象** | PRESSURE/TEMP ループ内の COMMON 変数 `TS_CNT` インクリメント |
| **問題** | 全スレッドが同一キャッシュラインを頻繁に書き換え（false sharing） |
| **修正** | TS_CNT のインクリメントを除去（計測用カウンタは不要と判断） |
| **効果** | 4スレッド: 7.92s → 2.72s（**2.9倍高速化**） |

**教訓**: PARALLEL ループ内で COMMON / MODULE / グローバル変数への**書き込み**が 1 箇所でもあると、false sharing によりスケーリングが壊滅する。パフォーマンスカウンタ・デバッグ変数は並列ループから完全に排除すること。

```fortran
C     ✗ false sharing が発生（全スレッドが同じ変数に書き込む）
C$OMP PARALLEL DO
      DO K = 1, KM
        DO J = 1, JM
          DO I = 1, IM
            TS_CNT = TS_CNT + 1    ! ← COMMON 変数
            ... 計算 ...
          ENDDO
        ENDDO
      ENDDO

C     ○ カウンタを除去
C$OMP PARALLEL DO
      DO K = 1, KM
        DO J = 1, JM
          DO I = 1, IM
            ... 計算 ...
          ENDDO
        ENDDO
      ENDDO
```

### 2.2 ヒントベース探索（テーブル補間の最適化）

| 項目 | 効果 |
|---|---|
| **対象** | TABSEARCH（蒸気性質テーブルの 2D 補間） |
| **問題** | 全格子点で毎回テーブルを先頭から線形探索（150×150 要素） |
| **修正** | THREADPRIVATE 変数に前回の探索位置を記憶し、次回はそこから探索 |
| **効果** | 1スレッド: 7.96s → 5.18s（**35%高速化**） |

**原理**: CFD では隣接格子点の物理量は連続的に変化するため、テーブル上の位置もほぼ同じセルに落ちる。前回位置からの探索でほぼ即座にヒットする。

```fortran
C     THREADPRIVATE でスレッドごとにヒント位置を保持
      INTEGER IFOUND_HINT, JFOUND_HINT
      COMMON /TABHINT/ IFOUND_HINT, JFOUND_HINT
C$OMP THREADPRIVATE(/TABHINT/)

C     探索時: 前回位置から開始
      IFOUND = IFOUND_HINT
      JFOUND = JFOUND_HINT
C     前回位置が正解ならスキップ、違えばそこから前後に探索
      ...
C     探索完了後にヒントを更新
      IFOUND_HINT = IFOUND
      JFOUND_HINT = JFOUND
```

**注意**: `COLLAPSE(3)` を使うと K-J-I の割当がインターリーブされ、隣接格子点の連続性が損なわれてヒント率が低下する。最外 K のみ並列化し、内側 J-I は連続実行するのが最適。

### 2.3 scatter → gather 変換（CELL→NODE 補間）

| 項目 | 効果 |
|---|---|
| **対象** | DO 1100: セル中心値 → ノード値の補間 |
| **問題** | scatter パターン（1セルが8隣接ノードに書き込み）→ 書き込み競合で ATOMIC 必須 |
| **修正** | gather パターン（1ノードが8周囲セルから読み込み）+ interior/boundary 分離 |
| **効果** | シリアルでも 40% 高速化、並列化で OMP=8: 7.38x スケーリング |

**scatter パターンの問題点**:
1. 各セルが 8 個のノードに書き込む → ノード (I,J,K) に対して最大 8 セルが同時書き込み → **ATOMIC 必須**
2. ATOMIC は gfortran ではロックベースになり**壊滅的に遅い**（22倍遅化を実測）

**gather パターンへの変換**:
```fortran
C     ✗ scatter（並列化不可、ATOMIC が必要）
      DO K = 1, KM-1
        DO J = 1, JM-1
          DO I = 1, IM-1
            D(I,J,K)     = D(I,J,K)     + STORE(I,J,K) * FBL
            D(I+1,J,K)   = D(I+1,J,K)   + STORE(I,J,K) * FBR
            D(I,J+1,K)   = D(I,J+1,K)   + STORE(I,J,K) * FACUP
            D(I,J,K+1)   = D(I,J,K+1)   + STORE(I,J,K) * FTL
            ... (8方向)
          ENDDO
        ENDDO
      ENDDO

C     ○ gather + interior/boundary 分離
C     Phase 1: interior（IF なし、ベクトル化可能）
C$OMP DO SCHEDULE(STATIC)
      DO K = 2, KM-1
        DO J = 2, JM-1
          DO I = 2, IM-1
            D(I,J,K) = STORE(I-1,J-1,K-1)*FBL_I_J_K
     &               + STORE(I,  J-1,K-1)*FBR_I_J_K
     &               + ... (8セルから読み込み)
          ENDDO
        ENDDO
      ENDDO
C$OMP END DO

C     Phase 2: boundary（6面ごとに個別ループ、並列化可能）
C$OMP DO SCHEDULE(STATIC)
      DO J = 1, JM      ! K=1 面
        DO I = 1, IM
          D(I,J,1) = ... (存在するセルのみから gather)
        ENDDO
      ENDDO
C$OMP END DO
      ... (K=KM面, I=1面, I=IM面, J=1面, J=JM面も同様)
```

**interior/boundary 分離が鍵**: 全ノード一括ループで 8 方向の存在判定 IF を入れると、分岐コストでシリアル scatter より遅くなる。interior は IF なし、boundary は面ごとのループで IF 不要にする。

### 2.4 orphaned worksharing（SMOOTH_VAR の並列化）

| 項目 | 効果 |
|---|---|
| **対象** | SMOOTH_VAR サブルーチン（6 領域に分かれた平滑化処理） |
| **問題** | 8 個の `C$OMP PARALLEL` があり fork/join が多発 |
| **修正** | 全 `C$OMP PARALLEL` を除去し、内部の `C$OMP DO` を orphaned worksharing に変換 |
| **効果** | OMP=2: 5.19s → 4.37s（-16%）、呼び出し元の TSTEP で fork/join を 4000→500 回に削減 |

**orphaned worksharing とは**: サブルーチン内で直接 `C$OMP PARALLEL` を持たず、`C$OMP DO` のみ（orphaned）にしておく。呼び出し元で既に PARALLEL 領域が開かれていれば、その中で worksharing として動作する。

```fortran
C     呼び出し元（TSTEP）: 1つの PARALLEL 領域
C$OMP PARALLEL DEFAULT(NONE) SHARED(D)
C$OMP DO
      DO K = 1, KM     ! 他のループ
        ...
      ENDDO
C$OMP END DO
      CALL SMOOTH_VAR(D)   ! orphaned: 内部の C$OMP DO が並列実行される
C$OMP END PARALLEL

C     SMOOTH_VAR 内: PARALLEL なし、DO のみ
      SUBROUTINE SMOOTH_VAR(D)
C$OMP DO SCHEDULE(STATIC)
      DO K = 1, KM
        ...
      ENDDO
C$OMP END DO
      ...
      END
```

**利点**:
1. 呼び出しごとに fork/join が不要
2. gfortran の PARALLEL 領域内コード劣化問題を回避（後述 §3.1）
3. スタック自動配列はスレッドごとに独立 → PRIVATE 指定不要

### 2.5 PARALLEL 領域の統合（fork/join 削減）

| 項目 | 効果 |
|---|---|
| **対象** | TSTEP 内の複数ループ、MOM FLUX BUILD の 10 ループ、VISCOUS の 2 ループ |
| **問題** | 各ループに `C$OMP PARALLEL DO` → ループ数 × 呼び出し回数分の fork/join |
| **修正** | 同一方向のループを 1 つの `C$OMP PARALLEL` リージョンに統合 |
| **効果** | MOM FLUX BUILD: OMP=1 で -0.71s（fork/join コスト削減） |

```fortran
C     ✗ 個別 PARALLEL DO（fork/join が 3 回）
C$OMP PARALLEL DO PRIVATE(I,J,K)
      DO K ... (ループ 1) ... ENDDO
C$OMP PARALLEL DO PRIVATE(I,J,K)
      DO K ... (ループ 2) ... ENDDO
C$OMP PARALLEL DO PRIVATE(I,J,K)
      DO K ... (ループ 3) ... ENDDO

C     ○ 統合 PARALLEL リージョン（fork/join が 1 回）
C$OMP PARALLEL DEFAULT(NONE) PRIVATE(I,J,K) SHARED(...)
C$OMP DO SCHEDULE(STATIC)
      DO K ... (ループ 1) ... ENDDO
C$OMP END DO
C$OMP DO SCHEDULE(STATIC)
      DO K ... (ループ 2) ... ENDDO
C$OMP END DO
C$OMP DO SCHEDULE(STATIC)
      DO K ... (ループ 3) ... ENDDO
C$OMP END DO
C$OMP END PARALLEL
```

**依存関係のないループ間は `NOWAIT` で barrier 省略可能**:
```fortran
C$OMP DO SCHEDULE(STATIC)
      DO K ... (XFLUX 書き込み) ... ENDDO
C$OMP END DO NOWAIT          ! RFLUX とは書き込み先が異なる
C$OMP DO SCHEDULE(STATIC)
      DO K ... (RFLUX 書き込み) ... ENDDO
C$OMP END DO
```

### 2.6 FINAL AVG の PARALLEL 領域内への移動

| 項目 | 効果 |
|---|---|
| **対象** | TSTEP 終端の FINAL AVG（周期境界・混合面平均化） |
| **問題** | `C$OMP END PARALLEL` 後のシリアル区間で実行 → OMP=8 で 11.3s（逆スケーリング） |
| **修正** | PARALLEL 領域内に移動し、J/K ループを `C$OMP DO` で並列化 |
| **効果** | HP Steam OMP=8: 11.3s → 2.87s（**75% 削減**）、LOOP: TOTAL -14.1s |

**逆スケーリングの原因**: 並列計算で 8 コアの L1/L2 キャッシュに分散したデータを、PARALLEL 外のシリアルコードでコア 0 がアクセスする際に、リモートキャッシュ読み出しのレイテンシが発生。スレッド数が増えるほどキャッシュ分散が進み悪化する。

**教訓**: 並列ループの直後にシリアルで同じデータを触るコードがある場合、それを PARALLEL 領域内に移動するだけで大幅に改善する。

### 2.7 COLLAPSE(2) によるループ trip 数の拡大

| 項目 | 効果 |
|---|---|
| **対象** | MG AGG（マルチグリッド凝集）の B1CHG/B2CHG ループ |
| **問題** | B1CHG: K1=12, B2CHG: K2=4 → 8スレッドで負荷不均衡 |
| **修正** | `COLLAPSE(2)` で K×J の 2 重ループを統合 |
| **効果** | OMP=8: 9.04s → 7.56s（-16%） |

```fortran
C     ✗ K のみ並列化（trip 数 12 → 8スレッドで 4 スレッドが 2 回、4 スレッドが 1 回）
C$OMP DO SCHEDULE(STATIC)
      DO K1 = 1, NKB1         ! 12 iterations
        DO J1 = 1, NJB1+1     ! ~143 iterations
          ...
        ENDDO
      ENDDO

C     ○ COLLAPSE(2) で trip 数 12×143 = 1716
C$OMP DO COLLAPSE(2) SCHEDULE(STATIC)
      DO K1 = 1, NKB1
        DO J1 = 1, NJB1+1
          ...
        ENDDO
      ENDDO
```

**適用条件**: 外側ループの trip 数がスレッド数の数倍以下の場合に有効。COLLAPSE の除算/剰余コストは本体計算に対して無視可能。

### 2.8 ブランチホイスティング + SIMD（STEPUP の高速化）

| 項目 | 効果 |
|---|---|
| **対象** | STEPUP 内 DO 1100（温度 → 速度計算ループ） |
| **問題** | 最内 I ループ内に IFGAS / ITIMST の条件分岐 → ベクトル化阻害 |
| **修正** | 条件分岐を J ループ外に引き上げ、4 つの特化 I ループに分割 + `C$OMP SIMD` |
| **効果** | scalar 命令: 352 → 164（**53% 削減**）、SIMD 率: 46% → 63% |

```fortran
C     ✗ 最内ループ内の分岐（ベクトル化を阻害）
      DO J = 1, JM
        DO I = 1, IM
          IF (IFGAS.EQ.0) THEN
            T(I,J,K) = ...    ! 理想気体
          ELSE IF (IFGAS.EQ.1) THEN
            T(I,J,K) = TFROMH(...)  ! 関数呼び出し
          ELSE IF (IFGAS.EQ.3) THEN
            CALL TABSEARCH(...)     ! テーブル補間
          ENDIF
        ENDDO
      ENDDO

C     ○ J ループレベルでの分岐 + 特化 I ループ + SIMD
      DO J = 1, JM
        IF (IFGAS.EQ.0) THEN
C$OMP SIMD
          DO I = 1, IM
            T(I,J,K) = ...    ! 理想気体（分岐なし）
          ENDDO
        ELSE IF (IFGAS.EQ.1) THEN
C$OMP SIMD
          DO I = 1, IM
C           TFROMH をインライン展開
            DH = ...
            T(I,J,K) = TREF + HT1*DH + HT2*DH2 + ...
          ENDDO
        ELSE IF (IFGAS.EQ.3) THEN
          DO I = 1, IM
            CALL TABSEARCH(...)   ! テーブル補間はSIMD不可
          ENDDO
        ENDIF
      ENDDO
```

**追加テクニック**: `IF(WSQ.LT.VLIM) WSQ = VLIM` → `WSQ = MAX(WSQ, VLIM)` に変換すると、条件分岐が消えて SIMD 化が促進される。

### 2.9 SETUP 高速化（SET_XLENGTH 壁面距離計算）

| 項目 | 効果 |
|---|---|
| **対象** | SET_XLENGTH（壁面距離計算、SETUP の 87% を占有） |
| **問題** | 全格子点 (I,J,K) × 全壁面探索 → $O(N_{cell} \times N_{wall})$ の完全シリアル |
| **修正** | (A) K ループの PARALLEL DO + (B) ハブ/ケーシング距離の I 依存性除去（事前計算） |
| **効果** | OMP=8: 17.91s → 2.49s（**7.19x**、ほぼ理想スケーリング） |

**Plan B の原理**: 壁面距離の中のハブ/ケーシング距離は X(J,K), R(J,K) のみに依存し I 方向に一定。にもかかわらず IM 回同一計算が繰り返されていた。事前計算で $O(IM)$ 倍の無駄を排除。

### 2.10 Periodic boundary ループの並列化（MOM/ENERGY FLUX）

| 項目 | 効果 |
|---|---|
| **対象** | MOM FLUX/ENERGY FLUX の periodic boundary ループ |
| **問題** | `C$OMP SINGLE` 内でシリアル実行 |
| **修正** | SINGLE を分割し periodic ループを `C$OMP DO` に、coolant/shroud は SINGLE のまま |
| **効果** | MOM FLUX VX SINGLE: 0.20s → 0.05s（75% 削減） |

---

## 3. 失敗事例と教訓

### 3.1 gfortran の PARALLEL 領域内コード劣化

| 試行 | 結果 |
|---|---|
| **内容** | TSTEP 全体を単一 `C$OMP PARALLEL` で囲み、逐次コードを `C$OMP MASTER` + `C$OMP BARRIER` で保護 |
| **結果** | MG AGG が 1.5s → 3.87s（**2.5 倍悪化**） |
| **原因** | gfortran は PARALLEL 領域内のコードに対してレジスタ割当て・ベクトル化を保守的にする。SINGLE でも MASTER でも同じ |
| **判断** | **revert** |

**教訓**: gfortran では「大きな PARALLEL 領域で全てを囲む」戦略は使えない。fork/join 削減よりもコンパイラ最適化の劣化が大きい。代わりに **orphaned worksharing** を使う（§2.4）。

### 3.2 ATOMIC の壊滅的コスト

| 試行 | 結果 |
|---|---|
| **内容** | CELL→NODE (scatter) と MG AGG (粗視化) に ATOMIC 付き PARALLEL DO |
| **結果** | CELL→NODE: 3.3s → **75.5s（22 倍悪化）**、MG AGG: 1.6s → **30.0s（18 倍悪化）** |
| **原因** | gfortran の ATOMIC はハードウェア CAS 不可時にロックベース実装。8 個同時 ATOMIC による競合が壊滅的 |
| **判断** | **即時 revert** |

**教訓**: Fortran の ATOMIC は scatter パターン（特に小配列への集中書き込み）に使ってはならない。代わりに gather 変換（§2.3）を検討する。

### 3.3 配列 REDUCTION と浮動小数点丸め誤差

| 試行 | 結果 |
|---|---|
| **内容** | STEP/DAMP の DO 1501 で NR ごとのスカラー REDUCTION → 配列 REDUCTION にバッチ化 |
| **結果** | STEP/DAMP: -2.53s（19% 改善）だが、**OMP=1 同士で Power 0.1% の数値差** |
| **原因** | 配列 REDUCTION はメモリ経由 load/store、スカラーはレジスタ保持 → SIMD パターンが異なり丸め順序が変化 → DAMP フィードバックループで 5600+ ステップにわたり増幅 |
| **判断** | **revert**（基準 0.001% 未満を未達） |

**教訓**: DAMP 等のフィードバック帰還ループ内の計算は、数学的に等価でもコンパイラ最適化の違いで数値差が生じる。帰還ループ内の最適化は、ループ本体を一切変えない手法（COLLAPSE, SCHEDULE 変更等）に限定すべき。

### 3.4 バイナリサーチの逆効果

| 試行 | 結果 |
|---|---|
| **内容** | TABSEARCH の 2 段階線形サーチをバイナリサーチに置換 |
| **結果** | **逆に遅くなった** |
| **原因** | 150 要素の小配列では、分岐予測が効く線形走査の方が速い。バイナリサーチの不規則アクセスパターンは分岐予測を悪化させる |
| **判断** | **revert** |

### 3.5 ALLOCATABLE 化の間接参照コスト

| 試行 | 結果 |
|---|---|
| **内容** | COMMON ブロックの全配列を MODULE + ALLOCATABLE に変換 |
| **結果** | PRESSURE/TEMP が OMP=1 で **+44% 悪化**（steamtest）。LOOP: TOTAL は two-stg で -10% |
| **原因** | ALLOCATABLE はディスクリプタ（ポインタ + サイズ情報）経由の間接参照。TABSEARCH のように 15 配列 × 全格子点 × 全ステップのアクセスではオーバーヘッドが蓄積 |
| **判断** | **COMMON 版を正として revert** |

**教訓**: ALLOCATABLE はメモリ効率（配列サイズの動的決定）は改善するが、高頻度アクセスパスではパフォーマンスコストがある。特に MODULE 経由で多数の ALLOCATABLE 配列を参照するルーチンが全格子点×全ステップで呼ばれる場合は要注意。`-flto` (Link Time Optimization) でも改善効果は限定的。

### 3.6 AoS パッキングの無効化

| 試行 | 結果 |
|---|---|
| **内容** | 15 個のテーブル配列を `TAB_PACK(15,ITB,JTB)` に詰めてキャッシュ局所性向上 |
| **結果** | ヒントベース探索と併用時は**効果なし** |
| **原因** | ヒント探索で同じテーブルセルがキャッシュに残るため、配列の詰め替えは意味がない |

---

## 4. パターン別並列化ガイド

### 4.1 演算バウンドループ（高スケーリング期待）

**特徴**: ループ内に多数の演算（乗除算、関数呼び出し）がある。メモリアクセスに対する演算比率が高い。

**例**: PRESSURE/TEMP, VISCOUS/TURB, STEPUP

**手法**: `C$OMP PARALLEL DO` + `DEFAULT(NONE)` + 全変数の明示分類

**期待スケーリング**: 8 コアで 5〜7.5x

```fortran
C$OMP PARALLEL DO DEFAULT(NONE)
C$OMP&PRIVATE(I,J,K, ローカル変数...)
C$OMP&SHARED(配列, 定数...)
C$OMP&SCHEDULE(STATIC)
      DO K = 1, KM
        DO J = 1, JM
          DO I = 1, IM
            ... 演算密度の高い計算 ...
          ENDDO
        ENDDO
      ENDDO
C$OMP END PARALLEL DO
```

### 4.2 メモリバウンドループ（スケーリング 1.2〜1.5x に留まる）

**特徴**: 配列の読み書きが支配的で演算が少ない。4-5 配列 READ + 1 配列 WRITE、演算は加減乗のみ。

**例**: MASS FLUX, ENERGY FLUX, MOM FLUX BUILD, DELTA/STORE

**手法**: 並列化は行うがスケーリングは期待しない。SIMD による帯域幅の効率利用が主。

**実測データ（4スレッド）**:

| セクション | 1T→4T 倍率 | 律速要因 |
|---|---:|---|
| ENERGY FLUX | 1.24x | メモリバンド幅 |
| MOM FLUX BUILD | 1.24x | メモリバンド幅 |
| MASS FLUX | 1.27x | メモリバンド幅 |
| VISCOUS/TURB | 2.22x | 演算律速（比較用） |

**教訓**: メモリバウンドなループは並列化しても DDR メモリ帯域幅の上限で頭打ちになる。ただし**上流セクションの並列化によるキャッシュウォーミング効果**で、下流セクションのスケーリングが間接的に改善することがある（§4.6 参照）。

### 4.3 scatter パターン（ATOMIC 回避必須）

**特徴**: 1 要素が複数要素に分配書き込み。書き込み先が重複する可能性がある。

**手法**: **gather パターンに変換**（§2.3 参照）

**禁止**: `C$OMP ATOMIC` による scatter の並列化は gfortran で壊滅的（§3.2）。

### 4.4 REDUCTION パターン

**特徴**: ループ内でスカラー変数に加算蓄積。

```fortran
C$OMP PARALLEL DO REDUCTION(+:SUMCHG)
C$OMP&PRIVATE(I,J,K) SHARED(...)
      DO K = 1, KM
        DO J = 1, JM
          DO I = 1, IM
            SUMCHG = SUMCHG + ABS(DELTA(I,J,K))
          ENDDO
        ENDDO
      ENDDO
C$OMP END PARALLEL DO
```

**注意**: フィードバックループ内での REDUCTION は丸め順序の変化に注意（§3.3）。長時間積分で誤差が増幅する可能性がある。

### 4.5 CRITICAL パターン（グローバル最大値の追跡）

**特徴**: ループ内で最大値と対応するインデックスを追跡。

```fortran
C     スレッドローカル変数で追跡し、最後に CRITICAL で統合
      T_MAX = -1.0E30
      T_IMAX = 0
      T_JMAX = 0
      T_KMAX = 0

C$OMP PARALLEL DEFAULT(NONE) PRIVATE(I,J,K,VAL,
C$OMP&  T_MAX,T_IMAX,T_JMAX,T_KMAX) SHARED(...)
      T_MAX = -1.0E30
C$OMP DO SCHEDULE(STATIC)
      DO K = 1, KM
        DO J = 1, JM
          DO I = 1, IM
            VAL = ...
            IF (VAL .GT. T_MAX) THEN
              T_MAX = VAL
              T_IMAX = I
              T_JMAX = J
              T_KMAX = K
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C$OMP END DO
C$OMP CRITICAL
      IF (T_MAX .GT. GLOBAL_MAX) THEN
        GLOBAL_MAX = T_MAX
        GLOBAL_IMAX = T_IMAX
        GLOBAL_JMAX = T_JMAX
        GLOBAL_KMAX = T_KMAX
      ENDIF
C$OMP END CRITICAL
C$OMP END PARALLEL
```

### 4.6 上流並列化によるキャッシュウォーミング効果

実測で確認された重要な間接効果。

**現象**: TSTEP（DENSITY/ENERGY/MOMENTUM）のコードを一切変更していないのに、02-11 → 02-14 でスケーリングが ~4.5x → ~6.1x に改善。

**原因**:
- 02-11: 上流フラックス計算（MASS FLUX, MOM FLUX, ENERGY FLUX 等）がシリアル → データはコア 0 の L1/L2 に集中 → TSTEP の PARALLEL 開始時にコア 1-7 がキャッシュコールドスタート
- 02-14: 上流セクションが並列化済み → 各コアの L1/L2 に担当データが載った状態 → `SCHEDULE(STATIC)` により同じスレッドが同じ範囲を処理 → キャッシュウォーム

**条件**: `SCHEDULE(STATIC)` を全 `C$OMP DO` で統一すること。スレッドと配列範囲の対応が一貫している必要がある。

---

## 5. COMMON ブロック・EQUIVALENCE の取り扱い

### 5.1 COMMON 変数のスレッド安全性分類

| 分類 | 扱い | 例 |
|---|---|---|
| 読み取りのみ | `SHARED` でそのまま | テーブル配列、格子座標、物理定数 |
| ループ外で 1 回だけ書き換え | `SHARED` + `SINGLE` / `MASTER` で保護 | フラグ設定 |
| ループ内で書き換えるが結果は不要 | ローカル変数に置換 | VLAM/VTURB → VLAM_L/VTURB_L |
| ループ内で書き換え、結果が必要 | `THREADPRIVATE` | テーブル探索ヒント IFOUND/JFOUND |
| サンプリングカウンタ | **除去** | TS_CNT（false sharing の元凶） |

### 5.2 COMMON → MODULE 化の注意点

- MODULE + ALLOCATABLE は高頻度アクセスパスで間接参照コストが発生する（§3.5）
- COMMON の PARAMETER 固定長配列はメモリ浪費だが、コンパイル時アドレス確定で高速
- 実メッシュサイズと PARAMETER 値が近い場合、ALLOCATABLE 化の利点は少ない
- 実メッシュサイズが PARAMETER 値より大幅に小さい場合（例: IM=37 vs ID=128）、ALLOCATABLE 化でキャッシュストライドが改善する可能性はあるが、間接参照コストとのトレードオフ

---

## 6. 計測と検証の方法論

### 6.1 タイマーの配置方法

```fortran
C     シリアル区間では直接呼び出し
      CALL TIMER_START(T_SECTION_A)
      ... 計算 ...
      CALL TIMER_STOP(T_SECTION_A)

C     PARALLEL 領域内では MASTER + BARRIER で全スレッド同期
C$OMP BARRIER
C$OMP MASTER
      CALL TIMER_STOP(T_SECTION_A)
      CALL TIMER_START(T_SECTION_B)
C$OMP END MASTER
C$OMP BARRIER
```

**BARRIER が必要な理由**: なしだと他スレッドが前区間を実行中に計測が切り替わり、正確な区間時間が取れない。

### 6.2 数値検証の 2 段階チェック

1. **収束指標**: OMP=1 の EMAX/EAVG/ECONT/FLOW が変更前と一致（last-digit 差のみ許容）
2. **全体性能**: PR/eta_TT/eta_TS/Power/Flow In/Out の相対差 0.001% 未満

### 6.3 AWS ベンチマークの注意点

- マルチテナント環境で 2-5% の計測変動がある
- OMP=1 の変動が最大（単一コアの周波数スケジューリング変動）
- 同一セッション内の相対比較を基準とする
- `OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE` が必須

---

## 7. コンパイラ固有の注意点（gfortran）

| 事項 | 説明 |
|---|---|
| PARALLEL 領域内の最適化劣化 | SINGLE/MASTER 内のコードもレジスタ割当て・ベクトル化が保守的になる。orphaned worksharing で回避。 |
| ATOMIC のロック化 | ハードウェア CAS 不可時にロックベース実装。scatter パターンでは壊滅的。 |
| `-flto` の限界 | モジュール間インライン化は部分的。ALLOCATABLE ディスクリプタのホイストは不十分。副作用で他セクションが悪化することもある。 |
| auto-vectorize | `-O3` で I 方向（stride-1）ループは自動で AVX2 ベクトル化される。`C$OMP SIMD` の明示が効くのは、関数呼び出しや条件分岐がある場合のみ。 |
| K 方向ループ | stride = IM × JM × 4 bytes。K 方向 stride が L1 キャッシュ（48KB）より大きいと SIMD 化しても効果なし。 |

---

## 8. 並列化の優先度判断フレームワーク

### 8.1 セクション分類

| 分類 | スケーリング | アクション |
|---|---|---|
| 演算バウンド | 5-7.5x / 8コア | 最優先で並列化 |
| メモリバウンド | 1.2-1.5x / 4コア | 並列化するがスケーリングは期待しない |
| I/O バウンド | 1.0-1.1x | 並列化不可 |
| 逆スケーリング | < 1.0x | 原因調査（false sharing, キャッシュ分散, PARALLEL 内劣化） |

### 8.2 アムダールの法則による改善上限

シリアル区間の割合 $s$ に対し、$p$ コアでの理論上限速度は $\frac{1}{s + (1-s)/p}$。

**実例**: OMP=8 で逐次部分が 20% (17s/84s) → 理論上限 5.0x。ENERGY FLUX を並列化して逐次 → 10% にすると理論上限は 6-7x。実測は 6.09x（逐次部分 ~5%）。

### 8.3 チェックリスト：新しいループを並列化する前に

1. [ ] ループ内に COMMON / MODULE 変数への**書き込み**はないか？ → false sharing リスク
2. [ ] scatter パターン（1 要素 → 複数要素書き込み）か？ → gather 変換を検討
3. [ ] GOTO 文を含むか？ → 構造化が先
4. [ ] I/O (WRITE/READ) を含むか？ → 原則並列化禁止
5. [ ] フィードバックループ（DAMP 等）内か？ → 丸め誤差の増幅に注意
6. [ ] ループの trip 数はスレッド数の数倍以上あるか？ → 不足なら COLLAPSE 検討
7. [ ] 既に auto-vectorize されているか？ → `C$OMP SIMD` の効果は限定的

---

## 9. セクション別最終スケーリング結果

### AWS c7i (8C/16T), two-stg 10000→6976 ステップ収束

| セクション | Timer (s) | OMP=1 (s) | OMP=8 (s) | OMP=1→8x |
|---|---:|---:|---:|---:|
| LOOP: TOTAL | 5159 | 4981 | 839 | **5.94x** |
| SMOOTH_VAR | 905 | 909 | 137 | 6.64x |
| MOM FLUX BUILD | 812 | 813 | 143 | 5.68x |
| PRESSURE/TEMP | 579 | 428 | 58 | **7.38x** |
| STEP/DAMP | 477 | 361 | 60 | 6.01x |
| MOMENTUM-R | 473 | 487 | 79 | 6.13x |
| MOMENTUM-X | 472 | 486 | 81 | 6.02x |
| DENSITY | 471 | 489 | 80 | 6.13x |
| ENERGY | 469 | 483 | 77 | 6.26x |
| MOMENTUM-T | 469 | 491 | 75 | 6.54x |
| DELTA/STORE | 447 | 463 | 69 | **6.68x** |
| VISCOUS/TURB | 361 | 371 | 50 | **7.50x** |
| CELL->NODE | 346 | 309 | 45 | 6.90x |
| MASS FLUX | 275 | 284 | 44 | 6.43x |
| ENERGY FLUX | 196 | 197 | 31 | 6.38x |
| STEPUP | 175 | 38 | 5 | **7.00x** |
| MG AGG | 165 | 381 | 57 | 6.73x |
| VEL UPDATE | 96 | 99 | 14 | 7.06x |
| DENSITY CHECK | 34 | 34 | 5 | 6.65x |
| EMAX/EAVG | 21 | 21 | 3 | 7.17x |
| EVERY 5 STEPS | 210 | 73 | 19 | 3.84x |
| FINAL AVG | 9 | 10 | 20 | 0.52x (※) |

※ FINAL AVG は逆スケーリングが残る（OMP=8 で 20s）。修正は HP Steam テストケースで検証済みだが、同一ベンチマークでの再計測が未実施。

---

## 10. 変更ファイルの構成

| ファイル | 変更内容 |
|---|---|
| `multall-open21.3-s1.0.f` | 全 OpenMP 並列化（メインソース） |
| `multall-open-21.3_timer.f` | タイマー計装版 original（ベースライン計測用） |
| `multall_data.f` | MODULE 定義（ALLOCATABLE 版、バックアップとして保持） |
| `commall-open-21.3` | COMMON ブロック定義（PARAMETER 固定長、現在使用中） |
| `Makefile` | build / build_timer / build_original ターゲット |

---

*最終更新: 2026-02-24*
