# 進捗まとめ（2026-02-09）

## 目的
- MULTALL の OpenMP 並列化と性能評価（PRESSURE/TEMP ループ）
- TABSEARCH/FINDIT 系の高速化検討
- メモリ帯域飽和の検証（プロファイラ導入）

## 実施内容
- OpenMP 化した PRESSURE/TEMP ループの性能測定（1/2/4 コア）
- `SMOOTH_VAR` の OpenMP 並列化を実装し、1/2 コアで再計測
- TABSEARCH のスレッドセーフ性確認（共有テーブルの読み取りのみ）
- FINDIT5 による 5 回呼び出しの 1 回化（FINDIT1×5 → FINDIT5）
- TAB5 パッキングによる局所性改善の試行（PACKTAB5 を CHECK_TAB 後に移動して NaN 回避）
- TABSEARCH のヒントベース局所探索の試行
- TABSEARCH の二分探索化の試行
- プロファイリングツール導入の試行（perf/likwid/VTune）

## 結果
- OpenMP は 2/4 コアで有意な高速化が出ず、ほぼ頭打ち
- `SMOOTH_VAR` の OpenMP 化は改善が出ず（2コアで全体・局所ともに悪化/微改善止まり）
- FINDIT5 は 1 コアで最速（ベースより短縮）
- TAB5 は NaN 問題を解消したが性能は悪化 → 採用せず
- ヒント探索・二分探索はいずれも遅くなり、元に戻した
- perf は WSL で利用不可、likwid は perfgroups 不足
- VTune はインストール自体は可能だが WSL カーネルヘッダ不在で計測不可

## 現在のコード状態（dev）
- [dev/src/multall-open21.3-s1.0.f](dev/src/multall-open21.3-s1.0.f)
  - PRESSURE/TEMP に OpenMP 指示あり
  - `SMOOTH_VAR` に OpenMP 指示あり
  - TABSEARCH は元の coarse→fine 探索
  - FINDIT5 方式のみ採用
- [dev/src/commall-open-21.3](dev/src/commall-open-21.3)
  - TAB5/ヒント配列/二分探索の変更なし（オリジナル相当）
- [dev/docs/OPENMP_PLAN_PRESSURE_TEMP.md](dev/docs/OPENMP_PLAN_PRESSURE_TEMP.md)
  - 並列化方針のドキュメント
- [dev/docs/OPENMP_PLAN_SMOOTH_VAR.md](dev/docs/OPENMP_PLAN_SMOOTH_VAR.md)
  - `SMOOTH_VAR` 並列化方針と結果の記録

## 未解決/次の一手
- WSL では VTune のカーネルヘッダ不足で計測不可
  - 対案: ネイティブ Linux 環境での VTune/pmu 計測
  - もしくは WSL2 カーネルヘッダの入手方法が必要

## まとめ
- 現時点での最良案は FINDIT5 のみ採用した構成
- さらなる高速化はメモリ帯域や計測環境の制約確認が必要
