#!/bin/bash
# =============================================================
# 5000ステップ ベンチマーク実行スクリプト
# AWS c7i (8コア) 用
# 実行場所: ~/MULTALL-project/dev/test_cases/two-stg-LP-ST+steam/
# =============================================================

set -e

TESTDIR=~/MULTALL-project/dev/test_cases/two-stg-LP-ST+steam
BINDIR=~/MULTALL-project/dev/bin
INPUT=two-stg-LP-ST+steam.dat
OMP_COMMON="OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE"

cd "$TESTDIR"

echo "========================================="
echo " 5000-step Benchmark on $(hostname)"
echo " $(date)"
echo " CPU: $(lscpu | grep 'Model name' | sed 's/.*: *//')"
echo " Cores: $(nproc)"
echo "========================================="

# --- Original (non-OpenMP) ---
echo ""
echo "[1/5] Running ORIGINAL (no OpenMP)..."
time env $BINDIR/multall-open21.3-original < $INPUT > run_original.out 2>&1
cp stage.log stage.log.5000stp_original
echo "  -> Done. Saved stage.log.5000stp_original"

# --- OMP 1 thread ---
echo ""
echo "[2/5] Running OMP 1 thread..."
time env OMP_NUM_THREADS=1 OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE \
  $BINDIR/multall-open21.3-s < $INPUT > run_omp1.out 2>&1
cp stage.log stage.log.5000stp_omp1
echo "  -> Done. Saved stage.log.5000stp_omp1"

# --- OMP 2 threads ---
echo ""
echo "[3/5] Running OMP 2 threads..."
time env OMP_NUM_THREADS=2 OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE \
  $BINDIR/multall-open21.3-s < $INPUT > run_omp2.out 2>&1
cp stage.log stage.log.5000stp_omp2
echo "  -> Done. Saved stage.log.5000stp_omp2"

# --- OMP 4 threads ---
echo ""
echo "[4/5] Running OMP 4 threads..."
time env OMP_NUM_THREADS=4 OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE \
  $BINDIR/multall-open21.3-s < $INPUT > run_omp4.out 2>&1
cp stage.log stage.log.5000stp_omp4
echo "  -> Done. Saved stage.log.5000stp_omp4"

# --- OMP 8 threads ---
echo ""
echo "[5/5] Running OMP 8 threads..."
time env OMP_NUM_THREADS=8 OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE \
  $BINDIR/multall-open21.3-s < $INPUT > run_omp8.out 2>&1
cp stage.log stage.log.5000stp_omp8
echo "  -> Done. Saved stage.log.5000stp_omp8"

# --- 結果サマリー ---
echo ""
echo "========================================="
echo " RESULTS SUMMARY (LOOP: TOTAL)"
echo "========================================="
for f in 5000stp_original 5000stp_omp1 5000stp_omp2 5000stp_omp4 5000stp_omp8; do
  LOOP=$(grep "LOOP: TOTAL" stage.log.$f | awk '{print $NF}')
  TOTAL=$(grep "MAIN: TOTAL WALL TIME" stage.log.$f | awk '{print $NF}')
  printf "  %-20s  LOOP: %10s  TOTAL: %10s\n" "$f" "$LOOP" "$TOTAL"
done

echo ""
echo "All benchmarks completed at $(date)"
