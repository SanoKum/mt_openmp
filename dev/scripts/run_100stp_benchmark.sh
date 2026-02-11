#!/bin/bash
# =============================================================
# 100ステップ ベンチマーク実行スクリプト
# AWS c7i (8コア) 用
# 実行場所: ~/MULTALL-project/dev/test_cases/
# =============================================================

set -e

TESTDIR=~/MULTALL-project/dev/test_cases
BINDIR=~/MULTALL-project/dev/bin
INPUT=two-stg-LP-ST+steam.dat
TIMESTAMP=$(date +%Y%m%d_%H%M)

cd "$TESTDIR"

echo "========================================="
echo " 100-step Benchmark on $(hostname)"
echo " $(date)"
echo " CPU: $(lscpu | grep 'Model name' | sed 's/.*: *//')"
echo " Cores: $(nproc)"
echo "========================================="

# --- Original (non-OpenMP) ---
# Note: original binary opens /dev/tty for Y/N prompt.
# Use 'script' to provide a pseudo-tty in non-interactive SSH.
if [ -f "$BINDIR/multall-open21.3-original" ]; then
echo ""
echo "[1/6] Running ORIGINAL (no OpenMP)..."
time script -qc "echo Y | $BINDIR/multall-open21.3-original < $INPUT" run_original.out > /dev/null 2>&1 || true
if [ -f stage.log ]; then
  cp stage.log stage.log.aws_original_${TIMESTAMP}
  echo "  -> Done."
else
  echo "  -> WARNING: stage.log not generated. Run original manually with a terminal."
fi
else
echo "[1/6] SKIP: multall-open21.3-original not found"
fi

# --- OMP 1 thread ---
echo ""
echo "[2/6] Running OMP 1 thread..."
time env OMP_NUM_THREADS=1 OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE \
  $BINDIR/multall-open21.3-s < $INPUT > run_omp1.out 2>&1
cp stage.log stage.log.aws_omp1_${TIMESTAMP}
echo "  -> Done."

# --- OMP 2 threads ---
echo ""
echo "[3/6] Running OMP 2 threads..."
time env OMP_NUM_THREADS=2 OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE \
  $BINDIR/multall-open21.3-s < $INPUT > run_omp2.out 2>&1
cp stage.log stage.log.aws_omp2_${TIMESTAMP}
echo "  -> Done."

# --- OMP 4 threads ---
echo ""
echo "[4/6] Running OMP 4 threads..."
time env OMP_NUM_THREADS=4 OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE \
  $BINDIR/multall-open21.3-s < $INPUT > run_omp4.out 2>&1
cp stage.log stage.log.aws_omp4_${TIMESTAMP}
echo "  -> Done."

# --- OMP 8 threads ---
echo ""
echo "[5/6] Running OMP 8 threads..."
time env OMP_NUM_THREADS=8 OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE \
  $BINDIR/multall-open21.3-s < $INPUT > run_omp8.out 2>&1
cp stage.log stage.log.aws_omp8_${TIMESTAMP}
echo "  -> Done."

# --- 結果サマリー ---
echo ""
echo "========================================="
echo " RESULTS SUMMARY"
echo "========================================="
echo ""
echo "--- LOOP: TOTAL ---"
for f in aws_original_${TIMESTAMP} aws_omp1_${TIMESTAMP} aws_omp2_${TIMESTAMP} aws_omp4_${TIMESTAMP} aws_omp8_${TIMESTAMP}; do
  if [ -f "stage.log.$f" ]; then
    LOOP=$(grep "LOOP: TOTAL" stage.log.$f | awk '{print $NF}')
    printf "  %-30s  LOOP: %10s\n" "$f" "$LOOP"
  fi
done

echo ""
echo "--- Section breakdown (OMP=1 vs OMP=8) ---"
echo ""
echo "  OMP=1:"
grep -E "^\s+[0-9]+" stage.log.aws_omp1_${TIMESTAMP} | awk '$NF+0 > 0.1 {print "    "$0}'
echo ""
echo "  OMP=8:"
grep -E "^\s+[0-9]+" stage.log.aws_omp8_${TIMESTAMP} | awk '$NF+0 > 0.1 {print "    "$0}'

echo ""
echo "All benchmarks completed at $(date)"
