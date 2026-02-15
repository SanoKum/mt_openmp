#!/bin/bash
# =============================================================
# 10000ステップ ベンチマーク実行スクリプト
# AWS c7i (8コア) 用
# 実行場所: ~/MULTALL-project/dev/test_cases/
#
# 注意: 10000ステップは1回あたり約25-80分かかるため、
#       全5回で合計3-6時間程度を見込むこと。
# =============================================================

set -e

TESTDIR=~/MULTALL-project/dev/test_cases
BINDIR=~/MULTALL-project/dev/bin
INPUT=two-stg-LP-ST+steam-10000stp.dat
TIMESTAMP=$(date +%Y%m%d_%H%M)

cd "$TESTDIR"

echo "========================================="
echo " 10000-step Benchmark on $(hostname)"
echo " $(date)"
echo " CPU: $(lscpu | grep 'Model name' | sed 's/.*: *//')"
echo " Cores: $(nproc)"
echo "========================================="

# --- Timer Original (non-OpenMP, with timers) ---
if [ -f "$BINDIR/multall-open21.3-timer" ]; then
echo ""
echo "[1/5] Running TIMER ORIGINAL (no OpenMP)..."
time script -qc "echo Y | $BINDIR/multall-open21.3-timer < $INPUT" run_timer_original.out > /dev/null 2>&1 || true
tr -d '\r' < run_timer_original.out > run_timer_original_clean.out
if [ -f stage.log ]; then
  cp stage.log stage.log.10000stp_timer_${TIMESTAMP}
  echo "  -> Done. stage.log saved."
else
  echo "  -> WARNING: stage.log not generated."
fi
else
echo "[1/5] SKIP: multall-open21.3-timer not found"
fi

# --- OMP 1 thread ---
echo ""
echo "[2/5] Running OMP 1 thread..."
time env OMP_NUM_THREADS=1 OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE \
  $BINDIR/multall-open21.3-s < $INPUT > run_omp1.out 2>&1
cp stage.log stage.log.10000stp_omp1_${TIMESTAMP}
echo "  -> Done."

# --- OMP 2 threads ---
echo ""
echo "[3/5] Running OMP 2 threads..."
time env OMP_NUM_THREADS=2 OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE \
  $BINDIR/multall-open21.3-s < $INPUT > run_omp2.out 2>&1
cp stage.log stage.log.10000stp_omp2_${TIMESTAMP}
echo "  -> Done."

# --- OMP 4 threads ---
echo ""
echo "[4/5] Running OMP 4 threads..."
time env OMP_NUM_THREADS=4 OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE \
  $BINDIR/multall-open21.3-s < $INPUT > run_omp4.out 2>&1
cp stage.log stage.log.10000stp_omp4_${TIMESTAMP}
echo "  -> Done."

# --- OMP 8 threads ---
echo ""
echo "[5/5] Running OMP 8 threads..."
time env OMP_NUM_THREADS=8 OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE \
  $BINDIR/multall-open21.3-s < $INPUT > run_omp8.out 2>&1
cp stage.log stage.log.10000stp_omp8_${TIMESTAMP}
echo "  -> Done."

# --- 数値検証 ---
echo ""
echo "========================================="
echo " NUMERICAL VERIFICATION"
echo "========================================="

if [ -f run_timer_original_clean.out ] && [ -f run_omp1.out ]; then
  echo ""
  echo "--- Timer Original vs OMP=1 ---"
  bash ~/MULTALL-project/dev/scripts/compare_performance.sh run_timer_original_clean.out run_omp1.out
fi

# --- 結果サマリー ---
echo ""
echo "========================================="
echo " RESULTS SUMMARY (10000 steps)"
echo "========================================="
echo ""
echo "--- LOOP: TOTAL ---"
for f in 10000stp_timer_${TIMESTAMP} 10000stp_omp1_${TIMESTAMP} 10000stp_omp2_${TIMESTAMP} 10000stp_omp4_${TIMESTAMP} 10000stp_omp8_${TIMESTAMP}; do
  if [ -f "stage.log.$f" ]; then
    printf "  %-35s " "$f"
    grep "LOOP: TOTAL" "stage.log.$f" | tail -1 | awk '{print $NF}'
  fi
done

echo ""
echo "--- MAIN: TOTAL WALL TIME ---"
for f in 10000stp_timer_${TIMESTAMP} 10000stp_omp1_${TIMESTAMP} 10000stp_omp2_${TIMESTAMP} 10000stp_omp4_${TIMESTAMP} 10000stp_omp8_${TIMESTAMP}; do
  if [ -f "stage.log.$f" ]; then
    printf "  %-35s " "$f"
    grep "MAIN: TOTAL WALL TIME" "stage.log.$f" | tail -1 | awk '{print $NF}'
  fi
done

echo ""
echo "--- EVERY 5 STEPS (convergence I/O) ---"
for f in 10000stp_timer_${TIMESTAMP} 10000stp_omp1_${TIMESTAMP} 10000stp_omp2_${TIMESTAMP} 10000stp_omp4_${TIMESTAMP} 10000stp_omp8_${TIMESTAMP}; do
  if [ -f "stage.log.$f" ]; then
    printf "  %-35s " "$f"
    grep "LOOP: EVERY 5 STEPS\|LOOP: CONV CHECK" "stage.log.$f" | tail -1 | awk '{print $NF}'
  fi
done

echo ""
echo "========================================="
echo " Benchmark complete: $TIMESTAMP"
echo " Total elapsed: see 'time' outputs above"
echo "========================================="
