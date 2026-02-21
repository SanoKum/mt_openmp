#!/bin/bash
# =============================================================
# 100ステップ ベンチマーク実行スクリプト
# AWS c7i (8コア) / ローカル 両対応
# 実行場所: ~/MULTALL-project/dev/
# テストケース:
#   1. two-stg-LP-ST+steam (100stp) — IM=64, 2段タービン
#   2. steamtest+steam               — 小規模メッシュ
# =============================================================

set -e

BASEDIR=~/MULTALL-project/dev
BINDIR=$BASEDIR/bin
SCRIPTSDIR=$BASEDIR/scripts
TIMESTAMP=$(date +%Y%m%d_%H%M)

export OMP_PROC_BIND=true
export OMP_PLACES=cores
export OMP_WAIT_POLICY=PASSIVE

echo "========================================="
echo " 100-step Benchmark on $(hostname)"
echo " $(date)"
echo " CPU: $(lscpu | grep 'Model name' | sed 's/.*: *//')"
echo " Cores: $(nproc)"
echo "========================================="

# =============================================================
# PART 1: two-stg-LP-ST+steam (100ステップ)
# =============================================================
TESTDIR=$BASEDIR/test_cases/two-stg-LP-ST+steam_100stp
INPUT=two-stg-LP-ST+steam.dat

echo ""
echo "========================================="
echo " PART 1: two-stg-LP-ST+steam (100stp)"
echo "========================================="
cd "$TESTDIR"

# --- Original (non-OpenMP) ---
if [ -f "$BINDIR/multall-open21.3-original" ]; then
  echo ""
  echo "[1a] Running ORIGINAL (no OpenMP) - two-stg..."
  time script -qc "echo Y | $BINDIR/multall-open21.3-original < $INPUT" run_original_${TIMESTAMP}.out > /dev/null 2>&1 || true
  if [ -f stage.log ]; then
    cp stage.log stage.log.aws_original_${TIMESTAMP}
    echo "  -> Done."
  else
    echo "  -> WARNING: stage.log not generated. Run original manually with a terminal."
  fi
else
  echo "[1a] SKIP: multall-open21.3-original not found"
fi

# --- Original timer ---
if [ -f "$BINDIR/multall-open21.3-timer" ]; then
  echo ""
  echo "[1b] Running ORIGINAL_TIMER - two-stg..."
  time $BINDIR/multall-open21.3-timer < $INPUT > run_timer_${TIMESTAMP}.out 2>&1
  cp stage.log stage.log.aws_timer_${TIMESTAMP}
  echo "  -> Done."
else
  echo "[1b] SKIP: multall-open21.3-timer not found"
fi

# --- OMP 1, 2, 4, 8 threads ---
for N in 1 2 4 8; do
  echo ""
  echo "[1-OMP$N] Running OMP $N thread(s) - two-stg..."
  time env OMP_NUM_THREADS=$N \
    $BINDIR/multall-open21.3-s < $INPUT > run_omp${N}_${TIMESTAMP}.out 2>&1
  cp stage.log stage.log.aws_omp${N}_${TIMESTAMP}
  echo "  -> Done."
done

# =============================================================
# PART 2: steamtest+steam
# =============================================================
TESTDIR=$BASEDIR/test_cases/steamtest+steam
INPUT=steamtest+steam.dat

echo ""
echo "========================================="
echo " PART 2: steamtest+steam"
echo "========================================="
cd "$TESTDIR"

# --- Original (non-OpenMP) ---
if [ -f "$BINDIR/multall-open21.3-original" ]; then
  echo ""
  echo "[2a] Running ORIGINAL (no OpenMP) - steamtest..."
  time script -qc "echo Y | $BINDIR/multall-open21.3-original < $INPUT" run_original_${TIMESTAMP}.out > /dev/null 2>&1 || true
  if [ -f stage.log ]; then
    cp stage.log stage.log.aws_original_${TIMESTAMP}
    echo "  -> Done."
  else
    echo "  -> WARNING: stage.log not generated."
  fi
else
  echo "[2a] SKIP: multall-open21.3-original not found"
fi

# --- Original timer ---
if [ -f "$BINDIR/multall-open21.3-timer" ]; then
  echo ""
  echo "[2b] Running ORIGINAL_TIMER - steamtest..."
  time $BINDIR/multall-open21.3-timer < $INPUT > run_timer_${TIMESTAMP}.out 2>&1
  cp stage.log stage.log.aws_timer_${TIMESTAMP}
  echo "  -> Done."
else
  echo "[2b] SKIP: multall-open21.3-timer not found"
fi

# --- OMP 1, 2, 4, 8 threads ---
for N in 1 2 4 8; do
  echo ""
  echo "[2-OMP$N] Running OMP $N thread(s) - steamtest..."
  time env OMP_NUM_THREADS=$N \
    $BINDIR/multall-open21.3-s < $INPUT > run_omp${N}_${TIMESTAMP}.out 2>&1
  cp stage.log stage.log.aws_omp${N}_${TIMESTAMP}
  echo "  -> Done."
done

# =============================================================
# RESULTS SUMMARY
# =============================================================
echo ""
echo "========================================="
echo " RESULTS SUMMARY"
echo "========================================="

echo ""
echo "--- two-stg-LP-ST+steam (100stp) ---"
echo "LOOP: TOTAL:"
for f in aws_original aws_timer aws_omp1 aws_omp2 aws_omp4 aws_omp8; do
  LOG=$BASEDIR/test_cases/two-stg-LP-ST+steam_100stp/stage.log.${f}_${TIMESTAMP}
  if [ -f "$LOG" ]; then
    LOOP=$(grep "LOOP: TOTAL" "$LOG" 2>/dev/null | awk '{print $NF}')
    printf "  %-30s  %10s\n" "$f" "$LOOP"
  fi
done

echo ""
echo "--- steamtest+steam ---"
echo "LOOP: TOTAL:"
for f in aws_original aws_timer aws_omp1 aws_omp2 aws_omp4 aws_omp8; do
  LOG=$BASEDIR/test_cases/steamtest+steam/stage.log.${f}_${TIMESTAMP}
  if [ -f "$LOG" ]; then
    LOOP=$(grep "LOOP: TOTAL" "$LOG" 2>/dev/null | awk '{print $NF}')
    printf "  %-30s  %10s\n" "$f" "$LOOP"
  fi
done

echo ""
echo "--- Numerical Verification (two-stg) ---"
if [ -f "$BASEDIR/test_cases/two-stg-LP-ST+steam_100stp/run_timer_${TIMESTAMP}.out" ] && \
   [ -f "$BASEDIR/test_cases/two-stg-LP-ST+steam_100stp/run_omp1_${TIMESTAMP}.out" ]; then
  bash $SCRIPTSDIR/compare_performance.sh \
    "$BASEDIR/test_cases/two-stg-LP-ST+steam_100stp/run_timer_${TIMESTAMP}.out" \
    "$BASEDIR/test_cases/two-stg-LP-ST+steam_100stp/run_omp1_${TIMESTAMP}.out"
fi

echo ""
echo "--- Numerical Verification (steamtest) ---"
if [ -f "$BASEDIR/test_cases/steamtest+steam/run_timer_${TIMESTAMP}.out" ] && \
   [ -f "$BASEDIR/test_cases/steamtest+steam/run_omp1_${TIMESTAMP}.out" ]; then
  bash $SCRIPTSDIR/compare_performance.sh \
    "$BASEDIR/test_cases/steamtest+steam/run_timer_${TIMESTAMP}.out" \
    "$BASEDIR/test_cases/steamtest+steam/run_omp1_${TIMESTAMP}.out"
fi

echo ""
echo "All benchmarks completed at $(date)"
