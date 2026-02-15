#!/bin/bash
# =============================================================
# HP Steam Turbine ベンチマーク実行スクリプト
# AWS c7i (8コア) 用
# 実行場所: ~/MULTALL-project/dev/test_cases_hp_steam/
# 入力: hp-steam-turb.dat (~5686 steps to convergence)
# =============================================================

set -e

TESTDIR=~/MULTALL-project/dev/test_cases_hp_steam
BINDIR=~/MULTALL-project/dev/bin
INPUT=hp-steam-turb.dat
TIMESTAMP=$(date +%Y%m%d_%H%M)

cd "$TESTDIR"

echo "========================================="
echo " HP Steam Turbine Benchmark on $(hostname)"
echo " $(date)"
echo " CPU: $(lscpu | grep 'Model name' | sed 's/.*: *//')"
echo " Cores: $(nproc)"
echo " Input: $INPUT"
echo "========================================="

# --- Original (non-OpenMP) ---
# original バイナリは /dev/tty を開いて Y/N プロンプトを出す。
# TTY に直接接続した状態で実行し、expect で Y を自動応答する。
if [ -f "$BINDIR/multall-open21.3-original" ]; then
echo ""
echo "[1/5] Running ORIGINAL (no OpenMP)..."
echo "  NOTE: Y/N prompt will appear — type Y and press Enter."
time $BINDIR/multall-open21.3-original < $INPUT > run_original.out 2>&1
if [ -f stage.log ]; then
  cp stage.log stage.log.hp_original_${TIMESTAMP}
  echo "  -> Done. stage.log saved."
else
  echo "  -> WARNING: stage.log not generated."
fi
else
echo "[1/5] SKIP: multall-open21.3-original not found"
fi

# --- OMP 1 thread ---
echo ""
echo "[2/5] Running OMP 1 thread..."
time env OMP_NUM_THREADS=1 OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE \
  $BINDIR/multall-open21.3-s < $INPUT > run_omp1.out 2>&1
cp stage.log stage.log.hp_omp1_${TIMESTAMP}
echo "  -> Done."

# --- OMP 2 threads ---
echo ""
echo "[3/5] Running OMP 2 threads..."
time env OMP_NUM_THREADS=2 OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE \
  $BINDIR/multall-open21.3-s < $INPUT > run_omp2.out 2>&1
cp stage.log stage.log.hp_omp2_${TIMESTAMP}
echo "  -> Done."

# --- OMP 4 threads ---
echo ""
echo "[4/5] Running OMP 4 threads..."
time env OMP_NUM_THREADS=4 OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE \
  $BINDIR/multall-open21.3-s < $INPUT > run_omp4.out 2>&1
cp stage.log stage.log.hp_omp4_${TIMESTAMP}
echo "  -> Done."

# --- OMP 8 threads ---
echo ""
echo "[5/5] Running OMP 8 threads..."
time env OMP_NUM_THREADS=8 OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE \
  $BINDIR/multall-open21.3-s < $INPUT > run_omp8.out 2>&1
cp stage.log stage.log.hp_omp8_${TIMESTAMP}
echo "  -> Done."

# --- 結果サマリー ---
echo ""
echo "========================================="
echo " RESULTS SUMMARY (HP Steam Turbine)"
echo "========================================="
echo ""
echo "--- LOOP: TOTAL ---"
for f in hp_original_${TIMESTAMP} hp_omp1_${TIMESTAMP} hp_omp2_${TIMESTAMP} hp_omp4_${TIMESTAMP} hp_omp8_${TIMESTAMP}; do
  if [ -f "stage.log.$f" ]; then
    printf "  %-30s " "$f"
    grep "LOOP: TOTAL" "stage.log.$f" | tail -1 | awk '{print $NF}' || echo "(no timer)"
  fi
done

echo ""
echo "--- MAIN: TOTAL WALL TIME ---"
for f in hp_original_${TIMESTAMP} hp_omp1_${TIMESTAMP} hp_omp2_${TIMESTAMP} hp_omp4_${TIMESTAMP} hp_omp8_${TIMESTAMP}; do
  if [ -f "stage.log.$f" ]; then
    printf "  %-30s " "$f"
    grep "MAIN: TOTAL WALL TIME" "stage.log.$f" | tail -1 | awk '{print $NF}' || echo "(no timer)"
  fi
done

echo ""
echo "--- Numerical Verification (last convergence line) ---"
for f in run_original.out run_omp1.out run_omp2.out run_omp4.out run_omp8.out; do
  if [ -f "$f" ]; then
    printf "  %-20s " "$f"
    grep "EMAX" "$f" | tail -1
  fi
done

echo ""
echo "========================================="
echo " Benchmark complete: $TIMESTAMP"
echo "========================================="
