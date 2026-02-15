#!/bin/bash
# Full benchmark: two-stg + HP Steam, original/OMP=1/2/4/8
# Uses 'script' command to provide pseudo-tty for original binary's Y/N prompt

set -e
export OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE
TS=$(date +%Y%m%d_%H%M)
BINDIR=~/MULTALL-project/dev/bin

echo "=========================================="
echo "  PART 1: two-stg (10000stp, ~7000 converge)"
echo "  Started: $(date)"
echo "=========================================="
cd ~/MULTALL-project/dev/test_cases

echo "--- original ---"
script -qc "echo Y | $BINDIR/multall-open21.3-original < two-stg-LP-ST+steam-10000stp.dat" run_${TS}_twostg_original.out > /dev/null 2>&1 || true
cp stage.log stage.log.${TS}_twostg_original
echo "  Done: $(date)"

for N in 1 2 4 8; do
  echo "--- OMP=$N ---"
  OMP_NUM_THREADS=$N $BINDIR/multall-open21.3-s < two-stg-LP-ST+steam-10000stp.dat > run_${TS}_twostg_omp${N}.out 2>&1
  cp stage.log stage.log.${TS}_twostg_omp${N}
  echo "  Done: $(date)"
done

echo ""
echo "=========================================="
echo "  PART 2: HP Steam (9000stp, ~5700 converge)"
echo "  Started: $(date)"
echo "=========================================="
cd ~/MULTALL-project/dev/test_cases_hp_steam

echo "--- original ---"
script -qc "echo Y | $BINDIR/multall-open21.3-original < hp-steam-turb.dat" run_${TS}_hp_original.out > /dev/null 2>&1 || true
cp stage.log stage.log.${TS}_hp_original
echo "  Done: $(date)"

for N in 1 2 4 8; do
  echo "--- OMP=$N ---"
  OMP_NUM_THREADS=$N $BINDIR/multall-open21.3-s < hp-steam-turb.dat > run_${TS}_hp_omp${N}.out 2>&1
  cp stage.log stage.log.${TS}_hp_omp${N}
  echo "  Done: $(date)"
done

echo ""
echo "=========================================="
echo "  SUMMARY: LOOP: TOTAL"
echo "=========================================="
echo "--- two-stg ---"
for f in ~/MULTALL-project/dev/test_cases/stage.log.${TS}_twostg_*; do
  echo -n "$(basename $f): "
  grep '^\s*4\s' $f
done
echo "--- HP Steam ---"
for f in ~/MULTALL-project/dev/test_cases_hp_steam/stage.log.${TS}_hp_*; do
  echo -n "$(basename $f): "
  grep '^\s*4\s' $f
done
