FC = gfortran
SRC = multall-open21.3-s1.0.f
BIN = ../bin/multall-open21.3-s
SRC_ORIG = multall-open-21.3.f
BIN_ORIG = ../bin/multall-open21.3-original
FFLAGS = -fopenmp -O3 -ffast-math -funroll-loops -march=native -mtune=native \
	-ffixed-line-length-none -mcmodel=large
FFLAGS_ORIG = -O3 -ffast-math -funroll-loops -march=native -mtune=native \
	-ffixed-line-length-none -mcmodel=large
VECFLAGS = -fopt-info-vec-optimized=vec.log -fopt-info-vec-missed=vec.log
RUN_DIR = ../test_cases
INPUT = two-stg-LP-ST+steam.dat
OUT_OMP2 = run_smoothvar_omp2.out
STAGE_LOG = stage.log
OMP_ENV = OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE

.PHONY: build build_original run_omp2 clean

build: $(BIN)

build_original: $(BIN_ORIG)

$(BIN): $(SRC)
	$(FC) $(FFLAGS) $(VECFLAGS) -o $@ $<

$(BIN_ORIG): $(SRC_ORIG)
	$(FC) $(FFLAGS_ORIG) $(VECFLAGS) -o $@ $<

clean:
	rm -f $(BIN) $(BIN_ORIG) vec.log

run_omp2: $(BIN)
	cd $(RUN_DIR) && $(OMP_ENV) ../bin/$(notdir $(BIN)) < $(INPUT) > $(OUT_OMP2)
	cd $(RUN_DIR) && cp $(STAGE_LOG) $(STAGE_LOG).omp2
