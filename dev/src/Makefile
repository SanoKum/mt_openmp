FC = gfortran
SRC = multall-open21.3-s1.0.f
BIN = ../bin/multall-open21.3-s
SRC_ORIG = multall-open-21.3.f
BIN_ORIG = ../bin/multall-open21.3-original
SRC_TIMER = multall-open-21.3_timer.f
BIN_TIMER = ../bin/multall-open21.3-timer
FFLAGS = -fopenmp -O3 -ffast-math -funroll-loops -march=native -mtune=native \
	-ffixed-line-length-none -mcmodel=large
FFLAGS_ORIG = -O3 -ffast-math -funroll-loops -march=native -mtune=native \
	-ffixed-line-length-none -mcmodel=large
VECFLAGS = -fopt-info-vec-optimized=vec.log -fopt-info-vec-missed=vec.log

# Intel ifx compiler settings
FC_IFX = ifx
BIN_IFX = ../bin/multall-open21.3-s-ifx
FFLAGS_IFX = -qopenmp -O2 -xHost -fixed -extend-source -mcmodel=large
VECFLAGS_IFX = -qopt-report=3 -qopt-report-file=vec-ifx.log

# ALLOCATABLE version (backup, requires multall_data.f)
SRC_ALLOC = multall-open21.3-s1.0-alloc.f
MODULE_SRC = multall_data.f
BIN_ALLOC = ../bin/multall-open21.3-s-alloc

RUN_DIR = ../test_cases
INPUT = two-stg-LP-ST+steam.dat
OUT_OMP2 = run_smoothvar_omp2.out
STAGE_LOG = stage.log
OMP_ENV = OMP_PROC_BIND=true OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE

.PHONY: build build_original build_timer build_ifx build_alloc run_omp2 clean

build: $(BIN)

build_original: $(BIN_ORIG)

build_timer: $(BIN_TIMER)

build_alloc: $(BIN_ALLOC)

$(BIN): $(SRC) commall-open-21.3
	$(FC) $(FFLAGS) $(VECFLAGS) -o $@ $<

$(BIN_ALLOC): $(SRC_ALLOC) $(MODULE_SRC)
	$(FC) $(FFLAGS) $(VECFLAGS) -o $@ $(MODULE_SRC) $<

$(BIN_ORIG): $(SRC_ORIG)
	$(FC) $(FFLAGS_ORIG) $(VECFLAGS) -o $@ $<

$(BIN_TIMER): $(SRC_TIMER)
	$(FC) $(FFLAGS_ORIG) $(VECFLAGS) -o $@ $<

clean:
	rm -f $(BIN) $(BIN_ORIG) $(BIN_TIMER) $(BIN_IFX) $(BIN_ALLOC) vec.log vec-ifx.log

build_ifx: $(BIN_IFX)

$(BIN_IFX): $(SRC) commall-open-21.3
	$(FC_IFX) $(FFLAGS_IFX) $(VECFLAGS_IFX) -o $@ $<

run_omp2: $(BIN)
	cd $(RUN_DIR) && $(OMP_ENV) ../bin/$(notdir $(BIN)) < $(INPUT) > $(OUT_OMP2)
	cd $(RUN_DIR) && cp $(STAGE_LOG) $(STAGE_LOG).omp2
