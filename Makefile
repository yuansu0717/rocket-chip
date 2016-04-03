default: all

base_dir = $(abspath .)

CONFIG       ?= DefaultFPGAConfig
SIM_CONFIG   ?= RocketSimConfig
NASTI_CONFIG ?= RocketNastiConfig
BACKEND      ?= v

include $(base_dir)/Makefrag

suites = asm bmark
vcs_sim_rtl_dir = $(base_dir)/strober-replay/vcs-sim-rtl
vcs_sim_gl_syn_dir = $(base_dir)/strober-replay/vcs-sim-gl-syn
vcs_sim_gl_par_dir = $(base_dir)/strober-replay/vcs-sim-gl-par

tests        = $(addsuffix -tests,        $(suites))
tests_debug  = $(addsuffix -tests-debug,  $(suites))
tests_rtl    = $(addsuffix -tests-rtl,    $(suites))
tests_gl_syn = $(addsuffix -tests-gl-syn, $(suites))
tests_gl_par = $(addsuffix -tests-gl-par, $(suites))
sim_tests    = $(addprefix sim-,    $(addsuffix -tests, $(suites)))
nasti_tests  = $(addprefix nasti-,  $(addsuffix -tests, $(suites)))
replay_tests = $(addprefix replay-, $(addsuffix -tests, $(suites)))

sbt:
	$(SBT)

# Tests for rocketchip
all: 
	$(SBT) ";\
	testOnly $(PROJECT).RocketChipTests -- -DCONFIG=$(CONFIG) -DBACKEND=c -DSUITES=asm -DDEBUG=false;\
	testOnly $(PROJECT).RocketChipTests -- -DCONFIG=$(CONFIG) -DBACKEND=c -DSUITES=bmark -DDEBUG=false;\
	testOnly $(PROJECT).RocketChipTests -- -DCONFIG=$(CONFIG) -DBACKEND=v -DSUITES=asm -DDEBUG=false;\
	testOnly $(PROJECT).RocketChipTests -- -DCONFIG=$(CONFIG) -DBACKEND=v -DSUITES=bmark -DDEBUG=false"

$(tests): %-tests:
	$(SBT) "testOnly $(PROJECT).RocketChipTests -- \
	-DCONFIG=$(CONFIG) -DBACKEND=$(BACKEND) -DSUITES=$* -DDEBUG=false"

# Tests for debugging
$(tests_debug): %-tests-debug:
	$(SBT) "testOnly $(PROJECT).RocketChipTests -- \
	-DCONFIG=$(CONFIG) -DBACKEND=$(BACKEND) -DSUITES=$* -DDEBUG=true"

# RTL simulations
$(tests_rtl): %-tests-rtl:
	$(SBT) "testOnly $(PROJECT).RocketChipTests -- \
	-DCONFIG=$(CONFIG) -DBACKEND=$(BACKEND) -DSUITES=$* -DDEBUG=true -DDIR=$(vcs_sim_rtl_dir)"

# Gate-level simulations
$(tests_gl_syn): %-tests-gl-syn:
	$(SBT) "testOnly $(PROJECT).RocketChipTests -- \
	-DCONFIG=$(CONFIG) -DBACKEND=$(BACKEND) -DSUITES=$* -DDEBUG=true -DDIR=$(vcs_sim_gl_syn_dir)"

$(tests_gl_par): %-tests-gl-par:
	$(SBT) "testOnly $(PROJECT).RocketChipTests -- \
	-DCONFIG=$(CONFIG) -DBACKEND=$(BACKEND) -DSUITES=$* -DDEBUG=true -DDIR=$(vcs_sim_gl_par_dir)"

# Tests for strober-chips
$(sim_tests): sim-%-tests:
	$(SBT) ";\
	testOnly $(PROJECT).SimTests -- \
	-DCONFIG=$(SIM_CONFIG) -DBACKEND=$(BACKEND) -DSUITES=$* -DDEBUG=true; \
	testOnly $(PROJECT).ReplayTests -- -DCONFIG=$(CONFIG) -DSUITES=$*"

$(nasti_tests): nasti-%-tests:
	$(SBT) ";\
	testOnly $(PROJECT).NastiShimTests -- \
	-DCONFIG=$(NASTI_CONFIG) -DBACKEND=$(BACKEND) -DSUITES=$* -DDEBUG=true; \
	testOnly $(PROJECT).ReplayTests -- -DCONFIG=$(CONFIG) -DSUITES=$*"

$(replay_tests): replay-%-tests:
	$(SBT) "testOnly $(PROJECT).ReplayTests -- -DCONFIG=$(CONFIG) -DSUITES=$*"
clean:
	rm -rf test-*

.PHONY: all clean
.PHONY: $(tests) $(tests_debug) 
.PHONY: $(tests_rtl) $(tests_gl_syn) $(tests_gl_par) 
.PHONY: $(sim_tests) $(nasti_tests)
