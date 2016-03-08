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

sbt:
	$(SBT)

# Tests for rocketchip
all: 
	$(SBT) ";\
	testOnly $(PROJECT).RocketChipTests -- -DCONFIG=$(CONFIG) -DBACKEND=c -DSUITES=asm -DDEBUG=false;\
	testOnly $(PROJECT).RocketChipTests -- -DCONFIG=$(CONFIG) -DBACKEND=c -DSUITES=bmark -DDEBUG=false;\
	testOnly $(PROJECT).RocketChipTests -- -DCONFIG=$(CONFIG) -DBACKEND=v -DSUITES=asm -DDEBUG=false;\
	testOnly $(PROJECT).RocketChipTests -- -DCONFIG=$(CONFIG) -DBACKEND=v -DSUITES=bmark -DDEBUG=false"

$(addsuffix -tests, $(suites)): %-tests:
	$(SBT) "testOnly $(PROJECT).RocketChipTests -- \
	-DCONFIG=$(CONFIG) -DBACKEND=$(BACKEND) -DSUITES=$* -DDEBUG=false"

# Tests for debugging
$(addsuffix -tests-debug, $(suites)): %-tests-debug:
	$(SBT) "testOnly $(PROJECT).RocketChipTests -- \
	-DCONFIG=$(CONFIG) -DBACKEND=$(BACKEND) -DSUITES=$* -DDEBUG=true"

# RTL simulations
$(addsuffix -tests-rtl, $(suites)): %-tests-rtl:
	$(SBT) "testOnly $(PROJECT).RocketChipTests -- \
	-DCONFIG=$(CONFIG) -DBACKEND=$(BACKEND) -DSUITES=$* -DDEBUG=true -DDIR=$(vcs_sim_rtl_dir)"

# Gate-level simulations
$(addsuffix -tests-gl-syn, $(suites)): %-tests-gl-syn:
	$(SBT) "testOnly $(PROJECT).RocketChipTests -- \
	-DCONFIG=$(CONFIG) -DBACKEND=$(BACKEND) -DSUITES=$* -DDEBUG=true -DDIR=$(vcs_sim_gl_syn_dir)"

$(addsuffix -tests-gl-par, $(suites)): %-tests-gl-par:
	$(SBT) "testOnly $(PROJECT).RocketChipTests -- \
	-DCONFIG=$(CONFIG) -DBACKEND=$(BACKEND) -DSUITES=$* -DDEBUG=true -DDIR=$(vcs_sim_gl_par_dir)"

# Tests for strober-chips
$(addprefix sim-, $(addsuffix -tests, $(suites))): sim-%-tests:
	$(SBT) ";\
	testOnly $(PROJECT).SimTests -- \
	-DCONFIG=$(SIM_CONFIG) -DBACKEND=$(BACKEND) -DSUITES=$* -DDEBUG=true; \
	testOnly $(PROJECT).ReplayTests -- -DCONFIG=$(CONFIG) -DSUITES=$*"

$(addprefix nasti-, $(addsuffix -tests, $(suites))): nasti-%-tests:
	$(SBT) ";\
	testOnly $(PROJECT).NastiShimTests -- \
	-DCONFIG=$(NASTI_CONFIG) -DBACKEND=$(BACKEND) -DSUITES=$* -DDEBUG=true; \
	testOnly $(PROJECT).ReplayTests -- -DCONFIG=$(CONFIG) -DSUITES=$*"

clean:
	rm -rf test-*
