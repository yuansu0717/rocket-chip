default: all

base_dir = $(abspath .)

CONFIG       ?= DefaultFPGAConfig
SIM_CONFIG   ?= RocketSimConfig
NASTI_CONFIG ?= RocketNastiConfig

include $(base_dir)/Makefrag

sbt:
	$(SBT)

# Tests for rocketchip
all: 
	$(SBT) "testOnly $(PROJECT).AllRocketChipTests -- -DCONFIG=$(CONFIG)"

asm-tests-cpp:
	$(SBT) "testOnly $(PROJECT).AsmCppTests -- -DCONFIG=$(CONFIG)"

asm-tests-v: 
	$(SBT) "testOnly $(PROJECT).AsmVerilogTests -- -DCONFIG=$(CONFIG)"

bmark-tests-cpp: 
	$(SBT) "testOnly $(PROJECT).BmarkCppTests -- -DCONFIG=$(CONFIG)"

bmark-tests-v: 
	$(SBT) "testOnly $(PROJECT).BmarVerilogTests -- -DCONFIG=$(CONFIG)"

# Tests for debugging
asm-tests-cpp-debug:
	$(SBT) "testOnly $(PROJECT).AsmCppTestsDebug -- -DCONFIG=$(CONFIG)"

asm-tests-v-debug:
	$(SBT) "testOnly $(PROJECT).AsmVerilogTestsDebug -- -DCONFIG=$(CONFIG)"

bmark-tests-cpp-debug: 
	$(SBT) "testOnly $(PROJECT).BmarkCppTestsDebug -- -DCONFIG=$(CONFIG)"

bmark-tests-v-debug: 
	$(SBT) "testOnly $(PROJECT).BmarkVerilogTestsDebug -- -DCONFIG=$(CONFIG)"

# RTL simulations
asm-tests-rtl: 
	$(SBT) "testOnly $(PROJECT).AsmRTLTests -- -DCONFIG=$(CONFIG)"

bmark-tests-rtl: 
	$(SBT) "testOnly $(PROJECT).BmarkRTLTests -- -DCONFIG=$(CONFIG)"

# Gate-level simulations
asm-tests-gl-syn: 
	$(SBT) "testOnly $(PROJECT).AsmSYNTests -- -DCONFIG=$(CONFIG)"

bmark-tests-gl-syn: 
	$(SBT) "testOnly $(PROJECT).BmarkSYNTests -- -DCONFIG=$(CONFIG)"

asm-tests-gl-par: 
	$(SBT) "testOnly $(PROJECT).AsmPARTests -- -DCONFIG=$(CONFIG)"

bmark-tests-gl-par: 
	$(SBT) "testOnly $(PROJECT).BmarkPARTests -- -DCONFIG=$(CONFIG)"

# Tests for strober-chips
sim-asm-tests-cpp: 
	$(SBT) "~;\
	testOnly $(PROJECT).SimAsmCppTests -- -DCONFIG=$(SIM_CONFIG); \
	testOnly $(PROJECT).ReplayAsmTests -- -DCONFIG=$(CONFIG)"

sim-asm-tests-v: 
	$(SBT) "~;\
	testOnly $(PROJECT).SimAsmVerilogTests -- -DCONFIG=$(SIM_CONFIG); \
	testOnly $(PROJECT).ReplayAsmTests     -- -DCONFIG=$(CONFIG)"

sim-bmark-tests-cpp:
	$(SBT) "~;\
	testOnly $(PROJECT).SimBmarkCppTests -- -DCONFIG=$(SIM_CONFIG);\
	testOnly $(PROJECT).ReplayBmarkTests -- -DCONFIG=$(CONFIG)"

sim-bmark-tests-v:
	$(SBT) "~;\
	testOnly $(PROJECT).SimBmarkVerilogTests -- -DCONFIG=$(SIM_CONFIG);\
	testOnly $(PROJECT).ReplayBmarkTests     -- -DCONFIG=$(CONFIG)"

nasti-asm-tests-cpp:
	$(SBT) "~;\
	testOnly $(PROJECT).NastiShimAsmCppTests -- -DCONFIG=$(NASTI_CONFIG);\
	testOnly $(PROJECT).ReplayAsmTests       -- -DCONFIG=$(CONFIG)"

nasti-asm-tests-v:
	$(SBT) "~;\
	testOnly $(PROJECT).NastiShimAsmVerilogTests -- -DCONFIG=$(NASTI_CONFIG);\
	testOnly $(PROJECT).ReplayAsmTests           -- -DCONFIG=$(CONFIG)"

nasti-bmark-tests-cpp: 
	$(SBT) "~;\
	testOnly $(PROJECT).NastiShimBmarkCppTests -- -DCONFIG=$(NASTI_CONFIG);\
	testOnly $(PROJECT).ReplayBmarkTests       -- -DCONFIG=$(CONFIG)"

nasti-bmark-tests-v: 
	$(SBT) "~;\
	testOnly $(PROJECT).NastiShimBmarkVerilogTests -- -DCONFIG=$(NASTI_CONFIG);\
	testOnly $(PROJECT).ReplayBmarkTests           -- -DCONFIG=$(CONFIG)"

clean:
	rm -rf test-*
