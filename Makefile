default: sbt

duts = Rocket BOOM
base_dir = $(abspath .)

include $(base_dir)/Makefrag

sbt:
	$(SBT)

# Tests for rocketchip
all: $(addsuffix -asm-tests-cpp, $(duts)) $(addsuffix -asm-tests-v, $(duts)) \
	$(addsuffix -bmarks-tests-cpp, $(duts)) $(addsuffix -bmarks-tests-cpp, $(duts))

$(addsuffix -asm-tests-cpp, $(duts)): %-asm-tests-cpp:
	$(SBT) "test-only $(PROJECT).$*AsmCppTests"

$(addsuffix -asm-tests-v, $(duts)): %-asm-tests-v:
	$(SBT) "test-only $(PROJECT).$*AsmVerilogTests"

$(addsuffix -bmark-tests-cpp, $(duts)): %-bmark-tests-cpp:
	$(SBT) "test-only $(PROJECT).$*BmarkCppTests"

$(addsuffix -bmark-tests-v, $(duts)): %-bmark-tests-v:
	$(SBT) "test-only $(PROJECT).$*BmarVerilogTests"

# Tests for debugging
$(addsuffix -asm-tests-cpp-debug, $(duts)): %-asm-tests-cpp-debug:
	$(SBT) "test-only $(PROJECT).$*AsmCppTestsDebug"

$(addsuffix -asm-tests-v-debug, $(duts)): %-asm-tests-v-debug:
	$(SBT) "test-only $(PROJECT).$*AsmVerilogTestsDebug"

$(addsuffix -bmark-tests-cpp-debug, $(duts)): %-bmark-tests-cpp-debug:
	$(SBT) "test-only $(PROJECT).$*BmarkCppTestsDebug"

$(addsuffix -bmark-tests-v-debug, $(duts)): %-bmark-tests-v-debug:
	$(SBT) "test-only $(PROJECT).$*BmarkVerilogTestsDebug"

# RTL simulations
$(addsuffix -asm-tests-rtl, $(duts)): %-asm-tests-rtl:
	$(SBT) "test-only $(PROJECT).$*AsmRTLTests"

$(addsuffix -bmark-tests-rtl, $(duts)): %-bmark-tests-rtl:
	$(SBT) "test-only $(PROJECT).$*BmarkRTLTests"

# Gate-level simulations
$(addsuffix -asm-tests-gl-syn, $(duts)): %-asm-tests-gl-syn:
	$(SBT) "test-only $(PROJECT).$*AsmSYNTests"

$(addsuffix -bmark-tests-gl-syn, $(duts)): %-bmark-tests-gl-syn:
	$(SBT) "test-only $(PROJECT).$*BmarkSYNTests"

$(addsuffix -asm-tests-gl-par, $(duts)): %-asm-tests-gl-par:
	$(SBT) "test-only $(PROJECT).$*AsmPARTests"

$(addsuffix -bmark-tests-gl-par, $(duts)): %-bmark-tests-gl-par:
	$(SBT) "test-only $(PROJECT).$*BmarkPARTests"

# Tests for strober-chips
$(addsuffix -sim-asm-tests-cpp, $(duts)): %-sim-asm-tests-cpp:
	$(SBT) "test-only $(PROJECT).$*SimAsmCppTests"

$(addsuffix -sim-asm-tests-v, $(duts)): %-sim-asm-tests-v:
	$(SBT) "test-only $(PROJECT).$*SimAsmVerilogTests"

$(addsuffix -sim-bmark-tests-cpp, $(duts)): %-sim-bmark-tests-cpp:
	$(SBT) "test-only $(PROJECT).$*SimBmarkCppTests"

$(addsuffix -sim-bmark-tests-v, $(duts)): %-sim-bmark-tests-v:
	$(SBT) "test-only $(PROJECT).$*SimBmarkVerilogTests"

$(addsuffix -nasti-asm-tests-cpp, $(duts)): %-nasti-asm-tests-cpp:
	$(SBT) "test-only $(PROJECT).$*NastiShimAsmCppTests"

$(addsuffix -nasti-asm-tests-v, $(duts)): %-nasti-asm-tests-v:
	$(SBT) "test-only $(PROJECT).$*NastiShimAsmVerilogTests"

$(addsuffix -nasti-bmark-tests-cpp, $(duts)): %-nasti-bmark-tests-cpp:
	$(SBT) "test-only $(PROJECT).$*NastiShimBmarkCppTests"

$(addsuffix -nasti-bmark-tests-v, $(duts)): %-nasti-bmark-tests-v:
	$(SBT) "test-only $(PROJECT).$*NastiShimBmarkVerilogTests"

clean:
	rm -rf test-*
