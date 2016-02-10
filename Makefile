default: all

duts = Rocket BOOM
base_dir = $(abspath .)

include $(base_dir)/Makefrag

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

# Tests for strober-chips
$(addsuffix -sim-tests-cpp, $(duts)): %-sim-tests-cpp:
	$(SBT) "test-only $(PROJECT).$*SimCppTests"

$(addsuffix -sim-tests-v, $(duts)): %-sim-tests-v:
	$(SBT) "test-only $(PROJECT).$*SimVerilogTests"

$(addsuffix -nasti-tests-cpp, $(duts)): %-nasti-tests-cpp:
	$(SBT) "test-only $(PROJECT).$*NastiShimCppTests"

$(addsuffix -nasti-tests-v, $(duts)): %-nasti-tests-v:
	$(SBT) "test-only $(PROJECT).$*NastiShimVerilogTests"

clean:
	rm -rf test-*
