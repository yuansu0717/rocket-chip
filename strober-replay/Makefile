defulat: compile

base_dir      = $(abspath ..)
replay_dir    = $(abspath .)
generated_dir = $(replay_dir)/generated-outs
vcs_sim_rtl_dir    = $(replay_dir)/vcs-sim-rtl
vcs_sim_gl_syn_dir = $(replay_dir)/vcs-sim-gl-syn
vcs_sim_gl_par_dir = $(replay_dir)/vcs-sim-gl-par
pt_pwr_dir = $(replay_dir)/pt-pwr

CONFIG ?= DefaultFPGAConfig

include $(base_dir)/Makefrag
include $(replay_dir)/Makefrag

compile: compile-v

clean:
	rm -rf $(log_dir) $(generated_dir)
