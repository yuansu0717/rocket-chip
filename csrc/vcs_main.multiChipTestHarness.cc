// See LICENSE for license details.

#include "htif_emulator.h"
#include "mm.h"
#include "mm_dramsim2.h"
#include <DirectC.h>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <sstream>
#include <iterator>

extern "C" {

extern int vcs_main(int argc, char** argv);
extern htif_emulator_t *htif;
extern unsigned htif_bytes;

static const int n_mems = NCHIPS * N_MEM_CHANNELS + N_DISAGG_MEM_CHANNELS;
static mm_t* mm[n_mems];
static const char* loadmem;
static bool dramsim = false;
static int memory_channel_mux_select = 0;

void do_memory_tick(
  mm_t *mmc,

  vc_handle ar_valid,
  vc_handle ar_ready,
  vc_handle ar_addr,
  vc_handle ar_id,
  vc_handle ar_size,
  vc_handle ar_len,

  vc_handle aw_valid,
  vc_handle aw_ready,
  vc_handle aw_addr,
  vc_handle aw_id,
  vc_handle aw_size,
  vc_handle aw_len,

  vc_handle w_valid,
  vc_handle w_ready,
  vc_handle w_strb,
  vc_handle w_data,
  vc_handle w_last,

  vc_handle r_valid,
  vc_handle r_ready,
  vc_handle r_resp,
  vc_handle r_id,
  vc_handle r_data,
  vc_handle r_last,

  vc_handle b_valid,
  vc_handle b_ready,
  vc_handle b_resp,
  vc_handle b_id);

int main(int argc, char** argv)
{
  for (int i = 1; i < argc; i++)
  {
    if (!strcmp(argv[i], "+dramsim"))
      dramsim = true;
    else if (!strncmp(argv[i], "+loadmem=", 9))
      loadmem = argv[i]+9;
    else if (!strncmp(argv[i], "+memory_channel_mux_select=", 27))
      memory_channel_mux_select = atoi(argv[i]+27);
  }

  htif = new htif_emulator_t(std::vector<std::string>(argv + 1, argv + argc));

  for (int i=0; i < n_mems; i++) {
    mm[i] = dramsim ? (mm_t*)(new mm_dramsim2_t) : (mm_t*)(new mm_magic_t);
    mm[i]->init(MEM_SIZE / N_MEM_CHANNELS, MEM_DATA_BITS / 8, CACHE_BLOCK_BYTES);
  }

  if (loadmem) {
    void *mems[NCHIPS * N_MEM_CHANNELS];
    for (int i = 0; i < NCHIPS * N_MEM_CHANNELS; i++)
      mems[i] = mm[i]->get_data();
    for (int i = 0; i < NCHIPS; i++) {
        load_mem(&mems[i * N_MEM_CHANNELS], loadmem,
                 CACHE_BLOCK_BYTES, N_MEM_CHANNELS);
    }
  }

  vcs_main(argc, argv);
  abort(); // should never get here
}

void memory_tick(
  vc_handle channel,

  vc_handle ar_valid,
  vc_handle ar_ready,
  vc_handle ar_addr,
  vc_handle ar_id,
  vc_handle ar_size,
  vc_handle ar_len,

  vc_handle aw_valid,
  vc_handle aw_ready,
  vc_handle aw_addr,
  vc_handle aw_id,
  vc_handle aw_size,
  vc_handle aw_len,

  vc_handle w_valid,
  vc_handle w_ready,
  vc_handle w_strb,
  vc_handle w_data,
  vc_handle w_last,

  vc_handle r_valid,
  vc_handle r_ready,
  vc_handle r_resp,
  vc_handle r_id,
  vc_handle r_data,
  vc_handle r_last,

  vc_handle b_valid,
  vc_handle b_ready,
  vc_handle b_resp,
  vc_handle b_id)
{
  int c = vc_4stVectorRef(channel)->d;
  assert(c < n_mems);
  mm_t* mmc = mm[c];

  do_memory_tick(mmc,
      ar_valid, ar_ready, ar_addr, ar_id, ar_size, ar_len,
      aw_valid, aw_ready, aw_addr, aw_id, aw_size, aw_len,
      w_valid, w_ready, w_strb, w_data, w_last,
      r_valid, r_ready, r_resp, r_id, r_data, r_last,
      b_valid, b_ready, b_resp, b_id);
}

}
