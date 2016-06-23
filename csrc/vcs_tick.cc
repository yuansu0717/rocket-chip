#include "htif_emulator.h"
#include "mm.h"
#include "mm_dramsim2.h"
#include <DirectC.h>
#include <stdio.h>
#include <stdlib.h>

extern "C" {

htif_emulator_t* htif;
unsigned htif_bytes = HTIF_WIDTH / 8;

void htif_fini(vc_handle failure)
{
  delete htif;
  htif = NULL;
  exit(vc_getScalar(failure));
}

void htif_tick
(
  vc_handle htif_in_valid,
  vc_handle htif_in_ready,
  vc_handle htif_in_bits,
  vc_handle htif_out_valid,
  vc_handle htif_out_ready,
  vc_handle htif_out_bits,
  vc_handle exit
)
{
  static bool peek_in_valid;
  static uint32_t peek_in_bits;
  if (vc_getScalar(htif_in_ready))
    peek_in_valid = htif->recv_nonblocking(&peek_in_bits, htif_bytes);

  vc_putScalar(htif_out_ready, 1);
  if (vc_getScalar(htif_out_valid))
  {
    vec32* bits = vc_4stVectorRef(htif_out_bits);
    htif->send(&bits->d, htif_bytes);
  }

  vec32 bits = {0, 0};
  bits.d = peek_in_bits;
  vc_put4stVector(htif_in_bits, &bits);
  vc_putScalar(htif_in_valid, peek_in_valid);

  bits.d = htif->done() ? (htif->exit_code() << 1 | 1) : 0;
  vc_put4stVector(exit, &bits);
}

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
  vc_handle b_id)
{
  uint32_t write_data[mmc->get_word_size()/sizeof(uint32_t)];
  for (size_t i = 0; i < mmc->get_word_size()/sizeof(uint32_t); i++)
    write_data[i] = vc_4stVectorRef(w_data)[i].d;

  uint32_t aw_id_val, ar_id_val;

  if (MEM_ID_BITS == 1) {
    aw_id_val = vc_getScalar(aw_id);
    ar_id_val = vc_getScalar(ar_id);
  } else {
    aw_id_val = vc_4stVectorRef(aw_id)->d;
    ar_id_val = vc_4stVectorRef(ar_id)->d;
  }

  mmc->tick
  (
    vc_getScalar(ar_valid),
    vc_4stVectorRef(ar_addr)->d - MEM_BASE,
    ar_id_val,
    vc_4stVectorRef(ar_size)->d,
    vc_4stVectorRef(ar_len)->d,

    vc_getScalar(aw_valid),
    vc_4stVectorRef(aw_addr)->d - MEM_BASE,
    aw_id_val,
    vc_4stVectorRef(aw_size)->d,
    vc_4stVectorRef(aw_len)->d,

    vc_getScalar(w_valid),
    vc_4stVectorRef(w_strb)->d,
    write_data,
    vc_getScalar(w_last),

    vc_getScalar(r_ready),
    vc_getScalar(b_ready)
  );

  vc_putScalar(ar_ready, mmc->ar_ready());
  vc_putScalar(aw_ready, mmc->aw_ready());
  vc_putScalar(w_ready, mmc->w_ready());
  vc_putScalar(b_valid, mmc->b_valid());
  vc_putScalar(r_valid, mmc->r_valid());
  vc_putScalar(r_last, mmc->r_last());

  vec32 d[mmc->get_word_size()/sizeof(uint32_t)];

  d[0].c = 0;
  d[0].d = mmc->b_resp();
  vc_put4stVector(b_resp, d);

  d[0].c = 0;
  d[0].d = mmc->r_resp();
  vc_put4stVector(r_resp, d);

  if (MEM_ID_BITS > 1) {
    d[0].c = 0;
    d[0].d = mmc->b_id();
    vc_put4stVector(b_id, d);

    d[0].c = 0;
    d[0].d = mmc->r_id();
    vc_put4stVector(r_id, d);
  } else {
    vc_putScalar(b_id, mmc->b_id());
    vc_putScalar(r_id, mmc->r_id());
  }

  for (size_t i = 0; i < mmc->get_word_size()/sizeof(uint32_t); i++)
  {
    d[i].c = 0;
    d[i].d = ((uint32_t*)mmc->r_data())[i];
  }
  vc_put4stVector(r_data, d);
}

}
