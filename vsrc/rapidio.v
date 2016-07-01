module RapidIOBlackBox(
  input clock,
  input reset,

  input          in_acquire_valid,
  output         in_acquire_ready,
  input  [  1:0] in_acquire_bits_client_xact_id,
  input          in_acquire_bits_is_builtin_type,
  input  [  2:0] in_acquire_bits_a_type,
  input  [ 25:0] in_acquire_bits_addr_block,
  input  [  1:0] in_acquire_bits_addr_beat,
  input  [ 16:0] in_acquire_bits_union,
  input  [127:0] in_acquire_bits_data,

  output         in_grant_valid,
  input          in_grant_ready,
  output [  1:0] in_grant_bits_client_xact_id,
  output         in_grant_bits_manager_xact_id,
  output         in_grant_bits_is_builtin_type,
  output [  3:0] in_grant_bits_g_type,
  output [  1:0] in_grant_bits_addr_beat,
  output [127:0] in_grant_bits_data,

  output         out_acquire_valid,
  input          out_acquire_ready,
  output [  1:0] out_acquire_bits_client_xact_id,
  output         out_acquire_bits_is_builtin_type,
  output [  2:0] out_acquire_bits_a_type,
  output [ 25:0] out_acquire_bits_addr_block,
  output [  1:0] out_acquire_bits_addr_beat,
  output [ 16:0] out_acquire_bits_union,
  output [127:0] out_acquire_bits_data,

  input          out_grant_valid,
  output         out_grant_ready,
  input  [  1:0] out_grant_bits_client_xact_id,
  input          out_grant_bits_manager_xact_id,
  input          out_grant_bits_is_builtin_type,
  input  [  3:0] out_grant_bits_g_type,
  input  [  1:0] out_grant_bits_addr_beat,
  input  [127:0] out_grant_bits_data
);

// RapidIO goes here

  assign out_acquire_valid = in_acquire_valid;
  assign in_acquire_ready = out_acquire_ready;
  assign out_acquire_bits_client_xact_id = in_acquire_bits_client_xact_id;
  assign out_acquire_bits_is_builtin_type = in_acquire_bits_is_builtin_type;
  assign out_acquire_bits_a_type = in_acquire_bits_a_type;
  assign out_acquire_bits_addr_block = in_acquire_bits_addr_block;
  assign out_acquire_bits_addr_beat = in_acquire_bits_addr_beat;
  assign out_acquire_bits_union = in_acquire_bits_union;
  assign out_acquire_bits_data = in_acquire_bits_data;

  assign in_grant_valid = out_grant_valid;
  assign out_grant_ready = in_grant_ready;
  assign in_grant_bits_client_xact_id = out_grant_bits_client_xact_id;
  assign in_grant_bits_manager_xact_id = out_grant_bits_manager_xact_id;
  assign in_grant_bits_is_builtin_type = out_grant_bits_is_builtin_type;
  assign in_grant_bits_g_type = out_grant_bits_g_type;
  assign in_grant_bits_addr_beat = out_grant_bits_addr_beat;
  assign in_grant_bits_data = out_grant_bits_data;

endmodule
