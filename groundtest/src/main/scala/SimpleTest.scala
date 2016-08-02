package groundtest

import Chisel._
import uncore.tilelink._
import junctions._
import cde.Parameters

class SimpleTest(implicit p: Parameters) extends GroundTest()(p) {
  require(nCached == 0)
  require(nUncached == 1)
  require(nPTW == 0)

  val s_idle :: s_put_req :: s_put_resp :: s_get_req :: s_get_resp :: s_done :: Nil = Enum(Bits(), 6)
  val state = Reg(init = s_idle)

  val put_acquire = Put(
    client_xact_id = UInt(0),
    addr_block = UInt(memStartBlock),
    addr_beat = UInt(0),
    data = UInt("hdeadbeef"))

  val get_acquire = Get(
    client_xact_id = UInt(0),
    addr_block = UInt(memStartBlock),
    addr_beat = UInt(0))

  val acq = io.mem.head.acquire
  val gnt = io.mem.head.grant

  acq.valid := (state === s_put_req) || (state === s_get_req)
  acq.bits := Mux(state === s_put_req, put_acquire, get_acquire)
  gnt.ready := (state === s_put_resp) || (state === s_get_resp)

  when (state === s_idle) { state := s_put_req }
  when (state === s_put_req && acq.ready) {
    state := s_put_resp
  }
  when (state === s_put_resp && gnt.valid) {
    state := s_get_req
  }
  when (state === s_get_req && acq.ready) {
    state := s_get_resp
  }
  when (state === s_get_resp && gnt.valid) {
    state := s_done
  }

  val data_mismatch = state === s_get_resp && gnt.valid && gnt.bits.data =/= UInt("hdeadbeef")
  assert(!data_mismatch, "SimpleTest: data mismatch")

  val timeout = Timer(4096, acq.fire(), gnt.fire())
  assert(!timeout, "SimpleTest: request timeout")

  io.status.finished := (state === s_done)
  io.status.error.valid := data_mismatch
  io.status.error.bits := UInt(1)
  io.status.timeout.valid := timeout
  io.status.timeout.bits := UInt(2)
}
