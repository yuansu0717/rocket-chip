package rocketchip

import Chisel._
import uncore.tilelink._
import cde.Parameters

class RapidIOBlackBox(implicit p: Parameters) extends BlackBox {
  val io = new Bundle {
    val clock = Clock(INPUT)
    val reset = Bool(INPUT)

    val in = new ClientUncachedTileLinkIO().flip
    val out = new ClientUncachedTileLinkIO()
  }
}

class RapidIOTop(topParams: Parameters) extends Module with HasTopLevelParameters {
  implicit val p = topParams
  val io = new TopIO

  val upperParams = topParams.alterPartial({
    case TMemoryChannels     => BusType.TL
  })
  val upper = Module(new Top(upperParams))
  upper.io.debug <> io.debug
  upper.io.interrupts := io.interrupts
  io.mmio_axi <> upper.io.mmio_axi
  io.mmio_ahb <> upper.io.mmio_ahb
  io.mmio_tl <> upper.io.mmio_tl

  val outerParams = topParams.alterPartial({
    case TLId => "Outermost"
  })
  val blackbox = Module(new RapidIOBlackBox()(outerParams))
  blackbox.io.clock := clock
  blackbox.io.reset := reset
  blackbox.io.in <> upper.io.mem_tl.head
  TopUtils.connectTilelinkNasti(
    io.mem_axi.head, blackbox.io.out)(outerParams)
}
