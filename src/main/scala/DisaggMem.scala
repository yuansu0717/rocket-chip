package rocketchip

import Chisel._
import cde.{Parameters, Field}
import uncore._
import junctions._

case object NChips extends Field[Int]
case object NDisaggMemClients extends Field[Int]
case object NDisaggMemChannels extends Field[Int]
case object DisaggMemBlackBoxNetwork extends Field[Boolean]

class BlackBoxTileLinkNetwork(implicit p: Parameters) extends BlackBox {
  val nClients = p(NDisaggMemClients)
  val nChannels = p(NDisaggMemChannels)

  val io = new Bundle {
    val clock = Clock(INPUT)
    val reset = Bool(INPUT)
    val tl = new TileLinkInterconnectIO(nClients, nChannels)
  }
}

class DisaggregatedMemory(implicit val p: Parameters) extends Module {
  val nClients = p(NDisaggMemClients)
  val nChannels = p(NDisaggMemChannels)
  val blackboxNetwork = p(DisaggMemBlackBoxNetwork)

  val io = new Bundle {
    val clients = Vec(p(NDisaggMemClients),
      new ClientUncachedTileLinkIO()).flip
    val channels = Vec(p(NDisaggMemChannels), new NastiIO)
  }
  
  def addrToBank(addr: UInt): UInt = {
    val offsetBits = p(CacheBlockOffsetBits)
    val bankSel = addr(offsetBits + log2Up(nChannels) - 1, offsetBits)
    UIntToOH(bankSel)
  }

  val mmioNetworkIO = if (blackboxNetwork) {
    val bbox = Module(new BlackBoxTileLinkNetwork)
    bbox.io.clock := clock
    bbox.io.reset := reset
    bbox.io.tl
  } else {
    Module(new ClientUncachedTileLinkIOCrossbar(
      nClients, nChannels, addrToBank)).io
  }

  mmioNetworkIO.in <> io.clients

  io.channels.zip(mmioNetworkIO.out).foreach {
    case (axi, tl) => TopUtils.connectTilelinkNasti(axi, tl)
  }
}

class MultiChipTopIO(implicit p: Parameters) extends BasicTopIO()(p) {
  val nMemPorts = p(NChips) * nMemChannels + p(NDisaggMemChannels)
  val mem_axi = Vec(nMemPorts, new NastiIO)
  val interrupts = Vec(p(NExtInterrupts), Bool()).asInput
  val debug = Vec(p(NChips), new DebugBusIO()(p)).flip
}

class MultiChipTop(topParams: Parameters) extends Module
    with HasTopLevelParameters {
  implicit val p = topParams
  val io = new MultiChipTopIO

  require(p(NChips) == p(NDisaggMemClients),
    "Only one disaggregated memory client per chip")
  require(p(NExtMMIOTLChannels) == 1,
    "Must have external TL channel for each chip")

  val nChips = p(NChips)
  val chips = Seq.fill(nChips) { Module(new Top(topParams)) }

  // Connect HostIO to first chip only
  io.host <> chips.head.io.host
  chips.tail.foreach { case chip =>
    chip.io.host.in.valid := Bool(false)
    chip.io.host.out.ready := Bool(false)
  }

  // Connect interrupts and debug interfaces
  chips.zip(io.debug).foreach {
    case (chip, debug) =>
      chip.io.interrupts := io.interrupts
      chip.io.debug <> debug
  }

  val disaggMem = Module(new DisaggregatedMemory()(outerMMIOParams))
  disaggMem.io.clients <> chips.map(_.io.mmio_tl.head)

  io.mem_axi <> chips.map(_.io.mem_axi).flatten ++ disaggMem.io.channels
}
