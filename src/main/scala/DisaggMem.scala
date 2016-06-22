package rocketchip

import Chisel._
import cde.{Parameters, Field}
import uncore._
import junctions._

case object NChips extends Field[Int]
case object NDisaggMemClients extends Field[Int]
case object NDisaggMemChannels extends Field[Int]

class DisaggregatedMemory(implicit val p: Parameters) extends Module {
  val nClients = p(NDisaggMemClients)
  val nChannels = p(NDisaggMemChannels)

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

  val mmioNetwork = Module(
    new ClientUncachedTileLinkIOCrossbar(nClients, nChannels, addrToBank))
  mmioNetwork.io.in <> io.clients

  io.channels.zip(mmioNetwork.io.out).foreach {
    case (axi, tl) => TopUtils.connectTilelinkNasti(axi, tl)
  }
}

class MultiChipTopIO(implicit p: Parameters) extends BasicTopIO()(p) {
  val coherent_mem = Vec(p(NChips), Vec(nMemChannels, new NastiIO))
  val disagg_mem = Vec(p(NDisaggMemChannels), new NastiIO)
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

  // Connect interrupts, debug interfaces, and coherent memory
  (chips, io.debug, io.coherent_mem).zipped.foreach {
    case (chip, debug, mem) =>
      chip.io.interrupts := io.interrupts
      chip.io.debug <> debug
      mem <> chip.io.mem_axi
  }

  val disaggMem = Module(new DisaggregatedMemory()(outerMMIOParams))
  disaggMem.io.clients <> chips.map(_.io.mmio_tl.head)
  io.disagg_mem <> disaggMem.io.channels
}
