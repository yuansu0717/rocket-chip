// See LICENSE for license details.

package rocketchip

import Chisel._
import Chisel.AdvTester._
import htif._
import junctions.{MemReqCmd, MemData, MemResp}
import scala.collection.mutable.{Queue => ScalaQueue}
import java.io.PrintStream
import java.nio.ByteBuffer

// Memory
case class TestMemReq(addr: Int, tag: BigInt, rw: Boolean) {
  override def toString = "[Mem Req] %s addr: %x, tag: %x".format(if (rw) "write" else "read", addr, tag)
}
case class TestMemData(data: BigInt) {
  override def toString = "[Mem Data] data: %x".format(data)
}
case class TestMemResp(data: BigInt, tag: BigInt) {
  override def toString = "[Mem Data] data: %x, tag: %x".format(data, tag)
}

abstract class SimMem(word_size: Int = 16, depth: Int = 1 << 20, 
    log: Option[PrintStream] = None) extends Processable {
  require(word_size % 4 == 0, "word_size should be divisible by 4")
  implicit def toBigInt(x: UInt) = x.litValue()
  private val addrMask = (1 << log2Up(depth))-1
  protected val off = log2Up(word_size)
  private val mem = Array.fill(depth){BigInt(0)}
  private def int(b: Byte) = (BigInt((b >>> 1) & 0x7f) << 1) | b & 0x1
  private def parseNibble(hex: Int) = if (hex >= 'a') hex - 'a' + 10 else hex - '0'

  def read(addr: Int) = {
    val data = mem(addr & addrMask)
    log match {
      case None =>
      case Some(f) => f.println("MEM[%x] => %x".format(addr & addrMask, data))
    }
    data
  }

  def write(addr: Int, data: BigInt) {
    log match {
      case None =>
      case Some(f) => f.println("MEM[%x] <= %x".format(addr & addrMask, data))
    }
    mem(addr & addrMask) = data
  }

  def loadMem(filename: String) {
    val lines = io.Source.fromFile(filename).getLines
    for ((line, i) <- lines.zipWithIndex) {
      val base = (i * line.length) / 2
      assert(base % word_size == 0)
      (((line.length - 2) to 0 by -2) foldLeft (BigInt(0), 0)){case ((data, offset), k) =>
        val shift = 8 * (offset % word_size)
        val byte = ((parseNibble(line(k)) << 4) | parseNibble(line(k+1))).toByte
        if ((offset % word_size) == word_size - 1) {
          mem((base+offset)>>off) = data | int(byte) << shift
          (BigInt(0), offset + 1)
        } else {
          (data | int(byte) << shift, offset + 1)
        }
      }
    }
  }
}

class FastMem(
    cmdQ: ScalaQueue[TestMemReq], 
    dataQ: ScalaQueue[TestMemData], 
    respQ: ScalaQueue[TestMemResp],
    log: Option[PrintStream] = None, 
    data_beats: Int = 1, word_size: Int = 16, depth: Int = 1 << 20)
    extends SimMem(word_size, depth, log) {
  private val line_size = data_beats*word_size
  private var store_inflight = false
  private var store_addr = 0
  private var store_count = 0
  def process {
    if (!dataQ.isEmpty && store_inflight) {
      val addr = store_addr + store_count*word_size
      val data = dataQ.dequeue.data
      write(addr >> off, data)
      store_count = (store_count + 1) % (data_beats)
      if (store_count == 0) {
        store_inflight = false
        cmdQ.dequeue
      }
    } else if (!cmdQ.isEmpty && cmdQ.front.rw && !store_inflight) {
      store_inflight = true
      store_addr = cmdQ.front.addr*line_size
    } else if (!cmdQ.isEmpty && !cmdQ.front.rw && !store_inflight) {
      val cmd  = cmdQ.dequeue
      val base = cmd.addr*line_size 
      (0 until data_beats) foreach {i => 
        val addr = base + i*word_size
        val resp = new TestMemResp(read(addr >> off), cmd.tag)
        respQ enqueue resp
      }
    }
  }
}

trait RocketTests extends AdvTests {
  System.loadLibrary("htif")
  class HTIFHandler(top: Top, htif: TesterHTIF) extends Processable {
    implicit def bigIntToBoolean(b: BigInt) = if (b == 0) false else true
    private val htif_bytes = top.io.host.in.bits.needWidth/8
    private var htif_in_valid = false
    private val htif_in_bits  = Array.fill(htif_bytes)(0.toByte)
    private val htif_out_bits = Array.fill(htif_bytes)(0.toByte)
    def process {
      if (peek(top.io.host.in.ready) || !htif_in_valid) {
        htif_in_valid = htif.recv_nonblocking(htif_in_bits, htif_bytes)
      }
      reg_poke(top.io.host.in.valid, int(htif_in_valid))
      reg_poke(top.io.host.in.bits,  int(ByteBuffer.wrap(htif_in_bits.reverse).getShort))
      if (peek(top.io.host.out.valid)) {
        val out_bits = peek(top.io.host.out.bits)
        (0 until htif_out_bits.size) foreach (htif_out_bits(_) = 0)
        out_bits.toByteArray.reverse.slice(0, htif_bytes).zipWithIndex foreach {
          case (bit, i) => htif_out_bits(i) = bit }
        htif.send(htif_out_bits, htif_bytes)
      }
      reg_poke(top.io.host.out.ready, 1)
    }
  }

  def run(c: Top, htif: TesterHTIF, maxcycles: Long, log: Option[PrintStream]) = {
    wire_poke(c.io.host.in.valid, 0)
    wire_poke(c.io.host.out.ready, 0)
    val startTime = System.nanoTime
    val pass = eventually(htif.done, maxcycles)
    val endTime = System.nanoTime
    val simTime = (endTime - startTime) / 1000000000.0
    val simSpeed = cycles / simTime
    val reason = if (cycles < maxcycles) s"tohost = ${htif.exit_code}" else "timeout"
    log match { case None => case Some(f) =>
      f.println("*** %s *** (%s) after %d simulation cycles".format(
                if (pass && htif.exit_code == 0) "PASSED" else "FAILED", reason, cycles))
      f.println("Time elapsed = %.1f s, Simulation Speed = %.2f Hz".format(simTime, simSpeed))
    }
    pass
  }
}

case class RocketChipTestArgs(
  loadmem: Option[String], 
  maxcycles: Long,
  dumpFile: Option[String] = None,
  logFile: Option[String] = None,
  testCmd: Option[String] = Driver.testCommand,
  htif: Array[String] = Array(),
  verbose: Boolean = true)

class RocketChipTester(c: Top, args: RocketChipTestArgs) 
    extends AdvTester(c, testCmd=args.testCmd, dumpFile=args.dumpFile) with RocketTests {
  val log = args.logFile match {
    case None    => System.out
    case Some(f) => new PrintStream(f)
  }
  val cmdHandler = new DecoupledSink(c.io.mem.req_cmd,
    (cmd: MemReqCmd) => new TestMemReq(peek(cmd.addr).toInt, peek(cmd.tag), peek(cmd.rw) != 0))
  val dataHandler = new DecoupledSink(c.io.mem.req_data,
    (data: MemData) => new TestMemData(peek(data.data)))
  val respHandler = new DecoupledSource(c.io.mem.resp,
    (resp: MemResp, in: TestMemResp) => {reg_poke(resp.data, in.data) ; reg_poke(resp.tag, in.tag)})
  val mem = new FastMem(cmdHandler.outputs, dataHandler.outputs, respHandler.inputs, 
                if (args.verbose) Some(log) else None, c.mifDataBeats, c.io.mem.resp.bits.data.needWidth/8)
  val htif = new TesterHTIF(args.htif.size, args.htif)
  val htifHandler = new HTIFHandler(c, htif) 

  preprocessors += htifHandler
  preprocessors += mem
  cmdHandler.max_count = 1
  cmdHandler.process()
  dataHandler.process()
  respHandler.process()

  if (args.verbose) addObserver(new Observer(file=log))

  args.loadmem match {
    case None =>
    case Some(f) => mem loadMem f
  }
  if (!run(c, htif, args.maxcycles, Some(log))) fail
}

class RocketChipSimTester(c: TopWrapper, sampleFile: Option[String], args: RocketChipTestArgs)
    extends strober.SimWrapperTester(c, new strober.StroberTestArgs(
      sampleFile, args.dumpFile, args.logFile, args.testCmd, args.verbose)) with RocketTests {
  val top = c.target
  val cmdHandler = new DecoupledSink(top.io.mem.req_cmd,
    (cmd: MemReqCmd) => new TestMemReq(peek(cmd.addr).toInt, peek(cmd.tag), peek(cmd.rw) != 0))
  val dataHandler = new DecoupledSink(top.io.mem.req_data,
    (data: MemData) => new TestMemData(peek(data.data)))
  val respHandler = new DecoupledSource(top.io.mem.resp,
    (resp: MemResp, in: TestMemResp) => {reg_poke(resp.data, in.data) ; reg_poke(resp.tag, in.tag)})
  val mem = new FastMem(cmdHandler.outputs, dataHandler.outputs, respHandler.inputs, 
              if (args.verbose) Some(log) else None, top.mifDataBeats, top.io.mem.resp.bits.data.needWidth/8)
  val htif = new TesterHTIF(args.htif.size, args.htif)
  val htifHandler = new HTIFHandler(top, htif) 

  preprocessors += mem
  preprocessors += htifHandler
  cmdHandler.max_count = 1
  cmdHandler.process()
  dataHandler.process()
  respHandler.process()


  args.loadmem match {
    case None =>
    case Some(f) => 
      println(s"[RocketchipSimTester] runs ${f}")
      mem loadMem f
  }
  setTraceLen(7)
  if (!run(top, htif, args.maxcycles, Some(log))) fail
}


class RocketChipNastiShimTester(c: NastiShim, sampleFile: Option[String], 
    args: RocketChipTestArgs, stepSize: Int = 128, memCycles: Int = 12) 
    extends strober.NastiShimTester(c, new strober.StroberTestArgs(
      sampleFile, args.dumpFile, args.logFile, args.testCmd, args.verbose)) with RocketTests {
  val top = c.sim.target
  val htif = new TesterHTIF(args.htif.size, args.htif)
  val htif_bytes = top.io.host.in.bits.needWidth/8
  var htif_in_valid = false
  val htif_in_bits  = Array.fill(htif_bytes)(0.toByte)
  val htif_out_bits = Array.fill(htif_bytes)(0.toByte)

  setTraceLen(stepSize)
  assert(traceLen % stepSize == 0)
  setMemCycles(memCycles)
  args.loadmem match {
    case None =>
    case Some(f) => 
      println(s"[RocketchipNastiShimTester] runs ${f}")
      loadMem(f)
  }

  val startTime = System.nanoTime
  do {
    assert(t % stepSize == 0)
    var stepped = 0
    do {
      if (peek(top.io.host.in.ready) || !htif_in_valid) {
        htif_in_valid = htif.recv_nonblocking(htif_in_bits, htif_bytes)
        poke(top.io.host.in.valid, htif_in_valid)
        poke(top.io.host.in.bits, ByteBuffer.wrap(htif_in_bits.reverse).getShort)
        if (htif_in_valid) {
          step(1)
          stepped += 1
          if (stepped >= stepSize) stepped -= stepSize
        }
      }
      if (peek(top.io.host.out.valid)) {
        val out_bits = peek(top.io.host.out.bits)
        (0 until htif_out_bits.size) foreach (htif_out_bits(_) = 0)
        out_bits.toByteArray.reverse.slice(0, htif_bytes).zipWithIndex foreach {
          case (bit, i) => htif_out_bits(i) = bit }
        htif.send(htif_out_bits, htif_bytes)
        poke(top.io.host.out.ready, true)
        step(1)
        stepped += 1
        if (stepped >= stepSize) stepped -= stepSize
      }
    } while ((peek(top.io.host.in.ready) && htif_in_valid) || peek(top.io.host.out.valid))
    poke(top.io.host.in.valid, false)
    poke(top.io.host.out.ready, false)
    step(stepSize-stepped)
  } while (!htif.done && cycles <= args.maxcycles)
  val endTime = System.nanoTime
  val simTime = (endTime - startTime) / 1000000000.0
  val simSpeed = cycles / simTime
  val reason = if (cycles < args.maxcycles) s"tohost = ${htif.exit_code}" else "timeout"
  expect(htif.exit_code == 0 && cycles <= args.maxcycles, 
    s"*** ${reason} *** after ${cycles} simulation cycles")
  log.println("Time elapsed = %.1f s, Simulation Speed = %.2f Hz".format(simTime, simSpeed))
}
