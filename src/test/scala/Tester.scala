// See LICENSE for license details.

package rocketchip

import Chisel._
import Chisel.AdvTester._
import htif._
import junctions._
import scala.collection.mutable.{Queue => ScalaQueue}
import java.io.PrintStream
import java.nio.ByteBuffer

// Memory
case class TestNastiReadAddr(id: Int, addr: Int, size: Int, len: Int) {
  override def toString = "[NastiReadAddr] id: %x, addr: %x, size: %x, len: %x".format(id, addr, size, len)
}
case class TestNastiWriteAddr(id: Int, addr: Int, size: Int, len: Int) {
  override def toString = "[NastiWriteAddr] id: %x, addr: %x, size: %x, len: %x".format(id, addr, size, len)
}
case class TestNastiReadData(id: Int, data: BigInt, last: Boolean) {
  override def toString = "[NastiReadData] id: %x, data: %x, last: %s".format(id, data, last)
}
case class TestNastiWriteData(data: BigInt, last: Boolean) {
  override def toString = "[NastiWriteData] data: %x, last: %s".format(data, last)
}
case class TestNastiWriteResp(id: Int, resp: Int) {
  override def toString = "[Nasti Write Resp] id: %x, resp: %x".format(id, resp)
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
    arQ: ScalaQueue[TestNastiReadAddr],  rQ: ScalaQueue[TestNastiReadData], 
    awQ: ScalaQueue[TestNastiWriteAddr], wQ: ScalaQueue[TestNastiWriteData],
    bQ: ScalaQueue[TestNastiWriteResp], 
    log: Option[PrintStream] = None,  word_size: Int = 16, depth: Int = 1 << 20) 
    extends SimMem(word_size, depth, log) {
  private var aw: Option[TestNastiWriteAddr] = None
  def process = aw match {
    case Some(p) if wQ.size > p.len =>
      assert((1 << p.size) == word_size)
      (0 to p.len) foreach (i =>
        write((p.addr >> off) + i, wQ.dequeue.data))
      bQ enqueue new TestNastiWriteResp(p.id, 0)
      aw = None
    case None if !awQ.isEmpty => aw = Some(awQ.dequeue)
    case None if !arQ.isEmpty =>
      val ar = arQ.dequeue
      (0 to ar.len) foreach (i =>
        rQ enqueue new TestNastiReadData(
          ar.id, read((ar.addr >> off) + i), i == ar.len))
    case _ =>
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

  val htif = new TesterHTIF(args.htif.size, args.htif)
  val htifHandler = new HTIFHandler(c, htif) 
  preprocessors += htifHandler

  implicit def bigIntToInt(b: BigInt) = b.toInt
  implicit def bigIntToBoolean(b: BigInt) = if (b == 0) false else true
  implicit def booleanToBigInt(b: Boolean) = if (b) BigInt(1) else BigInt(0)
  val mems = c.io.mem map { nasti =>
    val arHandler = new DecoupledSink(nasti.ar, (ar: NastiReadAddressChannel) =>
      new TestNastiReadAddr(peek(ar.id), peek(ar.addr), peek(ar.size), peek(ar.len)))
    val awHandler = new DecoupledSink(nasti.aw, (aw: NastiWriteAddressChannel) =>
      new TestNastiWriteAddr(peek(aw.id), peek(aw.addr), peek(aw.size), peek(aw.len)))
    val wHandler = new DecoupledSink(nasti.w, (w: NastiWriteDataChannel) =>
      new TestNastiWriteData(peek(w.data), peek(w.last)))
    val rHandler = new DecoupledSource(nasti.r,
      (r: NastiReadDataChannel, in: TestNastiReadData) =>
        {reg_poke(r.id, in.id) ; reg_poke(r.data, in.data) ; reg_poke(r.last, in.last)})
    val bHandler = new DecoupledSource(nasti.b,
      (b: NastiWriteResponseChannel, in: TestNastiWriteResp) =>
        {reg_poke(b.id, in.id) ; reg_poke(b.resp, in.resp)})
    val mem = new FastMem(
      arHandler.outputs, rHandler.inputs, 
      awHandler.outputs, wHandler.outputs, bHandler.inputs,
      if (args.verbose) Some(log) else None, nasti.r.bits.nastiXDataBits/8)
    preprocessors += mem
    arHandler.process()
    awHandler.process()
    rHandler.process()
    wHandler.process()
    bHandler.process()
    mem
  }

  if (args.verbose) addObserver(new Observer(file=log))

  args.loadmem match {
    case None =>
    case Some(f) => mems foreach (_ loadMem f)
  }
  if (!run(c, htif, args.maxcycles, Some(log))) fail
}

class RocketChipSimTester(c: TopWrapper, sampleFile: Option[String], args: RocketChipTestArgs)
    extends strober.SimWrapperTester(c, new strober.StroberTestArgs(
      sampleFile, args.dumpFile, args.logFile, args.testCmd, args.verbose)) with RocketTests {
  val top = c.target
  val htif = new TesterHTIF(args.htif.size, args.htif)
  val htifHandler = new HTIFHandler(top, htif) 
  preprocessors += htifHandler

  implicit def bigIntToInt(b: BigInt) = b.toInt
  implicit def bigIntToBoolean(b: BigInt) = if (b == 0) false else true
  implicit def booleanToBigInt(b: Boolean) = if (b) BigInt(1) else BigInt(0)
  val mems = top.io.mem map { nasti =>
    val arHandler = new DecoupledSink(nasti.ar, (ar: NastiReadAddressChannel) =>
      new TestNastiReadAddr(peek(ar.id), peek(ar.addr), peek(ar.size), peek(ar.len)))
    val awHandler = new DecoupledSink(nasti.aw, (aw: NastiWriteAddressChannel) =>
      new TestNastiWriteAddr(peek(aw.id), peek(aw.addr), peek(aw.size), peek(aw.len)))
    val wHandler = new DecoupledSink(nasti.w, (w: NastiWriteDataChannel) =>
      new TestNastiWriteData(peek(w.data), peek(w.last)))
    val rHandler = new DecoupledSource(nasti.r,
      (r: NastiReadDataChannel, in: TestNastiReadData) =>
        {reg_poke(r.id, in.id) ; reg_poke(r.data, in.data) ; reg_poke(r.last, in.last)})
    val bHandler = new DecoupledSource(nasti.b,
      (b: NastiWriteResponseChannel, in: TestNastiWriteResp) =>
        {reg_poke(b.id, in.id) ; reg_poke(b.resp, in.resp)})
    val mem = new FastMem(
      arHandler.outputs, rHandler.inputs, 
      awHandler.outputs, wHandler.outputs, bHandler.inputs,
      if (args.verbose) Some(log) else None, nasti.r.bits.nastiXDataBits/8)
    preprocessors += mem
    arHandler.process()
    awHandler.process()
    rHandler.process()
    wHandler.process()
    bHandler.process()
    mem
  }

  args.loadmem match {
    case None =>
    case Some(f) => 
      println(s"[RocketchipSimTester] runs ${f}")
      mems foreach (_ loadMem f)
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
  // setMemCycles(memCycles)
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
