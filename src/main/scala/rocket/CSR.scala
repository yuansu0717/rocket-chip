// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package rocket

import collection.mutable.LinkedHashMap

import Chisel._
import Instructions._
import config._
import tile._
import uncore.devices._
import util._
import Chisel.ImplicitConversions._

class MStatus extends Bundle {
  // not truly part of mstatus, but convenient
  val debug = Bool()
  val isa = UInt(width = 32)

  val prv = UInt(width = PRV.SZ) // not truly part of mstatus, but convenient
  val sd = Bool()
  val zero2 = UInt(width = 27)
  val sxl = UInt(width = 2)
  val uxl = UInt(width = 2)
  val sd_rv32 = Bool()
  val zero1 = UInt(width = 8)
  val tsr = Bool()
  val tw = Bool()
  val tvm = Bool()
  val mxr = Bool()
  val pum = Bool()
  val mprv = Bool()
  val xs = UInt(width = 2)
  val fs = UInt(width = 2)
  val mpp = UInt(width = 2)
  val hpp = UInt(width = 2)
  val spp = UInt(width = 1)
  val mpie = Bool()
  val hpie = Bool()
  val spie = Bool()
  val upie = Bool()
  val mie = Bool()
  val hie = Bool()
  val sie = Bool()
  val uie = Bool()
}

class DCSR extends Bundle {
  val xdebugver = UInt(width = 2)
  val ndreset = Bool()
  val fullreset = Bool()
  val zero3 = UInt(width = 12)
  val ebreakm = Bool()
  val ebreakh = Bool()
  val ebreaks = Bool()
  val ebreaku = Bool()
  val zero2 = Bool()
  val stopcycle = Bool()
  val stoptime = Bool()
  val cause = UInt(width = 3)
  val debugint = Bool()
  val zero1 = Bool()
  val halt = Bool()
  val step = Bool()
  val prv = UInt(width = PRV.SZ)
}

class MIP extends Bundle {
  val rocc = Bool()
  val meip = Bool()
  val heip = Bool()
  val seip = Bool()
  val ueip = Bool()
  val mtip = Bool()
  val htip = Bool()
  val stip = Bool()
  val utip = Bool()
  val msip = Bool()
  val hsip = Bool()
  val ssip = Bool()
  val usip = Bool()
}

class PTBR(implicit p: Parameters) extends CoreBundle()(p) {
  def pgLevelsToMode(i: Int) = (xLen, i) match {
    case (32, 2) => 1
    case (64, x) if x >= 3 && x <= 6 => x + 5
  }
  val (modeBits, maxASIdBits) = xLen match {
    case 32 => (1, 9)
    case 64 => (4, 16)
  }
  require(modeBits + maxASIdBits + maxPAddrBits - pgIdxBits == xLen)

  val mode = UInt(width = modeBits)
  val asid = UInt(width = maxASIdBits)
  val ppn = UInt(width = maxPAddrBits - pgIdxBits)
}

object PRV
{
  val SZ = 2
  val U = 0
  val S = 1
  val H = 2
  val M = 3
}

object CSR
{
  // commands
  val SZ = 3
  def X = BitPat.dontCare(SZ)
  def N = UInt(0,SZ)
  def W = UInt(1,SZ)
  def S = UInt(2,SZ)
  def C = UInt(3,SZ)
  def I = UInt(4,SZ)
  def R = UInt(5,SZ)

  val ADDRSZ = 12
  def debugIntCause = new MIP().getWidth
  def debugTriggerCause = {
    require(debugIntCause >= Causes.all.max)
    debugIntCause
  }

  val firstCtr = CSRs.cycle
  val firstCtrH = CSRs.cycleh
  val firstHPC = CSRs.hpmcounter3
  val firstHPCH = CSRs.hpmcounter3h
  val firstHPE = CSRs.mhpmevent3
  val firstMHPC = CSRs.mhpmcounter3
  val firstMHPCH = CSRs.mhpmcounter3h
  val firstHPM = 3
  val nCtr = 32
  val nHPM = nCtr - firstHPM
}

class PerfCounterIO(implicit p: Parameters) extends CoreBundle
    with HasRocketCoreParameters {
  val eventSel = UInt(OUTPUT, xLen)
  val inc = UInt(INPUT, log2Ceil(1+retireWidth))
}

class CSRFileIO(implicit p: Parameters) extends CoreBundle
    with HasRocketCoreParameters {
  val interrupts = new TileInterrupts().asInput
  val hartid = UInt(INPUT, xLen)
  val rw = new Bundle {
    val addr = UInt(INPUT, CSR.ADDRSZ)
    val cmd = Bits(INPUT, CSR.SZ)
    val rdata = Bits(OUTPUT, xLen)
    val wdata = Bits(INPUT, xLen)
  }

  val decode = new Bundle {
    val csr = UInt(INPUT, CSR.ADDRSZ)
    val fp_illegal = Bool(OUTPUT)
    val rocc_illegal = Bool(OUTPUT)
    val read_illegal = Bool(OUTPUT)
    val write_illegal = Bool(OUTPUT)
    val write_flush = Bool(OUTPUT)
    val system_illegal = Bool(OUTPUT)
  }

  val csr_stall = Bool(OUTPUT)
  val eret = Bool(OUTPUT)
  val singleStep = Bool(OUTPUT)

  val status = new MStatus().asOutput
  val ptbr = new PTBR().asOutput
  val evec = UInt(OUTPUT, vaddrBitsExtended)
  val exception = Bool(INPUT)
  val retire = UInt(INPUT, log2Up(1+retireWidth))
  val custom_mrw_csrs = Vec(nCustomMrwCsrs, UInt(INPUT, xLen))
  val cause = UInt(INPUT, xLen)
  val pc = UInt(INPUT, vaddrBitsExtended)
  val badaddr = UInt(INPUT, vaddrBitsExtended)
  val fatc = Bool(OUTPUT)
  val time = UInt(OUTPUT, xLen)
  val fcsr_rm = Bits(OUTPUT, FPConstants.RM_SZ)
  val fcsr_flags = Valid(Bits(width = FPConstants.FLAGS_SZ)).flip
  val rocc_interrupt = Bool(INPUT)
  val interrupt = Bool(OUTPUT)
  val interrupt_cause = UInt(OUTPUT, xLen)
  val bp = Vec(nBreakpoints, new BP).asOutput
  val counters = Vec(nPerfCounters, new PerfCounterIO)
}

class CSRFile(perfEventSets: EventSets = new EventSets(Seq()))(implicit p: Parameters) extends CoreModule()(p)
    with HasRocketCoreParameters {
  val io = new CSRFileIO

  val reset_mstatus = Wire(init=new MStatus().fromBits(0))
  reset_mstatus.mpp := PRV.M
  reset_mstatus.prv := PRV.M
  val reg_mstatus = Reg(init=reset_mstatus)

  val new_prv = Wire(init = reg_mstatus.prv)
  reg_mstatus.prv := legalizePrivilege(new_prv)

  val reset_dcsr = Wire(init=new DCSR().fromBits(0))
  reset_dcsr.xdebugver := 1
  reset_dcsr.prv := PRV.M
  val reg_dcsr = Reg(init=reset_dcsr)

  val (supported_interrupts, delegable_interrupts) = {
    val sup = Wire(init=new MIP().fromBits(0))
    sup.ssip := Bool(usingVM)
    sup.msip := true
    sup.stip := Bool(usingVM)
    sup.mtip := true
    sup.meip := true
    sup.seip := Bool(usingVM)
    sup.rocc := usingRoCC

    val del = Wire(init=sup)
    del.msip := false
    del.mtip := false
    del.meip := false

    (sup.asUInt, del.asUInt)
  }
  val delegable_exceptions = UInt(Seq(
    Causes.misaligned_fetch,
    Causes.fault_fetch,
    Causes.breakpoint,
    Causes.fault_load,
    Causes.fault_store,
    Causes.user_ecall).map(1 << _).sum)

  val reg_debug = Reg(init=Bool(false))
  val effective_prv = Cat(reg_debug, reg_mstatus.prv)
  val reg_dpc = Reg(UInt(width = vaddrBitsExtended))
  val reg_dscratch = Reg(UInt(width = xLen))
  val reg_singleStepped = Reg(Bool())

  val reg_tselect = Reg(UInt(width = log2Up(nBreakpoints)))
  val reg_bp = Reg(Vec(1 << log2Up(nBreakpoints), new BP))

  val reg_mie = Reg(UInt(width = xLen))
  val reg_mideleg = Reg(UInt(width = xLen))
  val reg_medeleg = Reg(UInt(width = xLen))
  val reg_mip = Reg(new MIP)
  val reg_mepc = Reg(UInt(width = vaddrBitsExtended))
  val reg_mcause = Reg(Bits(width = xLen))
  val reg_mbadaddr = Reg(UInt(width = vaddrBitsExtended))
  val reg_mscratch = Reg(Bits(width = xLen))
  val mtvecWidth = paddrBits min xLen
  val reg_mtvec = mtvecInit match {
    case Some(addr) => Reg(init=UInt(addr, mtvecWidth))
    case None => Reg(UInt(width = mtvecWidth))
  }
  val reg_mcounteren = Reg(UInt(width = 32))
  val reg_scounteren = Reg(UInt(width = 32))
  val delegable_counters = (BigInt(1) << (nPerfCounters + CSR.firstHPM)) - 1

  val reg_sepc = Reg(UInt(width = vaddrBitsExtended))
  val reg_scause = Reg(Bits(width = xLen))
  val reg_sbadaddr = Reg(UInt(width = vaddrBitsExtended))
  val reg_sscratch = Reg(Bits(width = xLen))
  val reg_stvec = Reg(UInt(width = vaddrBits))
  val reg_sptbr = Reg(new PTBR)
  val reg_wfi = Reg(init=Bool(false))

  val reg_fflags = Reg(UInt(width = 5))
  val reg_frm = Reg(UInt(width = 3))

  val reg_instret = WideCounter(64, io.retire)
  val reg_cycle = if (enableCommitLog) reg_instret else WideCounter(64)
  val reg_hpmevent = io.counters.map(c => Reg(init = UInt(0, xLen)))
  (io.counters zip reg_hpmevent) foreach { case (c, e) => c.eventSel := e }
  val reg_hpmcounter = io.counters.map(c => WideCounter(40, c.inc, reset = false))
  val hpm_mask = reg_mcounteren & Mux((!usingVM).B || reg_mstatus.prv === PRV.S, delegable_counters.U, reg_scounteren)

  val mip = Wire(init=reg_mip)
  mip.rocc := io.rocc_interrupt
  val read_mip = mip.asUInt & supported_interrupts

  val pending_interrupts = read_mip & reg_mie
  val m_interrupts = Mux(reg_mstatus.prv <= PRV.S || (reg_mstatus.prv === PRV.M && reg_mstatus.mie), pending_interrupts & ~reg_mideleg, UInt(0))
  val s_interrupts = Mux(m_interrupts === 0 && (reg_mstatus.prv < PRV.S || (reg_mstatus.prv === PRV.S && reg_mstatus.sie)), pending_interrupts & reg_mideleg, UInt(0))
  val all_interrupts = m_interrupts | s_interrupts
  val interruptMSB = BigInt(1) << (xLen-1)
  val interruptCause = UInt(interruptMSB) + PriorityEncoder(all_interrupts)
  io.interrupt := all_interrupts.orR && !reg_debug && !io.singleStep || reg_singleStepped
  io.interrupt_cause := interruptCause
  io.bp := reg_bp take nBreakpoints

  // debug interrupts are only masked by being in debug mode
  when (Bool(usingDebug) && reg_dcsr.debugint && !reg_debug) {
    io.interrupt := true
    io.interrupt_cause := UInt(interruptMSB) + CSR.debugIntCause
  }

  val isaMaskString =
    (if (usingMulDiv) "M" else "") +
    (if (usingAtomics) "A" else "") +
    (if (usingFPU) "F" else "") +
    (if (usingFPU && xLen > 32) "D" else "") +
    (if (usingCompressed) "C" else "") +
    (if (usingRoCC) "X" else "")
  val isaString = "I" + isaMaskString +
    (if (usingVM) "S" else "") +
    (if (usingUser) "U" else "")
  val isaMax = (BigInt(log2Ceil(xLen) - 4) << (xLen-2)) | isaStringToMask(isaString)
  val reg_misa = Reg(init=UInt(isaMax))
  val read_mstatus = io.status.asUInt()(xLen-1,0)

  val read_mapping = LinkedHashMap[Int,Bits](
    CSRs.tselect -> reg_tselect,
    CSRs.tdata1 -> reg_bp(reg_tselect).control.asUInt,
    CSRs.tdata2 -> reg_bp(reg_tselect).address.sextTo(xLen),
    CSRs.mimpid -> UInt(0),
    CSRs.marchid -> UInt(0),
    CSRs.mvendorid -> UInt(0),
    CSRs.mcycle -> reg_cycle,
    CSRs.minstret -> reg_instret,
    CSRs.misa -> reg_misa,
    CSRs.mstatus -> read_mstatus,
    CSRs.mtvec -> reg_mtvec,
    CSRs.mip -> read_mip,
    CSRs.mie -> reg_mie,
    CSRs.mideleg -> reg_mideleg,
    CSRs.medeleg -> reg_medeleg,
    CSRs.mscratch -> reg_mscratch,
    CSRs.mepc -> reg_mepc.sextTo(xLen),
    CSRs.mbadaddr -> reg_mbadaddr.sextTo(xLen),
    CSRs.mcause -> reg_mcause,
    CSRs.mhartid -> io.hartid)

  val debug_csrs = LinkedHashMap[Int,Bits](
    CSRs.dcsr -> reg_dcsr.asUInt,
    CSRs.dpc -> reg_dpc.asUInt,
    CSRs.dscratch -> reg_dscratch.asUInt)

  val fp_csrs = LinkedHashMap[Int,Bits](
    CSRs.fflags -> reg_fflags,
    CSRs.frm -> reg_frm,
    CSRs.fcsr -> Cat(reg_frm, reg_fflags))

  if (usingDebug)
    read_mapping ++= debug_csrs

  if (usingFPU)
    read_mapping ++= fp_csrs

  for (((e, c), i) <- (reg_hpmevent.padTo(CSR.nHPM, UInt(0))
                       zip reg_hpmcounter.map(x => x: UInt).padTo(CSR.nHPM, UInt(0))) zipWithIndex) {
    read_mapping += (i + CSR.firstHPE) -> e // mhpmeventN
    read_mapping += (i + CSR.firstMHPC) -> c // mhpmcounterN
    if (usingUser) read_mapping += (i + CSR.firstHPC) -> c // hpmcounterN
    if (xLen == 32) {
      read_mapping += (i + CSR.firstMHPCH) -> c // mhpmcounterNh
      if (usingUser) read_mapping += (i + CSR.firstHPCH) -> c // hpmcounterNh
    }
  }

  if (usingVM) {
    val read_sie = reg_mie & reg_mideleg
    val read_sip = read_mip & reg_mideleg
    val read_sstatus = Wire(init=io.status)
    read_sstatus.mprv := 0
    read_sstatus.mpp := 0
    read_sstatus.hpp := 0
    read_sstatus.mpie := 0
    read_sstatus.hpie := 0
    read_sstatus.mie := 0
    read_sstatus.hie := 0

    read_mapping += CSRs.sstatus -> (read_sstatus.asUInt())(xLen-1,0)
    read_mapping += CSRs.sip -> read_sip.asUInt
    read_mapping += CSRs.sie -> read_sie.asUInt
    read_mapping += CSRs.sscratch -> reg_sscratch
    read_mapping += CSRs.scause -> reg_scause
    read_mapping += CSRs.sbadaddr -> reg_sbadaddr.sextTo(xLen)
    read_mapping += CSRs.sptbr -> reg_sptbr.asUInt
    read_mapping += CSRs.sepc -> reg_sepc.sextTo(xLen)
    read_mapping += CSRs.stvec -> reg_stvec.sextTo(xLen)
    read_mapping += CSRs.scounteren -> reg_scounteren
  }

  if (usingUser) {
    read_mapping += CSRs.mcounteren -> reg_mcounteren
    read_mapping += CSRs.cycle -> reg_cycle
    read_mapping += CSRs.instret -> reg_instret
  }

  if (xLen == 32) {
    read_mapping += CSRs.mcycleh -> (reg_cycle >> 32)
    read_mapping += CSRs.minstreth -> (reg_instret >> 32)
    if (usingUser) {
      read_mapping += CSRs.cycleh -> (reg_cycle >> 32)
      read_mapping += CSRs.instreth -> (reg_instret >> 32)
    }
  }

  for (i <- 0 until nCustomMrwCsrs) {
    val addr = 0xff0 + i
    require(addr < (1 << CSR.ADDRSZ))
    require(!read_mapping.contains(addr), "custom MRW CSR address " + i + " is already in use")
    read_mapping += addr -> io.custom_mrw_csrs(i)
  }

  val decoded_addr = read_mapping map { case (k, v) => k -> (io.rw.addr === k) }
  val wdata = (Mux(io.rw.cmd.isOneOf(CSR.S, CSR.C), io.rw.rdata, UInt(0)) | io.rw.wdata) &
              ~Mux(io.rw.cmd === CSR.C, io.rw.wdata, UInt(0))

  val system_insn = io.rw.cmd === CSR.I
  val opcode = UInt(1) << io.rw.addr(2,0)
  val insn_rs2 = io.rw.addr(5)
  val insn_call = system_insn && !insn_rs2 && opcode(0)
  val insn_break = system_insn && opcode(1)
  val insn_ret = system_insn && opcode(2)
  val insn_wfi = system_insn && opcode(5)
  val insn_sfence_vma = system_insn && insn_rs2

  private def decodeAny(m: LinkedHashMap[Int,Bits]): Bool = m.map { case(k: Int, _: Bits) => io.decode.csr === k }.reduce(_||_)
  val allow_wfi = Bool(!usingVM) || effective_prv > PRV.S || !reg_mstatus.tw
  val allow_sfence_vma = Bool(!usingVM) || effective_prv > PRV.S || !reg_mstatus.tvm
  val allow_sret = Bool(!usingVM) || effective_prv > PRV.S || !reg_mstatus.tsr
  io.decode.fp_illegal := io.status.fs === 0 || !reg_misa('f'-'a')
  io.decode.rocc_illegal := io.status.xs === 0 || !reg_misa('x'-'a')
  io.decode.read_illegal := effective_prv < io.decode.csr(9,8) ||
    !decodeAny(read_mapping) ||
    io.decode.csr === CSRs.sptbr && !allow_sfence_vma ||
    (io.decode.csr.inRange(CSR.firstCtr, CSR.firstCtr + CSR.nCtr) || io.decode.csr.inRange(CSR.firstCtrH, CSR.firstCtrH + CSR.nCtr)) && effective_prv <= PRV.S && hpm_mask(io.decode.csr(log2Ceil(CSR.firstCtr)-1,0)) ||
    Bool(usingDebug) && decodeAny(debug_csrs) && !reg_debug ||
    Bool(usingFPU) && decodeAny(fp_csrs) && io.decode.fp_illegal
  io.decode.write_illegal := io.decode.csr(11,10).andR
  io.decode.write_flush := !(io.decode.csr >= CSRs.mscratch && io.decode.csr <= CSRs.mbadaddr || io.decode.csr >= CSRs.sscratch && io.decode.csr <= CSRs.sbadaddr)
  io.decode.system_illegal := effective_prv < io.decode.csr(9,8) ||
    !io.decode.csr(5) && io.decode.csr(2) && !allow_wfi ||
    !io.decode.csr(5) && io.decode.csr(1) && !allow_sret ||
    io.decode.csr(5) && !allow_sfence_vma

  val cause =
    Mux(insn_call, reg_mstatus.prv + Causes.user_ecall,
    Mux[UInt](insn_break, Causes.breakpoint, io.cause))
  val cause_lsbs = cause(log2Up(xLen)-1,0)
  val causeIsDebugInt = cause(xLen-1) && cause_lsbs === CSR.debugIntCause
  val causeIsDebugTrigger = !cause(xLen-1) && cause_lsbs === CSR.debugTriggerCause
  val causeIsDebugBreak = !cause(xLen-1) && insn_break && Cat(reg_dcsr.ebreakm, reg_dcsr.ebreakh, reg_dcsr.ebreaks, reg_dcsr.ebreaku)(reg_mstatus.prv)
  val trapToDebug = Bool(usingDebug) && (reg_singleStepped || causeIsDebugInt || causeIsDebugTrigger || causeIsDebugBreak || reg_debug)
  val delegate = Bool(usingVM) && reg_mstatus.prv <= PRV.S && Mux(cause(xLen-1), reg_mideleg(cause_lsbs), reg_medeleg(cause_lsbs))
  val debugTVec = Mux(reg_debug, UInt(0x808), UInt(0x800))
  val tvec = Mux(trapToDebug, debugTVec, Mux(delegate, reg_stvec.sextTo(vaddrBitsExtended), reg_mtvec))
  io.fatc := insn_sfence_vma
  io.evec := tvec
  io.ptbr := reg_sptbr
  io.eret := insn_call || insn_break || insn_ret
  io.singleStep := reg_dcsr.step && !reg_debug
  io.status := reg_mstatus
  io.status.sd := io.status.fs.andR || io.status.xs.andR
  io.status.debug := reg_debug
  io.status.isa := reg_misa
  io.status.uxl := (if (usingUser) log2Ceil(xLen) - 4 else 0)
  io.status.sxl := (if (usingVM) log2Ceil(xLen) - 4 else 0)
  if (xLen == 32)
    io.status.sd_rv32 := io.status.sd

  val exception = insn_call || insn_break || io.exception
  assert(PopCount(insn_ret :: insn_call :: insn_break :: io.exception :: Nil) <= 1, "these conditions must be mutually exclusive")

  when (insn_wfi) { reg_wfi := true }
  when (pending_interrupts.orR || exception) { reg_wfi := false }
  assert(!reg_wfi || io.retire === UInt(0))

  when (io.retire(0)) { reg_singleStepped := true }
  when (!io.singleStep) { reg_singleStepped := false }
  assert(!io.singleStep || io.retire <= UInt(1))
  assert(!reg_singleStepped || io.retire === UInt(0))

  when (exception) {
    val epc = ~(~io.pc | (coreInstBytes-1))
    val pie = read_mstatus(reg_mstatus.prv)

    val write_badaddr = cause isOneOf (Causes.breakpoint,
      Causes.misaligned_load, Causes.misaligned_store, Causes.misaligned_fetch,
      Causes.fault_load, Causes.fault_store, Causes.fault_fetch)

    when (trapToDebug) {
      reg_debug := true
      reg_dpc := epc
      reg_dcsr.cause := Mux(reg_singleStepped, 4, Mux(causeIsDebugInt, 3, Mux[UInt](causeIsDebugTrigger, 2, 1)))
      reg_dcsr.prv := trimPrivilege(reg_mstatus.prv)
    }.elsewhen (delegate) {
      reg_sepc := formEPC(epc)
      reg_scause := cause
      when (write_badaddr) { reg_sbadaddr := io.badaddr }
      reg_mstatus.spie := pie
      reg_mstatus.spp := reg_mstatus.prv
      reg_mstatus.sie := false
      new_prv := PRV.S
    }.otherwise {
      reg_mepc := formEPC(epc)
      reg_mcause := cause
      when (write_badaddr) { reg_mbadaddr := io.badaddr }
      reg_mstatus.mpie := pie
      reg_mstatus.mpp := trimPrivilege(reg_mstatus.prv)
      reg_mstatus.mie := false
      new_prv := PRV.M
    }
  }

  when (insn_ret) {
    when (Bool(usingVM) && !io.rw.addr(9)) {
      when (reg_mstatus.spp.toBool) { reg_mstatus.sie := reg_mstatus.spie }
      reg_mstatus.spie := true
      reg_mstatus.spp := PRV.U
      new_prv := reg_mstatus.spp
      io.evec := reg_sepc
    }.elsewhen (Bool(usingDebug) && io.rw.addr(10)) {
      new_prv := reg_dcsr.prv
      reg_debug := false
      io.evec := reg_dpc
    }.otherwise {
      when (reg_mstatus.mpp(1)) { reg_mstatus.mie := reg_mstatus.mpie }
      .elsewhen (Bool(usingVM) && reg_mstatus.mpp(0)) { reg_mstatus.sie := reg_mstatus.mpie }
      reg_mstatus.mpie := true
      reg_mstatus.mpp := legalizePrivilege(PRV.U)
      new_prv := reg_mstatus.mpp
      io.evec := reg_mepc
    }
  }

  io.time := reg_cycle
  io.csr_stall := reg_wfi

  io.rw.rdata := Mux1H(for ((k, v) <- read_mapping) yield decoded_addr(k) -> v)

  io.fcsr_rm := reg_frm
  when (io.fcsr_flags.valid) {
    reg_fflags := reg_fflags | io.fcsr_flags.bits
  }

  when (io.rw.cmd.isOneOf(CSR.S, CSR.C, CSR.W)) {
    when (decoded_addr(CSRs.mstatus)) {
      val new_mstatus = new MStatus().fromBits(wdata)
      reg_mstatus.mie := new_mstatus.mie
      reg_mstatus.mpie := new_mstatus.mpie

      if (usingUser) {
        reg_mstatus.mprv := new_mstatus.mprv
        reg_mstatus.mpp := trimPrivilege(new_mstatus.mpp)
        reg_mstatus.mxr := new_mstatus.mxr
        if (usingVM) {
          reg_mstatus.pum := new_mstatus.pum
          reg_mstatus.spp := new_mstatus.spp
          reg_mstatus.spie := new_mstatus.spie
          reg_mstatus.sie := new_mstatus.sie
          reg_mstatus.tw := new_mstatus.tw
          reg_mstatus.tvm := new_mstatus.tvm
          reg_mstatus.tsr := new_mstatus.tsr
        }
      }

      if (usingVM || usingFPU) reg_mstatus.fs := Fill(2, new_mstatus.fs.orR)
      if (usingRoCC) reg_mstatus.xs := Fill(2, new_mstatus.xs.orR)
    }
    when (decoded_addr(CSRs.misa)) {
      val mask = UInt(isaStringToMask(isaMaskString))
      val f = wdata('f' - 'a')
      reg_misa := ~(~wdata | (!f << ('d' - 'a'))) & mask | reg_misa & ~mask
    }
    when (decoded_addr(CSRs.mip)) {
      val new_mip = new MIP().fromBits(wdata)
      if (usingVM) {
        reg_mip.ssip := new_mip.ssip
        reg_mip.stip := new_mip.stip
      }
    }
    when (decoded_addr(CSRs.mie))      { reg_mie := wdata & supported_interrupts }
    when (decoded_addr(CSRs.mepc))     { reg_mepc := formEPC(wdata) }
    when (decoded_addr(CSRs.mscratch)) { reg_mscratch := wdata }
    if (mtvecWritable)
      when (decoded_addr(CSRs.mtvec))  { reg_mtvec := wdata >> 2 << 2 }
    when (decoded_addr(CSRs.mcause))   { reg_mcause := wdata & UInt((BigInt(1) << (xLen-1)) + 31) /* only implement 5 LSBs and MSB */ }
    when (decoded_addr(CSRs.mbadaddr)) { reg_mbadaddr := wdata(vaddrBitsExtended-1,0) }

    for (((e, c), i) <- (reg_hpmevent zip reg_hpmcounter) zipWithIndex) {
      writeCounter(i + CSR.firstMHPC, c, wdata)
      when (decoded_addr(i + CSR.firstHPE)) { e := perfEventSets.maskEventSelector(wdata) }
    }
    writeCounter(CSRs.mcycle, reg_cycle, wdata)
    writeCounter(CSRs.minstret, reg_instret, wdata)

    if (usingFPU) {
      when (decoded_addr(CSRs.fflags)) { reg_fflags := wdata }
      when (decoded_addr(CSRs.frm))    { reg_frm := wdata }
      when (decoded_addr(CSRs.fcsr))   { reg_fflags := wdata; reg_frm := wdata >> reg_fflags.getWidth }
    }
    if (usingDebug) {
      when (decoded_addr(CSRs.dcsr)) {
        val new_dcsr = new DCSR().fromBits(wdata)
        reg_dcsr.halt := new_dcsr.halt
        reg_dcsr.step := new_dcsr.step
        reg_dcsr.ebreakm := new_dcsr.ebreakm
        if (usingVM) reg_dcsr.ebreaks := new_dcsr.ebreaks
        if (usingUser) reg_dcsr.ebreaku := new_dcsr.ebreaku
        if (usingUser) reg_dcsr.prv := trimPrivilege(new_dcsr.prv)
      }
      when (decoded_addr(CSRs.dpc))      { reg_dpc := ~(~wdata | (coreInstBytes-1)) }
      when (decoded_addr(CSRs.dscratch)) { reg_dscratch := wdata }
    }
    if (usingVM) {
      when (decoded_addr(CSRs.sstatus)) {
        val new_sstatus = new MStatus().fromBits(wdata)
        reg_mstatus.sie := new_sstatus.sie
        reg_mstatus.spie := new_sstatus.spie
        reg_mstatus.spp := new_sstatus.spp
        reg_mstatus.pum := new_sstatus.pum
        reg_mstatus.fs := Fill(2, new_sstatus.fs.orR) // even without an FPU
        if (usingRoCC) reg_mstatus.xs := Fill(2, new_sstatus.xs.orR)
      }
      when (decoded_addr(CSRs.sip)) {
        val new_sip = new MIP().fromBits(wdata)
        reg_mip.ssip := new_sip.ssip
      }
      when (decoded_addr(CSRs.sptbr)) {
        val new_sptbr = new PTBR().fromBits(wdata)
        val valid_mode = new_sptbr.pgLevelsToMode(pgLevels)
        when (new_sptbr.mode === 0) { reg_sptbr.mode := 0 }
        when (new_sptbr.mode === valid_mode) { reg_sptbr.mode := valid_mode }
        when (new_sptbr.mode === 0 || new_sptbr.mode === valid_mode) {
          reg_sptbr.ppn := new_sptbr.ppn(ppnBits-1,0)
          if (asIdBits > 0) reg_sptbr.asid := new_sptbr.asid(asIdBits-1,0)
        }
      }
      when (decoded_addr(CSRs.sie))      { reg_mie := (reg_mie & ~reg_mideleg) | (wdata & reg_mideleg) }
      when (decoded_addr(CSRs.sscratch)) { reg_sscratch := wdata }
      when (decoded_addr(CSRs.sepc))     { reg_sepc := formEPC(wdata) }
      when (decoded_addr(CSRs.stvec))    { reg_stvec := wdata >> 2 << 2 }
      when (decoded_addr(CSRs.scause))   { reg_scause := wdata & UInt((BigInt(1) << (xLen-1)) + 31) /* only implement 5 LSBs and MSB */ }
      when (decoded_addr(CSRs.sbadaddr)) { reg_sbadaddr := wdata(vaddrBitsExtended-1,0) }
      when (decoded_addr(CSRs.mideleg))  { reg_mideleg := wdata & delegable_interrupts }
      when (decoded_addr(CSRs.medeleg))  { reg_medeleg := wdata & delegable_exceptions }
      when (decoded_addr(CSRs.scounteren)) { reg_scounteren := wdata & UInt(delegable_counters) }
    }
    if (usingUser) {
      when (decoded_addr(CSRs.mcounteren)) { reg_mcounteren := wdata & UInt(delegable_counters) }
    }
    if (nBreakpoints > 0) {
      when (decoded_addr(CSRs.tselect)) { reg_tselect := wdata }

      val bp = reg_bp(reg_tselect)
      when (!bp.control.dmode || reg_debug) {
        when (decoded_addr(CSRs.tdata1)) {
          val newBPC = new BPControl().fromBits(wdata)
          val dMode = newBPC.dmode && reg_debug
          bp.control := newBPC
          bp.control.dmode := dMode
          bp.control.action := dMode && newBPC.action
        }
        when (decoded_addr(CSRs.tdata2)) { bp.address := wdata }
      }
    }
  }

  reg_mip <> io.interrupts
  reg_dcsr.debugint := io.interrupts.debug

  if (!usingVM) {
    reg_mideleg := 0
    reg_medeleg := 0
    reg_scounteren := 0
  }

  if (!usingUser) {
    reg_mcounteren := 0
  }

  reg_sptbr.asid := 0
  if (nBreakpoints <= 1) reg_tselect := 0
  if (nBreakpoints >= 1)
    reg_bp(nBreakpoints-1).control.chain := false
  for (bpc <- reg_bp map {_.control}) {
    bpc.ttype := bpc.tType
    bpc.maskmax := bpc.maskMax
    bpc.reserved := 0
    bpc.zero := 0
    bpc.h := false
    if (!usingVM) bpc.s := false
    if (!usingUser) bpc.u := false
    if (!usingVM && !usingUser) bpc.m := true
    when (reset) {
      bpc.action := false
      bpc.dmode := false
      bpc.r := false
      bpc.w := false
      bpc.x := false
    }
  }
  for (bp <- reg_bp drop nBreakpoints)
    bp := new BP().fromBits(0)

  def legalizePrivilege(priv: UInt): UInt =
    if (usingVM) Mux(priv === PRV.H, PRV.U, priv)
    else if (usingUser) Fill(2, priv(0))
    else PRV.M

  def trimPrivilege(priv: UInt): UInt =
    if (usingVM) priv
    else legalizePrivilege(priv)

  def writeCounter(lo: Int, ctr: WideCounter, wdata: UInt) = {
    if (xLen == 32) {
      val hi = lo + CSRs.mcycleh - CSRs.mcycle
      when (decoded_addr(lo)) { ctr := Cat(ctr(ctr.getWidth-1, 32), wdata) }
      when (decoded_addr(hi)) { ctr := Cat(wdata(ctr.getWidth-33, 0), ctr(31, 0)) }
    } else {
      when (decoded_addr(lo)) { ctr := wdata(ctr.getWidth-1, 0) }
    }
  }
  def formEPC(x: UInt) = ~(~x | Cat(!reg_misa('c'-'a'), UInt(1)))
  def isaStringToMask(s: String) = s.map(x => 1 << (x - 'A')).reduce(_|_)
}
