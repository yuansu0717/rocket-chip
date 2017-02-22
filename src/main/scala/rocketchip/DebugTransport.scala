// See LICENSE.SiFive for license details.

package rocketchip

import Chisel._
import uncore.devices._
import junctions._
import util._
import config._
import jtag._
import uncore.devices.{DbBusConsts, DebugBusReq, DebugBusResp}

case object IncludeJtagDTM extends Field[Boolean]

object dtmJTAGAddrs {
  def IDCODE       = 0x1
  def DTM_INFO     = 0x10
  def DEBUG_ACCESS = 0x11
}

class DebugAccessUpdate(addrBits: Int) extends Bundle {
  val addr = UInt(width = addrBits)
  val data = UInt(width = DbBusConsts.dbDataSize)
  val op = UInt(width = DbBusConsts.dbOpSize)

  override def cloneType = new DebugAccessUpdate(addrBits).asInstanceOf[this.type]
}

class DebugAccessCapture(addrBits: Int) extends Bundle {
  val addr = UInt(width = addrBits)
  val data = UInt(width = DbBusConsts.dbDataSize)
  val resp = UInt(width = DbBusConsts.dbRespSize)

  override def cloneType = new DebugAccessCapture(addrBits).asInstanceOf[this.type]

}

class DTMInfo extends Bundle {
  val reserved1 = UInt(15.W)
  val dmireset = Bool()
  val reserved0 = UInt(1.W)
  val dmiIdleCycles = UInt(3.W)
  val dmiStatus = UInt(2.W)
  val debugAddrBits = UInt(6.W)
  val debugVersion = UInt(4.W)
}

class DebugTransportModuleJTAG(
  idcodeVersion    : Int = 1,
  idcodePartNum    : Int, // 0xE31, // Reference to Freedom Everywhere Coreplex
  idcodeManufId    : Int, // Assigned by JEDEC
  debugAddrBits    : Int = 5,  // Spec allows 5-7
  debugIdleCycles  : Int = 5
) (implicit val p: Parameters) extends Module  {

  val io = new Bundle {
    val dmi = new DebugBusIO()(p)
    val jtag = Flipped(new JTAGIO())
    val jtagPOReset = Bool(INPUT)
    val fsmReset = Bool(OUTPUT)
  }

  //--------------------------------------------------------
  // Reg and Wire Declarations

  val dtmInfo = Wire(new DTMInfo)

  val busyReg = Reg(init = Bool(false))
  val stickyBusyReg = Reg(init = Bool(false))
  val stickyNonzeroRespReg = Reg(init = Bool(false))

  val skipOpReg = Reg(init = Bool(false)) // Skip op because we're busy
  val downgradeOpReg = Reg(init = Bool(false)) // downgrade op because prev. failed.

  val busy = Wire(Bool())
  val nonzeroResp = Wire(Bool())

  val busyResp = Wire(new DebugAccessCapture(debugAddrBits))
  val nonbusyResp = Wire(new DebugAccessCapture(debugAddrBits))

  val dmiReqReg  = Reg(new DebugBusReq(debugAddrBits))
  val dmiReqValidReg = Reg(init = Bool(false));

  val dmiStatus = Wire(UInt(width = 2))

  //--------------------------------------------------------
  // DTM Info Chain Declaration

  dmiStatus := Cat(stickyNonzeroRespReg, stickyNonzeroRespReg | stickyBusyReg)

  dtmInfo.debugVersion   := 1.U // This implements version 1 of the spec.
  dtmInfo.debugAddrBits  := UInt(debugAddrBits)
  dtmInfo.dmiStatus     := dmiStatus
  dtmInfo.dmiIdleCycles := UInt(debugIdleCycles)
  dtmInfo.reserved0      := 0.U
  dtmInfo.dmireset      := false.B // This is write-only
  dtmInfo.reserved1      := 0.U

  val dtmInfoChain = Module (CaptureUpdateChain(gen = new DTMInfo()))
  dtmInfoChain.io.capture.bits := dtmInfo

  //--------------------------------------------------------
  // Debug Access Chain Declaration

   val debugAccessChain = Module(CaptureUpdateChain(genCapture = new DebugAccessCapture(debugAddrBits),
     genUpdate = new DebugAccessUpdate(debugAddrBits)))

  //--------------------------------------------------------
  // Debug Access Support

  // Busy Register. We become busy when we first try to send a request.
  // We stop being busy when we accept a response.

  when (io.dmi.req.valid) {
    busyReg <= Bool(true)
  }
  when (io.dmi.resp.fire()) {
    busyReg <= Bool(false)
  }

  // We are busy during a given CAPTURE
  // if we haven't received a valid response yet or if we
  // were busy last time without a reset.
  // busyReg will still be set when we check it,
  // so the logic for checking busy looks ahead.
  busy := (busyReg & !io.dmi.resp.valid) | stickyBusyReg;

  // Downgrade/Skip. We make the decision to downgrade or skip
  // during every CAPTURE_DR, and use the result in UPDATE_DR.
  // The sticky versions are reset by write to dmiReset in DTM_INFO.
    when (debugAccessChain.io.update.valid) {
      skipOpReg := Bool(false)
      downgradeOpReg := Bool(false)
    }
    when (debugAccessChain.io.capture.capture) {
      skipOpReg := busy
      downgradeOpReg := (!busy & nonzeroResp)
      stickyBusyReg := busy
      stickyNonzeroRespReg <= nonzeroResp
    }
  when (dtmInfoChain.io.update.valid) {
    when (dtmInfoChain.io.update.bits.dmireset) {
      stickyNonzeroRespReg := Bool(false)
      stickyBusyReg := Bool(false)
    }
  }

  // Especially for the first request, we must consider dtmResp.valid,
  // so that we don't consider junk in the FIFO to be an error response.
  // The current specification says that any non-zero response is an error.
  nonzeroResp := stickyNonzeroRespReg | (io.dmi.resp.valid & (io.dmi.resp.bits.resp != UInt(0)))

  busyResp.addr  := UInt(0)
  busyResp.resp  := UInt(0)
  busyResp.data  := UInt(0)

  nonbusyResp.addr := dmiReqReg.addr
  nonbusyResp.resp := io.dmi.resp.bits.resp
  nonbusyResp.data := io.dmi.resp.bits.data

  //--------------------------------------------------------
  // Debug Access Chain Implementation

   debugAccessChain.io.capture.bits := Mux(busy, busyResp, nonbusyResp)
   when (debugAccessChain.io.update.valid) {
     skipOpReg := Bool(false)
     downgradeOpReg := Bool(false)
   }
   when (debugAccessChain.io.capture.capture) {
       skipOpReg := busy
       downgradeOpReg := (!busy & nonzeroResp)
       stickyBusyReg := busy
       stickyNonzeroRespReg <= nonzeroResp
   }

  //--------------------------------------------------------
  // Drive Ready Valid Interface

   when (debugAccessChain.io.update.valid) {
     when (skipOpReg) {
       // Do Nothing
     }.otherwise {
       when (downgradeOpReg) {
         dmiReqReg.addr := UInt(0)
         dmiReqReg.data := UInt(0)
         dmiReqReg.op   := UInt(0)
       }.otherwise {
         dmiReqReg := debugAccessChain.io.update.bits
       }
       dmiReqValidReg := Bool(true)
     }
   }.otherwise {
     when (io.dmi.req.ready) {
       dmiReqValidReg := Bool(false)
     }
   }

  io.dmi.resp.ready := debugAccessChain.io.capture.capture
  io.dmi.req.valid := dmiReqValidReg

  // This is a name-based, not type-based assignment. Do these still work?
  io.dmi.req.bits := dmiReqReg

  //--------------------------------------------------------
  // Actual JTAG TAP

  val tapIO = JtagTapGenerator(irLength = 5,
    instructions = Map(dtmJTAGAddrs.DEBUG_ACCESS -> debugAccessChain,
                       dtmJTAGAddrs.DTM_INFO -> dtmInfoChain),
    idcode = Some((dtmJTAGAddrs.IDCODE, JtagIdcode(idcodeVersion, idcodePartNum, idcodeManufId))))

  tapIO.jtag <> io.jtag

  tapIO.control.jtagPOReset := io.jtagPOReset

  //--------------------------------------------------------
  // Reset Generation (this is fed back to us by the instantiating module,
  // and is used to reset the debug registers).

  io.fsmReset := tapIO.output.reset

}

/*  JTAG-based Debug Transport Module
 *  and synchronization logic.
 *  
 *  This implements JTAG interface described
 *  in the RISC-V Debug Specification
 *
 * This Module is currently a
 *  wrapper around a JTAG implementation
 *  of the Debug Transport Module.
 *  This is black-boxed because
 *  Chisel doesn't currently support:
 *    - Negative Edge Clocking
 *    - Asynchronous Resets
 *   (The tristate requirements of JTAG are exported from the 
 *    Chisel domain with the DRV_TDO signal).
 *
 *  The 'TRST' input is used to asynchronously
 *  reset the JTAG TAP. 
 *  This design requires that TRST be
 *  synchronized to TCK (for de-assert) outside
 *  of this module. 
 * 
 *  TRSTn is not a required input. If unused, it 
 *  should be tied high.
 *  
 *  clock and reset of this block should be  TCK and dtmReset
 */

class JtagDTMWithSync(implicit val p: Parameters) extends Module {

  // io.DebugBusIO <-> Sync <-> DebugBusIO <-> DTM

  val io = new Bundle {
    // This should be flip.
    val jtag = Flipped(new JTAGIO())
    val debug = new AsyncDebugBusIO
    val fsmReset = Bool(OUTPUT)
    val jtagPOReset = Bool(INPUT)
  }

  val jtag_dtm = Module(new DebugTransportModuleJTAG(
    debugAddrBits  = p(DMKey).nDebugBusAddrSize,
    idcodePartNum  = 0xE31,
    idcodeManufId  = 0x489,
    debugIdleCycles = 5)(p))

  jtag_dtm.io.jtag <> io.jtag
  jtag_dtm.io.jtagPOReset := io.jtagPOReset

  val io_debug_bus = Wire (new DebugBusIO)
  io.debug <> ToAsyncDebugBus(io_debug_bus)

  io_debug_bus <> jtag_dtm.io.dmi

  io.fsmReset := jtag_dtm.io.fsmReset
}
