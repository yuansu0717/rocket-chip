// See LICENSE.SiFive for license details.

package rocketchip

import Chisel._
import uncore.devices._
import junctions._
import util._
import config._
import jtag._
import uncore.devices.{DMIConsts, DMIReq, DMIResp}

case object IncludeJtagDTM extends Field[Boolean]

case class JtagDTMConfig (
  idcodeVersion    : Int,      // chosen by manuf.
  idcodePartNum    : Int,      // Chosen by manuf.
  idcodeManufId    : Int,      // Assigned by JEDEC
  debugIdleCycles  : Int)

case object JtagDTMKey extends Field[JtagDTMConfig]

class JtagDTMKeyDefault extends JtagDTMConfig(
  idcodeVersion = 0,
  idcodePartNum = 0,
  idcodeManufId = 0,
  debugIdleCycles = 5) // Reasonable guess for synchronization.

object dtmJTAGAddrs {
  def IDCODE       = 0x1
  def DTM_INFO     = 0x10
  def DMI_ACCESS = 0x11
}

class DMIAccessUpdate(addrBits: Int) extends Bundle {
  val addr = UInt(width = addrBits)
  val data = UInt(width = DMIConsts.dmiDataSize)
  val op = UInt(width = DMIConsts.dmiOpSize)

  override def cloneType = new DMIAccessUpdate(addrBits).asInstanceOf[this.type]
}

class DMIAccessCapture(addrBits: Int) extends Bundle {
  val addr = UInt(width = addrBits)
  val data = UInt(width = DMIConsts.dmiDataSize)
  val resp = UInt(width = DMIConsts.dmiRespSize)

  override def cloneType = new DMIAccessCapture(addrBits).asInstanceOf[this.type]

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

class DebugTransportModuleJTAG(debugAddrBits: Int, c: JtagDTMConfig)
  (implicit val p: Parameters) extends Module  {

  val io = new Bundle {
    val dmi = new DMIIO()(p)
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

  val busyResp = Wire(new DMIAccessCapture(debugAddrBits))
  val nonbusyResp = Wire(new DMIAccessCapture(debugAddrBits))

  val dmiReqReg  = Reg(new DMIReq(debugAddrBits))
  val dmiReqValidReg = Reg(init = Bool(false));

  val dmiStatus = Wire(UInt(width = 2))

  //--------------------------------------------------------
  // DTM Info Chain Declaration

  dmiStatus := Cat(stickyNonzeroRespReg, stickyNonzeroRespReg | stickyBusyReg)

  dtmInfo.debugVersion   := 1.U // This implements version 1 of the spec.
  dtmInfo.debugAddrBits  := UInt(debugAddrBits)
  dtmInfo.dmiStatus     := dmiStatus
  dtmInfo.dmiIdleCycles := UInt(c.debugIdleCycles)
  dtmInfo.reserved0      := 0.U
  dtmInfo.dmireset      := false.B // This is write-only
  dtmInfo.reserved1      := 0.U

  val dtmInfoChain = Module (CaptureUpdateChain(gen = new DTMInfo()))
  dtmInfoChain.io.capture.bits := dtmInfo

  //--------------------------------------------------------
  // Debug Access Chain Declaration

   val dmiAccessChain = Module(CaptureUpdateChain(genCapture = new DMIAccessCapture(debugAddrBits),
     genUpdate = new DMIAccessUpdate(debugAddrBits)))

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
    when (dmiAccessChain.io.update.valid) {
      skipOpReg := Bool(false)
      downgradeOpReg := Bool(false)
    }
    when (dmiAccessChain.io.capture.capture) {
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

   dmiAccessChain.io.capture.bits := Mux(busy, busyResp, nonbusyResp)
   when (dmiAccessChain.io.update.valid) {
     skipOpReg := Bool(false)
     downgradeOpReg := Bool(false)
   }
   when (dmiAccessChain.io.capture.capture) {
       skipOpReg := busy
       downgradeOpReg := (!busy & nonzeroResp)
       stickyBusyReg := busy
       stickyNonzeroRespReg <= nonzeroResp
   }

  //--------------------------------------------------------
  // Drive Ready Valid Interface

   when (dmiAccessChain.io.update.valid) {
     when (skipOpReg) {
       // Do Nothing
     }.otherwise {
       when (downgradeOpReg) {
         dmiReqReg.addr := UInt(0)
         dmiReqReg.data := UInt(0)
         dmiReqReg.op   := UInt(0)
       }.otherwise {
         dmiReqReg := dmiAccessChain.io.update.bits
       }
       dmiReqValidReg := Bool(true)
     }
   }.otherwise {
     when (io.dmi.req.ready) {
       dmiReqValidReg := Bool(false)
     }
   }

  io.dmi.resp.ready := dmiAccessChain.io.capture.capture
  io.dmi.req.valid := dmiReqValidReg

  // This is a name-based, not type-based assignment. Do these still work?
  io.dmi.req.bits := dmiReqReg

  //--------------------------------------------------------
  // Actual JTAG TAP

  val tapIO = JtagTapGenerator(irLength = 5,
    instructions = Map(dtmJTAGAddrs.DMI_ACCESS -> dmiAccessChain,
                       dtmJTAGAddrs.DTM_INFO   -> dtmInfoChain),
    idcode = Some((dtmJTAGAddrs.IDCODE, JtagIdcode(c.idcodeVersion, c.idcodePartNum, c.idcodeManufId))))

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

  // io.DMIIO <-> Sync <-> DMIIO <-> DTM

  val io = new Bundle {
    // This should be flip.
    val jtag = Flipped(new JTAGIO())
    val debug = new AsyncDMIIO
    val fsmReset = Bool(OUTPUT)
    val jtagPOReset = Bool(INPUT)
  }

  val jtag_dtm = Module(new DebugTransportModuleJTAG(
    debugAddrBits  = p(DMKey).nDMIAddrSize,
    c = p(JtagDTMKey))(p))

  jtag_dtm.io.jtag <> io.jtag
  jtag_dtm.io.jtagPOReset := io.jtagPOReset

  val io_dmi = Wire (new DMIIO)
  io.debug <> ToAsyncDMI(io_dmi)

  io_dmi <> jtag_dtm.io.dmi

  io.fsmReset := jtag_dtm.io.fsmReset
}
