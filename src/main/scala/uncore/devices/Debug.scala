// See LICENSE.SiFive for license details.

package uncore.devices

import Chisel._
import junctions._
import util._
import regmapper._
import tile.XLen
import uncore.tilelink2._
import config._


/** Constant values used by both Debug Bus Response & Request
  */

object DMIConsts{

  def dmiDataSize = 32

  def dmiOpSize = 2
  def dmi_OP_NONE            = "b00".U
  def dmi_OP_READ            = "b01".U
  def dmi_OP_WRITE           = "b10".U

  def dmiRespSize = 2
  def dmi_RESP_SUCCESS     = "b00".U
  def dmi_RESP_FAILURE     = "b01".U
  def dmi_RESP_HW_FAILURE  = "b10".U
  // This is used outside this block
  // to indicate 'busy'.
  def dmi_RESP_RESERVED    = "b11".U

}

object DsbBusConsts {

  def sbAddrWidth = 12
  def sbIdWidth   = 10 

}

object DsbRegAddrs{

  // These may need to move around to be used by the serial interface.

  // These are used by the ROM.
  def HALTED       = 0x100
  def GOING        = 0x104
  def RESUMING     = 0x108
  def EXCEPTION    = 0x10C

  def GO           = 0x400

  def ROMBASE      = 0x800

  def WHERETO      = 0x900
  def ABSTRACT     = 0x910
  def PROGBUF      = 0xA00

  // This shows up in HartInfo
  def DATA         = 0xB00

  //Not implemented: Serial.
 
}

/** Enumerations used both in the hardware
  * and in the configuration specification.
  */

object DebugModuleAccessType extends scala.Enumeration {
  type DebugModuleAccessType = Value
  val Access8Bit, Access16Bit, Access32Bit, Access64Bit, Access128Bit = Value
}
import DebugModuleAccessType._

object DebugModuleHartStatus extends scala.Enumeration {
  type DebugModuleHartStatus = Value
  val Halted, Running, Unavailable, NonExistent = Value
}
import DebugModuleHartStatus._

object DebugAbstractCommandError extends scala.Enumeration {
  type DebugAbstractCommandError = Value
  val None, ErrBusy, ErrNotSupported, ErrException, ErrHaltResume = Value
}
import DebugAbstractCommandError._


object DebugAbstractCommandType extends scala.Enumeration {
  type DebugAbstractCommandType = Value
  val AccessRegister, QuickAccess  = Value
}
import DebugAbstractCommandType._



/** Parameters exposed to the top-level design, set based on
  * external requirements, etc.
  *
  *  This object checks that the parameters conform to the 
  *  full specification. The implementation which receives this
  *  object can perform more checks on what that implementation
  *  actually supports.
  *  nComponents : The number of components to support debugging.
  *  nDMIAddrSize : Size of the Debug Bus Address
  *  nAbstractDataWords: 
  *  nProgamBufferWords: 
  *  hasBusMaster: Whethr or not a bus master should be included
  *    The size of the accesses supported by the Bus Master. 
  *  nSerialPorts : Number of serial ports to instantiate
  *  authType : The Authorization Type
  *  Number of cycles to assert ndreset when pulsed. 
  **/


case class DebugModuleConfig (
  nComponents        : Int,
  nDMIAddrSize  : Int,
  nProgramBufferWords: Int,
  nAbstractDataWords : Int,
  hasBusMaster : Boolean,
  hasAccess128 : Boolean,
  hasAccess64  : Boolean,
  hasAccess32  : Boolean,
  hasAccess16  : Boolean,
  hasAccess8   : Boolean,
  nSerialPorts : Int,
  supportQuickAccess : Boolean,
  nNDResetCycles : Int
) {

  if (hasBusMaster == false){
    require (hasAccess128 == false)
    require (hasAccess64  == false)
    require (hasAccess32  == false)
    require (hasAccess16  == false)
    require (hasAccess8   == false)
  }

  require (nSerialPorts <= 8)

  require ((nDMIAddrSize >= 7) && (nDMIAddrSize <= 32))

  //TODO: Revisit these.
  private val maxComponents = nDMIAddrSize match {
    case 5 => (32*4)
    case 6 => (32*32)
    case 7 => (32*32)
  }
  require (nComponents > 0 && nComponents <= maxComponents)

  //TODO: Revisit.
  private val maxRam = nDMIAddrSize match {
    case 5 => (4 * 16)
    case 6 => (4 * 16)
    case 7 => (4 * 64)
  }

  require (nNDResetCycles > 0)

  // TODO: Check that quick access requirements are met.
}

class DefaultDebugModuleConfig (val ncomponents : Int, val xlen:Int)
    extends DebugModuleConfig(
      nComponents = ncomponents,
      nDMIAddrSize = 7,
      //TODO: what should these values be.
      nProgramBufferWords   =  8,
      nAbstractDataWords = xlen match{
        case 32  => 1 
        case 64  => 2
        case 128 => 4
      },
      hasBusMaster = false,
      hasAccess128 = false, 
      hasAccess64 = false, 
      hasAccess32 = false, 
      hasAccess16 = false, 
      hasAccess8 = false, 
      nSerialPorts = 0,
      nNDResetCycles = 1,
      supportQuickAccess = false
    )

case object DMKey extends Field[DebugModuleConfig]

// *****************************************
// Module Interfaces
// 
// *****************************************


/** Structure to define the contents of a Debug Bus Request
  */

class DMIReq(addrBits : Int) extends Bundle {
  val addr = UInt(addrBits.W)
  val data = UInt(DMIConsts.dmiDataSize.W)
  val op   = UInt(DMIConsts.dmiOpSize.W)

  override def cloneType = new DMIReq(addrBits).asInstanceOf[this.type]
}

/** Structure to define the contents of a Debug Bus Response
  */
class DMIResp( ) extends Bundle {
  val data = UInt(DMIConsts.dmiDataSize.W)
  val resp = UInt(DMIConsts.dmiRespSize.W)

}

/** Structure to define the top-level DMI interface 
  *  of DebugModule.
  *  DebugModule is the consumer of this interface.
  *  Therefore it has the 'flipped' version of this.
  */

class DMIIO(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val req = new  DecoupledIO(new DMIReq(p(DMKey).nDMIAddrSize))
  val resp = new DecoupledIO(new DMIResp).flip()
}

trait HasDebugModuleParameters {
  implicit val p: Parameters
  val cfg = p(DMKey)
}

/** Debug Module I/O, with the exclusion of the RegisterRouter
  *  Access interface.
  */

trait DebugModuleBundle extends Bundle with HasDebugModuleParameters {
  val dmi = new DMIIO()(p).flip()
  val debugInterrupts = Vec(cfg.nComponents, Bool()).asOutput
  val debugUnavail    = Vec(cfg.nComponents, Bool()).asInput
  val ndreset         = Bool(OUTPUT)
  val debugActive     = Bool(OUTPUT)
}

// *****************************************
// The Module 
// 
// *****************************************

/** Parameterized version of the Debug Module defined in the
  *  RISC-V Debug Specification 
  *  
  *  DebugModule is a slave to two masters:
  *    The Debug Bus (DMI) -- implemented as a generic Decoupled IO with request
  *                           and response channels
  *    The System Bus -- implemented as generic RegisterRouter
  *  
  *  DebugModule is responsible for holding registers, RAM, and ROM
  *      to support debug interactions, as well as driving interrupts
  *      to a configurable number of components in the system.
  *      It must also maintain a state machine to track the state of harts
  *      being debugged.
  *      It is also responsible for some reset lines.
  */

trait DebugModule extends Module with HasDebugModuleParameters with HasRegMap {

  val io: DebugModuleBundle

  //--------------------------------------------------------------
  // Import constants for shorter variable names
  //--------------------------------------------------------------

  import DMI_RegAddrs._
  import DsbRegAddrs._
  import DsbBusConsts._
  import DMIConsts._

  //--------------------------------------------------------------
  // Sanity Check Configuration For this implementation.
  //--------------------------------------------------------------

  //TODO: is this requirement still true.
  require (cfg.nComponents <= 128)
  require (cfg.nSerialPorts == 0)
  require (cfg.hasBusMaster == false)
  // ??? require((DMIConsts.dbRamWordBits % 8) == 0)

  //--------------------------------------------------------------
  // Register & Wire Declarations
  //--------------------------------------------------------------

  val debugIntRegs = Reg(init=Vec.fill(cfg.nComponents){Bool(false)})
  val haltedBitRegs  = Reg(init=Vec.fill(cfg.nComponents){Bool(false)})

  val dmiReq    = io.dmi.req.bits
  val dmiWrEn   = Wire(Bool())

  // --- regmapper outputs

  val hartHaltedWrEn       = Wire(Bool())
  val hartHaltedId         = Wire(UInt(sbIdWidth.W))
  val hartGoingWrEn        = Wire(Bool())
  val hartGoingId          = Wire(UInt(sbIdWidth.W))
  val hartResumingWrEn     = Wire(Bool())
  val hartResumingId       = Wire(UInt(sbIdWidth.W))
  val hartExceptionWrEn    = Wire(Bool())
  val hartExceptionId      = Wire(UInt(sbIdWidth.W))

  //--------------------------------------------------------------
  // DMI Registers 
  //--------------------------------------------------------------

  //----DMCONTROL

  val DMCONTROLReset = Wire(init = (new DMCONTROLFields()).fromBits(0.U))
  DMCONTROLReset.authenticated := true.B // Not implemented
  DMCONTROLReset.version       := 1.U
  val DMCONTROLReg = RegInit(DMCONTROLReset)

  val unavailVec = Wire(Vec(cfg.nComponents, Bool()))
  unavailVec := io.debugUnavail

  val DMCONTROLRdData = Wire(init = DMCONTROLReg)
  when (DMCONTROLReg.hartsel >= cfg.nComponents.U) {
    DMCONTROLRdData.hartstatus := DebugModuleHartStatus.NonExistent.id.U
  } .elsewhen (haltedBitRegs(DMCONTROLReg.hartsel)) {
    DMCONTROLRdData.hartstatus := DebugModuleHartStatus.Halted.id.U
  } .elsewhen(unavailVec(DMCONTROLReg.hartsel)) {
    DMCONTROLRdData.hartstatus := DebugModuleHartStatus.Unavailable.id.U
  } .otherwise {
    DMCONTROLRdData.hartstatus := DebugModuleHartStatus.Running.id.U
  }

  val DMCONTROLWrData = (new DMCONTROLFields()).fromBits(dmiReq.data)
  val DMCONTROLWrEn   = dmiWrEn & (dmiReq.addr === DMI_DMCONTROL)

  val dmactive = DMCONTROLReg.dmactive

  when (~dmactive) {
    DMCONTROLReg := DMCONTROLReset
  } .otherwise {
    DMCONTROLReg.reset     := DMCONTROLWrData.reset
    DMCONTROLReg.haltreq   := DMCONTROLWrData.haltreq
    DMCONTROLReg.resumereq := DMCONTROLWrData.resumereq
  }

  // Put this last to override its own effects.
  when (DMCONTROLWrEn) {
    DMCONTROLReg.dmactive := DMCONTROLWrData.dmactive
  }

  //----HARTINFO

  val HARTINFORdData = Wire (init = (new HARTINFOFields()).fromBits(0.U))
  HARTINFORdData.dataaccess  := true.B
  HARTINFORdData.datasize    := cfg.nAbstractDataWords.U
  HARTINFORdData.dataaddr    := DsbRegAddrs.DATA.U

  //----HALTSUM (and halted registers)
  val numHaltedStatus = ((cfg.nComponents - 1) / 32) + 1

  val haltedStatus   = Wire(Vec(numHaltedStatus, Bits(width = 32)))

  for (ii <- 0 until numHaltedStatus) {
    haltedStatus(ii) := Cat(haltedBitRegs.slice(ii * 32, (ii + 1) * 32).reverse)
  }

  //TODO: Make this more efficient with masks vs arithmetic.
  //TODO: Use something other than a magic number here.
  val dmiHaltedStatusIdx      = dmiReq.addr - 0x40.U
  val dmiHaltedStatusIdxValid = dmiHaltedStatusIdx < cfg.nComponents.U

  val haltedStatusRdData = haltedStatus(dmiHaltedStatusIdx)

  val haltedSummary = Cat(haltedStatus.map(_.orR).reverse)

  val HALTSUMRdData = (new HALTSUMFields()).fromBits(haltedSummary)

  //----ABSTRACTCS

  val ABSTRACTCSReset = Wire(init = (new ABSTRACTCSFields()).fromBits(0.U))
  ABSTRACTCSReset.datacount := cfg.nAbstractDataWords.U

  val ABSTRACTCSReg = RegInit(ABSTRACTCSReset)
  val ABSTRACTCSWrData = (new ABSTRACTCSFields()).fromBits(dmiReq.data)
  val ABSTRACTCSRdData = Wire(init = ABSTRACTCSReg)

  val ABSTRACTCSWrEnMaybe = dmiWrEn & (dmiReq.addr === DMI_ABSTRACTCS)

  val ABSTRACTCSWrEnLegal = Wire(init = false.B)
  val ABSTRACTCSWrEn      = ABSTRACTCSWrEnMaybe && ABSTRACTCSWrEnLegal

  val errorBusy        = Wire(init = false.B)
  val errorException   = Wire(init = false.B)
  val errorUnsupported = Wire(init = false.B)
  val errorHaltResume  = Wire(init = false.B)

  val abstractCSClearError = ABSTRACTCSWrEn && (ABSTRACTCSWrData.cmderr === 0.U)

  when(~dmactive){
    ABSTRACTCSReg := ABSTRACTCSReset
  }.otherwise {

    when (errorBusy){
      ABSTRACTCSReg.cmderr := DebugAbstractCommandError.ErrBusy.id.U
    }.elsewhen (errorException) {
      ABSTRACTCSReg.cmderr := DebugAbstractCommandError.ErrException.id.U
    }.elsewhen (errorUnsupported) {
      ABSTRACTCSReg.cmderr := DebugAbstractCommandError.ErrNotSupported.id.U
    }.elsewhen (errorHaltResume) {
      ABSTRACTCSReg.cmderr := DebugAbstractCommandError.ErrHaltResume.id.U
    }.otherwise {
      when (abstractCSClearError){
        ABSTRACTCSReg.cmderr := DebugAbstractCommandError.None.id.U
      }
    }

    when (ABSTRACTCSWrEn) {
      if (cfg.nAbstractDataWords > 0) ABSTRACTCSReg.autoexec0 := ABSTRACTCSWrData.autoexec0
      if (cfg.nAbstractDataWords > 1) ABSTRACTCSReg.autoexec1 := ABSTRACTCSWrData.autoexec1
      if (cfg.nAbstractDataWords > 2) ABSTRACTCSReg.autoexec2 := ABSTRACTCSWrData.autoexec2
      if (cfg.nAbstractDataWords > 3) ABSTRACTCSReg.autoexec3 := ABSTRACTCSWrData.autoexec3
      if (cfg.nAbstractDataWords > 4) ABSTRACTCSReg.autoexec4 := ABSTRACTCSWrData.autoexec4
      if (cfg.nAbstractDataWords > 5) ABSTRACTCSReg.autoexec5 := ABSTRACTCSWrData.autoexec5
      if (cfg.nAbstractDataWords > 6) ABSTRACTCSReg.autoexec6 := ABSTRACTCSWrData.autoexec6
      if (cfg.nAbstractDataWords > 7) ABSTRACTCSReg.autoexec7 := ABSTRACTCSWrData.autoexec7
    }
  }

  // For busy, see below state machine.
  val abstractCommandBusy = Wire(init = true.B)
  ABSTRACTCSRdData.busy := abstractCommandBusy

  //---- COMMAND

  val COMMANDReset = (new COMMANDFields()).fromBits(0.U)
  val COMMANDReg = RegInit(COMMANDReset)

  val COMMANDWrData       = (new COMMANDFields()).fromBits(dmiReq.data)
  val COMMANDWrEnMaybe    = dmiWrEn & (dmiReq.addr === DMI_COMMAND)
  val COMMANDWrEnLegal    = Wire(init = false.B)

  val COMMANDWrEn = COMMANDWrEnMaybe && COMMANDWrEnLegal
  val COMMANDRdData = COMMANDReg

  when (~dmactive) {
    COMMANDReg := COMMANDReset
  }.otherwise {
    when (COMMANDWrEn) {
      COMMANDReg := COMMANDWrData
    }
  }


  // --- Abstract Data

  val abstractDataWidth     = 32
  val abstractDataAddrWidth = log2Up(cfg.nAbstractDataWords)

  // These are byte addressible, s.t. the Processor can use
  // byte-addressible instructions to store to them.
  val abstractDataMem       = RegInit(Vec.fill(cfg.nAbstractDataWords*4){0.U(8.W)})

  val dmiAbstractDataIdx        = Wire(UInt(abstractDataAddrWidth.W))
  val dmiAbstractDataIdxValid   = Wire(Bool())
  val dmiAbstractDataRdData      = Wire (UInt(32.W))
  val dmiAbstractDataWrData      = Wire(UInt(32.W))

  val dmiAbstractDataOffset = log2Up(abstractDataWidth/8)

  // --- Program Buffer

  val PROGBUFCSRdData = Wire(init = (new PROGBUFCSFields).fromBits(0.U))
  PROGBUFCSRdData.progsize := cfg.nProgramBufferWords.U

  val programBufferDataWidth  = 32
  val programBufferAddrWidth = log2Up(cfg.nProgramBufferWords)

  val programBufferMem    = RegInit(Vec.fill(cfg.nProgramBufferWords*4){0.U(8.W)})

  val dmiProgramBufferIdx   = Wire(UInt(programBufferAddrWidth.W))
  val dmiProgramBufferIdxValid   = Wire(Bool())
  val dmiProgramBufferRdData = Wire (UInt(32.W))
  val dmiProgramBufferWrData = Wire(UInt(32.W))

  val dmiProgramBufferOffset = log2Up(programBufferDataWidth/8)


  //--------------------------------------------------------------
  // Interrupt Registers
  //--------------------------------------------------------------
  
  for (component <- 0 until cfg.nComponents) {
    io.debugInterrupts(component) := debugIntRegs(component)
  }

  // Halt request registers are written by write to DMCONTROL.haltreq
  // and cleared by writes to DMCONTROL.resumereq.
  
  for (component <- 0 until cfg.nComponents) {
    when (~dmactive) {
      debugIntRegs(component) := false.B
    }. otherwise {
      when (DMCONTROLWrEn) {
        when (DMCONTROLWrData.hartsel === component.U) {
          debugIntRegs(component) := (debugIntRegs(component) | DMCONTROLWrData.haltreq) &
          ~(DMCONTROLWrData.resumereq)
        }
      }
    }
  }

  //--------------------------------------------------------------
  // These bits are implementation-specific bits set
  // by harts executing code.
  //--------------------------------------------------------------

  for (component <- 0 until cfg.nComponents) {
    when (~dmactive) {
      haltedBitRegs(component) := false.B
    }.otherwise {
      when (hartHaltedWrEn) {
        when (hartHaltedId === component.U) {
          haltedBitRegs(component) := true.B
        }
      }.elsewhen (hartResumingWrEn) {
        when (hartResumingId === component.U) {
          haltedBitRegs(component) := false.B
        }
      }
    }
  }

 
  //--------------------------------------------------------------
  // Abstract Data Access (Debug Bus ... System Bus can override)
  //--------------------------------------------------------------

  //TODO: Make this more efficient with the final addresses to use masking
  // instead of actual comparisons.
  dmiAbstractDataIdx       := dmiReq.addr - DMI_DATA0
  dmiAbstractDataIdxValid  := (dmiReq.addr >= DMI_DATA0) && (dmiReq.addr <= (DMI_DATA0 + cfg.nAbstractDataWords.U))

  val dmiAbstractDataWrEn  = dmiAbstractDataIdxValid && dmiWrEn


  val dmiAbstractDataFields = List.tabulate(cfg.nAbstractDataWords) { ii =>
    val slice = abstractDataMem.slice(ii * 4, (ii+1)*4)
    slice.reduce[UInt]{ case (x: UInt, y: UInt) => Cat(y, x) }
  }

  when (dmiWrEn & dmiAbstractDataIdxValid) {
    for (ii <- 0 until 4) {
      abstractDataMem((dmiAbstractDataIdx << 2) + ii.U) := dmiReq.data((8*(ii+1)-1), (8*ii))
    }
  }

  dmiAbstractDataRdData := dmiAbstractDataFields(dmiAbstractDataIdx)

  //--------------------------------------------------------------
  // Program Buffer Access (Debug Bus ... System Bus can override)
  //--------------------------------------------------------------

  //TODO: Make this more efficient with the final addresses to use masking
  // instead of actual comparisons.

  dmiProgramBufferIdx       := dmiReq.addr - DMI_PROGBUF0
  dmiProgramBufferIdxValid  := (dmiReq.addr >= DMI_PROGBUF0) && dmiReq.addr <= (DMI_PROGBUF0 + cfg.nProgramBufferWords.U)

  val dmiProgramBufferFields = List.tabulate(cfg.nProgramBufferWords) { ii =>
    val slice = programBufferMem.slice(ii * 4, (ii+1)*4)
    slice.reduce[UInt]{ case (x: UInt, y: UInt) => Cat(y, x)}
  }

  dmiProgramBufferRdData := dmiProgramBufferFields(dmiProgramBufferIdx)

  when (dmiWrEn & dmiProgramBufferIdxValid) {
    for (ii <- 0 until 4) {
      programBufferMem((dmiProgramBufferIdx << 2) + ii.U) := dmiReq.data((8*(ii+1)-1), (8*ii))
    }
  }

  //--------------------------------------------------------------
  // DMI Access
  //--------------------------------------------------------------

  val dmiBusyReg = RegInit(false.B)

  // -----------------------------------------
  // DMI Access Read Mux
  val dmiRdData = Wire(UInt(DMIConsts.dmiDataSize.W))

  when     (dmiReq.addr === DMI_DMCONTROL)     {dmiRdData := DMCONTROLRdData.asUInt()}
    .elsewhen (dmiReq.addr === DMI_HARTINFO)   {dmiRdData := HARTINFORdData.asUInt()}
    .elsewhen (dmiReq.addr === DMI_HALTSUM)    {dmiRdData := HALTSUMRdData.asUInt()}
    .elsewhen (dmiReq.addr === DMI_ABSTRACTCS) {dmiRdData := ABSTRACTCSRdData.asUInt()}
    .elsewhen (dmiReq.addr === DMI_PROGBUFCS)  {dmiRdData := PROGBUFCSRdData.asUInt()}
    .elsewhen (dmiReq.addr === DMI_COMMAND)    {dmiRdData := COMMANDRdData.asUInt()}
    .elsewhen (dmiAbstractDataIdxValid)        {dmiRdData := dmiAbstractDataRdData}
    .elsewhen (dmiProgramBufferIdxValid)       {dmiRdData := dmiProgramBufferRdData}
    .elsewhen (dmiHaltedStatusIdxValid)        {dmiRdData := haltedStatusRdData}
    .otherwise {dmiRdData := 0.U}

  // There is no way to return failure without SB or Serial, which are not
  // implemented yet.


  // -----------------------------------------
  // DMI Access State Machine Decode (Combo)

  val dmiResult  = Wire(new DMIResp())

  dmiResult.resp := dmi_RESP_SUCCESS
  dmiResult.data := dmiRdData

  val dmiRespReg = Reg(new DMIResp())

  io.dmi.req.ready := (!dmiBusyReg) || (dmiBusyReg && io.dmi.resp.fire())

  io.dmi.resp.valid := dmiBusyReg
  io.dmi.resp.bits  := dmiRespReg

  dmiWrEn := (dmiReq.op === dmi_OP_WRITE) && io.dmi.req.fire()

  // -----------------------------------------
  // DMI Access State Machine Update (Seq)

  when (!dmiBusyReg) {
    when (io.dmi.req.fire()){
      dmiBusyReg := true.B
      dmiRespReg := dmiResult
    }
  } .otherwise {
    when (io.dmi.req.fire()){
      dmiBusyReg := true.B
      dmiRespReg := dmiResult
    }.elsewhen (io.dmi.resp.fire()){
      dmiBusyReg := false.B
    }
  }

  //--------------------------------------------------------------
  // Debug ROM
  //--------------------------------------------------------------

  // See the debug directory for contents and scripts to generate this.
  def debugRomContents : Array[Byte] = Array (
    0x6f, 0x00, 0xc0, 0x00, 0x6f, 0x00, 0xc0, 0x03, 0x6f, 0x00, 0x40, 0x02,
    0x0f, 0x00, 0xf0, 0x0f, 0x73, 0x10, 0x24, 0x7b, 0x73, 0x24, 0x40, 0xf1,
    0x23, 0x20, 0x80, 0x10, 0x03, 0x04, 0x04, 0x40, 0x63, 0x1a, 0x80, 0x00,
    0x73, 0x24, 0x40, 0xf1, 0x6f, 0xf0, 0x5f, 0xff, 0x23, 0x26, 0x00, 0x10,
    0x73, 0x00, 0x10, 0x00, 0x73, 0x24, 0x20, 0x7b, 0x23, 0x22, 0x00, 0x10,
    0x67, 0x00, 0x00, 0x90, 0x73, 0x10, 0x24, 0x7b, 0x73, 0x24, 0x40, 0xf1,
    0x23, 0x24, 0x80, 0x10, 0x73, 0x24, 0x20, 0x7b, 0x73, 0x00, 0x20, 0x7b
  ).map(_.toByte)

  val romRegFields  =  debugRomContents.map( x => RegField.r(8, (x.toInt & 0xFF).U))

  //--------------------------------------------------------------
  // "Variable" ROM Generation
  //--------------------------------------------------------------
  // f05ff06f                j       804 <resume>
  // 00c0006f                j       910 <abstract>
  // 0f80006f                j       a00 <prog_buffer>

  val goProgramBuffer = Wire(init = false.B)
  val goResume        = Wire(init = false.B)
  val goAbstract      = Wire(init = false.B)

  val whereToReg = RegInit(0.U(32.W))

  when (goProgramBuffer) {
    whereToReg := 0x0f80006fL.U    // j       a00 <prog_buffer>
  }.elsewhen (goResume) {
    whereToReg := 0xf05ff06fL.U    // j       804 <resume>
  }.elsewhen (goAbstract) {
    whereToReg := 0x00c0006fL.U    // j       910 <abstract>
  }

  val goReg            = Reg (init = false.B)
  when (goProgramBuffer | goResume | goAbstract) {
    goReg := true.B
  }.elsewhen (hartGoingWrEn){
    assert(hartGoingId === 0.U,
      "Unexpected 'GOING' hart: %x, expected %x", hartGoingId, DMCONTROLReg.hartsel)
    goReg := false.B
  }

  val goBytes = Wire(init = Vec.fill(cfg.nComponents){0.U(8.W)})
  goBytes(DMCONTROLReg.hartsel) := Cat(0.U(7.W), goReg)

  //----------------------------
  // Abstract Command Decoding & Generation
  //----------------------------

  val accessRegisterCommandWr  = Wire(init = (new ACCESS_REGISTERFields()).fromBits(COMMANDWrData.toBits()))
  val accessRegisterCommandReg = Wire(init = (new ACCESS_REGISTERFields()).fromBits(COMMANDWrData.toBits()))

  // TODO: Quick Access
  val quickAccessCommand    = Wire(init = (new QUICK_ACCESSFields()).fromBits(COMMANDReg.toBits()))

  class GeneratedI extends Bundle {
    val opcode = UInt(7.W)
    val rd     = UInt(5.W)
    val funct3 = UInt(3.W)
    val rs1    = UInt(5.W)
    val imm    = UInt(12.W)
  }

  class GeneratedS extends Bundle {
    val opcode = UInt(7.W)
    val immlo  = UInt(5.W)
    val funct3 = UInt(3.W)
    val rs1    = UInt(5.W)
    val rs2    = UInt(5.W)
    val immhi  = UInt(7.W)
  }

  val abstractGeneratedReg = Reg(UInt(32.W))
  val abstractGeneratedI = Wire(new GeneratedI())
  val abstractGeneratedS = Wire(new GeneratedS())

  abstractGeneratedI.opcode := ((new GeneratedI()).fromBits(rocket.Instructions.LW.value.U)).opcode
  abstractGeneratedI.rd     := (accessRegisterCommandReg.regno & 0x1F.U) // TODO: refuse to do this for CSRs/FPRs
  abstractGeneratedI.funct3 := accessRegisterCommandReg.size
  abstractGeneratedI.rs1    := 0.U //addr
  abstractGeneratedI.imm    := DATA.U

  abstractGeneratedS.opcode := ((new GeneratedI()).fromBits(rocket.Instructions.SW.value.U)).opcode
  abstractGeneratedS.immlo  := (DATA & 0x1F).U
  abstractGeneratedS.funct3 := accessRegisterCommandReg.size
  abstractGeneratedS.rs1    := 0.U // addr
  abstractGeneratedS.rs2    := (accessRegisterCommandReg.regno & 0x1F.U) // TODO: refuse to do this for CSRs/FPRs
  abstractGeneratedS.immhi  := (DATA >> 5).U

  when (goAbstract) {
    when (accessRegisterCommandReg.write) {
      // To write a register, we need to do LW.
      abstractGeneratedReg := abstractGeneratedI.asUInt()
    }.otherwise {
      // To read a register, we need to do SW.
      abstractGeneratedReg := abstractGeneratedS.asUInt()
    }
  }

  val ebreakInstruction = Wire(rocket.Instructions.EBREAK.value.U)

  //--------------------------------------------------------------
  // System Bus Access
  //--------------------------------------------------------------

  // Local reg mapper function : Notify when written, but give the value as well.
  def wValue (n: Int, value: UInt, set: Bool) : RegField = {
    RegField(n, value, RegWriteFn((valid, data) => {set := valid ; value := data; Bool(true)}))
  }

  regmap(
    // This memory is writable.
    HALTED      -> Seq(wValue(sbIdWidth, hartHaltedId, hartHaltedWrEn)),
    GOING       -> Seq(wValue(sbIdWidth, hartGoingId,  hartGoingWrEn)),
    RESUMING    -> Seq(wValue(sbIdWidth, hartResumingId,  hartResumingWrEn)),
    EXCEPTION   -> Seq(wValue(sbIdWidth, hartExceptionId,  hartExceptionWrEn)),
    DATA        -> abstractDataMem.map(x => RegField(8, x)),
    PROGBUF     -> programBufferMem.map(x => RegField(8, x)),

    // These sections are read-only.
    ROMBASE     -> romRegFields,
    GO          -> goBytes.map(x => RegField.r(8, x)),   
    WHERETO     -> Seq(RegField.r(32, whereToReg)),
    ABSTRACT    -> Seq(RegField.r(32, abstractGeneratedReg), RegField.r(32, ebreakInstruction))
  )

  //--------------------------------------------------------------
  // Abstract Command State Machine
  //--------------------------------------------------------------

  object CtrlState extends scala.Enumeration {
    type CtrlState = Value
    val Waiting, Generate, PreExec, Abstract, PostExec = Value

    def apply( t : Value) : UInt = {
      t.id.U(log2Up(values.size).W)
    }
  }
  import CtrlState._

  val ctrlStateReg = RegInit(CtrlState(Waiting))

  // Combo
  val hartHalted   = (DMCONTROLRdData.hartstatus === DebugModuleHartStatus.Halted.id.U)
  val ctrlStateNxt = Wire(init = ctrlStateReg)
  val autoexecVec  = Wire(init = Vec.fill(8){false.B})

  if (cfg.nAbstractDataWords > 0) autoexecVec(0) := (dmiAbstractDataIdx === 0.U) && dmiAbstractDataWrEn && ABSTRACTCSReg.autoexec0
  if (cfg.nAbstractDataWords > 1) autoexecVec(1) := (dmiAbstractDataIdx === 1.U) && dmiAbstractDataWrEn && ABSTRACTCSReg.autoexec1
  if (cfg.nAbstractDataWords > 2) autoexecVec(2) := (dmiAbstractDataIdx === 2.U) && dmiAbstractDataWrEn && ABSTRACTCSReg.autoexec2
  if (cfg.nAbstractDataWords > 3) autoexecVec(3) := (dmiAbstractDataIdx === 3.U) && dmiAbstractDataWrEn && ABSTRACTCSReg.autoexec3
  if (cfg.nAbstractDataWords > 4) autoexecVec(4) := (dmiAbstractDataIdx === 4.U) && dmiAbstractDataWrEn && ABSTRACTCSReg.autoexec4
  if (cfg.nAbstractDataWords > 5) autoexecVec(5) := (dmiAbstractDataIdx === 5.U) && dmiAbstractDataWrEn && ABSTRACTCSReg.autoexec5
  if (cfg.nAbstractDataWords > 6) autoexecVec(6) := (dmiAbstractDataIdx === 6.U) && dmiAbstractDataWrEn && ABSTRACTCSReg.autoexec6
  if (cfg.nAbstractDataWords > 7) autoexecVec(7) := (dmiAbstractDataIdx === 7.U) && dmiAbstractDataWrEn && ABSTRACTCSReg.autoexec7

  val autoexec = autoexecVec.reduce(_ || _)

  //------------------------

  // DMI Register Control and Status
  abstractCommandBusy := (ctrlStateReg != CtrlState(Waiting))

  //TODO: What are we allowed to write in the event of an error?
  // The spec says not command will be 'started', but not that it won't
  // be written.
  // This implementation allows it to be written but not started, but
  // not sure that is really the best design.
  ABSTRACTCSWrEnLegal := (ctrlStateReg === CtrlState(Waiting))
  COMMANDWrEnLegal    := (ctrlStateReg === CtrlState(Waiting))

  errorBusy := (ABSTRACTCSWrEnMaybe && ~ABSTRACTCSWrEnLegal) ||
               (COMMANDWrEnMaybe && ~COMMANDWrEnLegal)


  // TODO: Other Commands
  val commandWrIsAccessRegister  = (COMMANDWrData.cmdtype === DebugAbstractCommandType.AccessRegister.id.U)
  val commandRegIsAccessRegister = (COMMANDReg.cmdtype === DebugAbstractCommandType.AccessRegister.id.U)

  val commandWrIsUnsupported = Wire(init = true.B)
  val commandWrBadHaltResume = Wire(init = true.B)
  when (commandWrIsAccessRegister) {
    when ((accessRegisterCommandWr.regno >= 0x1000.U && accessRegisterCommandWr.regno <= 0x101F.U)){
      commandWrIsUnsupported := false.B
      commandWrBadHaltResume := ~hartHalted
    }
  }
  val commandRegIsUnsupported = Wire(init = true.B)
  val commandRegBadHaltResume = Wire(init = true.B)
  when (commandRegIsAccessRegister) {
    when ((accessRegisterCommandReg.regno < 0x1000.U || accessRegisterCommandReg.regno > 0x101F.U)){
      commandRegIsUnsupported := false.B
      commandRegBadHaltResume := ~hartHalted
    }
  }

  errorUnsupported := (COMMANDWrEn && commandWrIsUnsupported) || (autoexec && commandRegIsUnsupported)
  errorHaltResume  := (COMMANDWrEn && commandWrBadHaltResume) || (autoexec && commandRegBadHaltResume)

  val unclearedError = (ABSTRACTCSReg.cmderr != DebugAbstractCommandError.None.id.U)

  // TODO: Handle Other Command Types.
  val wrAccessRegisterCommand  = COMMANDWrEn && commandWrIsAccessRegister  && ~commandWrIsUnsupported  && ~commandWrBadHaltResume  && ~unclearedError
  val regAccessRegisterCommand = autoexec    && commandRegIsAccessRegister && ~commandRegIsUnsupported && ~commandRegBadHaltResume && ~unclearedError

  //------------------------
  // Variable ROM STATE MACHINE
  // -----------------------

  when (ctrlStateReg === CtrlState(Waiting)){

    when (wrAccessRegisterCommand || regAccessRegisterCommand) {
      ctrlStateNxt := CtrlState(Generate)
    }

  }.elsewhen (ctrlStateReg === CtrlState(Generate)){

    // We use this state to ensure that the COMMAND has been
    // registered by the time that we need to use it, to avoid
    // generating it directly from the COMMANDWrData.
    when (accessRegisterCommandReg.preexec) {
      ctrlStateNxt    := CtrlState(PreExec)
      goProgramBuffer := true.B
    }.otherwise {
      ctrlStateNxt := CtrlState(Abstract)
      goAbstract := true.B
    }
  }.elsewhen (ctrlStateReg === CtrlState(PreExec)) {

    // We can't just look at 'hartHalted' here, because
    // hartHaltedWrEn is overloaded to mean 'got an ebreak'
    // which may have happened when we were already halted.
    when(hartHaltedWrEn && (hartHaltedId === DMCONTROLReg.hartsel)){
      ctrlStateNxt := CtrlState(Abstract)
      goAbstract := true.B
    }
    when(hartExceptionWrEn) {
      assert(hartExceptionId === 0.U,
        "Unexpected 'EXCEPTION' hart, %x, expected %x", hartExceptionId, DMCONTROLReg.hartsel)
      ctrlStateNxt := CtrlState(Waiting)
      errorException := true.B
    }
  }.elsewhen (ctrlStateReg === CtrlState(Abstract)) {
  
    // We can't just look at 'hartHalted' here, because
    // hartHaltedWrEn is overloaded to mean 'got an ebreak'
    // which may have happened when we were already halted.
    when(hartHaltedWrEn && (hartHaltedId === DMCONTROLReg.hartsel)){
      when (accessRegisterCommandReg.postexec) {
        ctrlStateNxt := CtrlState(PostExec)
        goProgramBuffer := true.B
      }.otherwise {
        ctrlStateNxt := CtrlState(Waiting)
      }
    }

    when(hartExceptionWrEn) {
      assert(hartExceptionId === 0.U, 
        "Unexpected 'EXCEPTION' hart, %x, expected %x", hartExceptionId, DMCONTROLReg.hartsel)
      ctrlStateNxt := CtrlState(Waiting)
      errorUnsupported := true.B
    }

  }.elsewhen (ctrlStateReg === CtrlState(PostExec)) {

    // We can't just look at 'hartHalted' here, because
    // hartHaltedWrEn is overloaded to mean 'got an ebreak'
    // which may have happened when we were already halted.
    when(hartHaltedWrEn && (hartHaltedId === DMCONTROLReg.hartsel)){
      ctrlStateNxt := CtrlState(Waiting)
    }

    when(hartExceptionWrEn) {
      assert(hartExceptionId === 0.U,
        "Unexpected 'EXCEPTION' hart, %x, expected %x", hartExceptionId, DMCONTROLReg.hartsel)
      ctrlStateNxt := CtrlState(Waiting)
      errorException := true.B
    }
  }

  // Sequential
  when (!dmactive) {
    ctrlStateReg := CtrlState(Waiting)
  }.otherwise {
    ctrlStateReg := ctrlStateNxt
  }

  //--------------------------------------------------------------
  // Misc. Outputs
  //--------------------------------------------------------------

  // TODO
  io.ndreset   := false.B

  io.debugActive := dmactive

}

/** Create a concrete TL2 Slave for the DebugModule RegMapper interface.
  *  
  */
class TLDebugModule(address: BigInt = 0)(implicit p: Parameters)
  extends TLRegisterRouter(address, beatBytes=p(XLen)/8, executable=true)(
  new TLRegBundle((), _ )    with DebugModuleBundle)(
  new TLRegModule((), _, _)  with DebugModule)


/** Synchronizers for DMI
  *  
  */


class AsyncDMIIO(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val req  = new AsyncBundle(1, new DMIReq(p(DMKey).nDMIAddrSize))
  val resp = new AsyncBundle(1, new DMIResp).flip
}

object FromAsyncDMI
{
  def apply(x: AsyncDMIIO) = {
    val out = Wire(new DMIIO()(x.p))
    out.req <> FromAsyncBundle(x.req)
    x.resp <> ToAsyncBundle(out.resp, 1)
    out
  }
}

object ToAsyncDMI
{
  def apply(x: DMIIO) = {
    val out = Wire(new AsyncDMIIO()(x.p))
    out.req <> ToAsyncBundle(x.req, 1)
    x.resp <> FromAsyncBundle(out.resp)
    out
  }
}

object AsyncDMICrossing {
  // takes from_source from the 'from' clock domain to the 'to' clock domain
  def apply(from_clock: Clock, from_reset: Bool, from_source: DMIIO, to_clock: Clock, to_reset: Bool, depth: Int = 1, sync: Int = 3) = {
    val to_sink = Wire(new DMIIO()(from_source.p))
    to_sink.req <> AsyncDecoupledCrossing(from_clock, from_reset, from_source.req, to_clock, to_reset, depth, sync)
    from_source.resp <> AsyncDecoupledCrossing(to_clock, to_reset, to_sink.resp, from_clock, from_reset, depth, sync)
    to_sink // is now to_source
  }
}

object AsyncDMIFrom { // OutsideClockDomain
  // takes from_source from the 'from' clock domain and puts it into your clock domain
  def apply(from_clock: Clock, from_reset: Bool, from_source: DMIIO, depth: Int = 1, sync: Int = 3): DMIIO = {
    val scope = AsyncScope()
    AsyncDMICrossing(from_clock, from_reset, from_source, scope.clock, scope.reset, depth, sync)
  }
}

object AsyncDMITo { // OutsideClockDomain
  // takes source from your clock domain and puts it into the 'to' clock domain
  def apply(to_clock: Clock, to_reset: Bool, source: DMIIO, depth: Int = 1, sync: Int = 3): DMIIO = {
    val scope = AsyncScope()
    AsyncDMICrossing(scope.clock, scope.reset, source, to_clock, to_reset, depth, sync)
  }
}
