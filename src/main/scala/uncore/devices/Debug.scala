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
  def GOING        = 0x10C
  def RESUMING     = 0x110
  def EXCEPTION    = 0x114

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

  //TODO: is this requiredment still true.
  require (cfg.nComponents <= 128)
  require (cfg.nSerialPorts == 0)
  require (cfg.hasBusMaster == false)
  // ??? require((DMIConsts.dbRamWordBits % 8) == 0)

  //--------------------------------------------------------------
  // Register & Wire Declarations
  //--------------------------------------------------------------

  val debugIntRegs = Reg(init=Vec.fill(cfg.nComponents){Bool(false)})
  val haltedBitRegs  = Reg(init=Vec.fill(cfg.nComponents){Bool(false)})

  val dmiReq = io.dmi.req.bits
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

  val DMCONTROLReset = (new DMCONTROLFields()).fromBits(0.U)
  DMCONTROLReset.authenticated := true.B // Not implemented
  DMCONTROLReset.version       := 1.U
  val DMCONTROLReg = RegInit(DMCONTROLReset)

  val unavailVec = Wire(Vec(cfg.nComponents, Bool()))
  unavailVec := io.debugUnavail

  val DMCONTROLRdData = DMCONTROLReg
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

  val HARTINFORdData = (new HARTINFOFields()).fromBits(0.U)
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

  val ABSTRACTCSReset = (new ABSTRACTCSFields()).fromBits(0.U)
  ABSTRACTCSReset.datacount := cfg.nAbstractDataWords.U

  val ABSTRACTCSReg = RegInit(ABSTRACTCSReset)
  val ABSTRACTCSWrData = (new ABSTRACTCSFields()).fromBits(dmiReq.data)
  val ABSTRACTCSRdData = ABSTRACTCSReg

  val ABSTRACTCSWrEn = dmiWrEn & (dmiReq.addr === DMI_ABSTRACTCS)

  when(~dmactive){
    ABSTRACTCSReg := ABSTRACTCSReset
  }.otherwise {

    when (ABSTRACTCSWrEn){
      // TODO -- check to make sure it is legal to write this register now.
      when (ABSTRACTCSWrData.cmderr === 0.U) {
        ABSTRACTCSReg.cmderr := 0.U
      }
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

  // TODO: ABSTRACTCSReg.busy
  // TODO: ABSTRACTCSReg.cmderr

  //---- COMMAND

  val COMMANDReset = (new COMMANDFields()).fromBits(0.U)
  val COMMANDReg = RegInit(COMMANDReset)

  val COMMANDWrData = (new COMMANDFields()).fromBits(dmiReq.data)
  val COMMANDWrEn    = dmiWrEn & (dmiReq.addr === DMI_COMMAND)

  val COMMANDRdData = COMMANDReg

  when (~dmactive) {
    COMMANDReg := COMMANDReset
  }.otherwise {
    when (COMMANDWrEn) {
      //TODO : Check that it is legal to write this register right now.
      COMMANDReg := COMMANDWrData
    }
  }

  // --- Abstract Data

  val abstractDataWidth     = 32
  val abstractDataAddrWidth = log2Up(cfg.nAbstractDataWords)

  // These are byte addressible, s.t. the Processor can use
  // byte-addressible instructions to store to them.
  val abstractDataMem       = Reg(init = Vec.fill(cfg.nAbstractDataWords*4){0.asUInt(8.W)})

  val dmiAbstractDataIdx        = Wire(UInt(abstractDataAddrWidth.W))
  val dmiAbstractDataIdxValid   = Wire(Bool())
  val dmiAbstractDataRdData      = Wire (UInt(32.W))
  val dmiAbstractDataWrData      = Wire(UInt(32.W))

  val dmiAbstractDataOffset = log2Up(abstractDataWidth/8)

  // --- Program Buffer

  val PROGBUFCSRdData = (new PROGBUFCSFields).fromBits(0.U)
  PROGBUFCSRdData.progsize := cfg.nProgramBufferWords.U

  val programBufferDataWidth  = 32
  val programBufferAddrWidth = log2Up(cfg.nProgramBufferWords)

  val programBufferMem    = RegInit(Vec.fill(cfg.nProgramBufferWords*4){0.asUInt(8.W)})

  val dmiProgramBufferIdx   = Wire(UInt(programBufferAddrWidth.W))
  val dmiProgramBufferIdxValid   = Wire(Bool())
  val dmiProgramBufferRdData = Wire (UInt(32.W))
  val dmiProgramBufferWrData = Wire(UInt(32.W))
  val dmiProgramBufferWrEnFinal   = Wire(Bool())
  val dmiProgramBufferRdEnFinal   = Wire(Bool())

  val dmiProgramBufferOffset = log2Up(programBufferDataWidth/8)

  //--------------------------------------------------------------
  // DMI Registers 
  //--------------------------------------------------------------

  // --- Debug Bus Accesses

  val s_DMI_READY :: s_DMI_RESP :: Nil = Enum(Bits(), 2)
  val dmiStateReg = Reg(init = s_DMI_READY)

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
  dmiAbstractDataIdxValid := (dmiReq.addr >= DMI_DATA0) && (dmiReq.addr <= (DMI_DATA0 + cfg.nAbstractDataWords.U))

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
  dmiProgramBufferIdxValid := (dmiReq.addr >= DMI_PROGBUF0) && dmiReq.addr <= (DMI_PROGBUF0 + cfg.nProgramBufferWords.U)

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

  // -----------------------------------------
  // DMI Access Read Mux
  val dmiRdData = Wire(UInt(DMIConsts.dmiDataSize.W))

  when     (dmiReq.addr === DMI_DMCONTROL)  {dmiRdData := DMCONTROLRdData.asUInt()}
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

  val dmiResult  = Wire(new DMIResp())

  dmiResult.resp := dmi_RESP_SUCCESS
  dmiResult.data := dmiRdData

  // -----------------------------------------
  // DMI Access State Machine Decode (Combo)

  val dmiRespReg = Reg(new DMIResp())

  io.dmi.req.ready := (dmiStateReg === s_DMI_READY) ||
  (dmiStateReg === s_DMI_RESP && io.dmi.resp.fire())

  io.dmi.resp.valid := (dmiStateReg === s_DMI_RESP)
  io.dmi.resp.bits  := dmiRespReg

  dmiWrEn := (dmiReq.op === dmi_OP_WRITE) && io.dmi.req.fire()

  // -----------------------------------------
  // DMI Access State Machine Update (Seq)

  when (dmiStateReg === s_DMI_READY){
    when (io.dmi.req.fire()){
      dmiStateReg := s_DMI_RESP
      dmiRespReg := dmiResult
    }
  } .elsewhen (dmiStateReg === s_DMI_RESP){
    when (io.dmi.req.fire()){
      dmiStateReg := s_DMI_RESP
      dmiRespReg := dmiResult
    }.elsewhen (io.dmi.resp.fire()){
      dmiStateReg := s_DMI_READY
    }
  }

  //--------------------------------------------------------------
  // Debug ROM
  //--------------------------------------------------------------

  // See the debug directory for contents and scripts to generate this.

  val debugRomContents : Array[Byte] = Array(
    0x6f, 0x00, 0xc0, 0x00, 0x6f, 0x00, 0x80, 0x02, 0x6f, 0x00, 0x80, 0x03,
    0x0f, 0x00, 0xf0, 0x0f, 0x73, 0x10, 0x24, 0x7b, 0x73, 0x24, 0x40, 0xf1,
    0x23, 0x20, 0x80, 0x10, 0x03, 0x04, 0x04, 0x40, 0x63, 0x1a, 0x80, 0x00,
    0x73, 0x24, 0x40, 0xf1, 0x6f, 0xf0, 0x5f, 0xff, 0x23, 0x26, 0x00, 0x10,
    0x73, 0x00, 0x10, 0x00, 0x73, 0x24, 0x20, 0x7b, 0x23, 0x22, 0x00, 0x10,
    0x67, 0x00, 0x00, 0x90, 0x23, 0x24, 0x00, 0x10, 0x73, 0x00, 0x20, 0x7b
  ).map(_.toByte)

  val romRegFields  =  debugRomContents.map( x => RegField.r(8, (x.toInt & 0xFF).U))

  //--------------------------------------------------------------
  // "Variable" ROM
  //--------------------------------------------------------------
  // f09ff06f                j       808 <resume>
  // 00c0006f                j       910 <abstract>
  // 0f80006f                j       a00 <prog_buffer>

  val whereToReg = Reg(UInt(32.W))
  //TODO: whereToReg logic.

  val goBits = RegInit(Vec(cfg.nComponents, false.B))

  //TODO: goBits logic.

  // TODO: abstractGeneratedInstrucitonLogic.
  val abstractGeneratedInstruction = Reg(UInt(32.W))
  val ebreakInstruction = 0x00100073.U

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
    GO          -> goBits.map(x => RegField.r(8, x)),   
    WHERETO     -> Seq(RegField.r(32, whereToReg)),
    ABSTRACT    -> Seq(RegField.r(32, abstractGeneratedInstruction), RegField.r(32, ebreakInstruction))
  )

  //--------------------------------------------------------------
  // State Machine
  //--------------------------------------------------------------



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
