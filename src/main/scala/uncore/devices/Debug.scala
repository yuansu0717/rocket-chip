// See LICENSE.SiFive for license details.

package uncore.devices

import Chisel._
import junctions._
import util._
import regmapper._
import tile.XLen
import uncore.tilelink2._
import config._
import diplomacy._


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

  def dmi_haltStatusAddr   = 0x40
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
  def ENTRY        = 0x800
  //def EXCEPTION    = 0x808
  def RESUME       = 0x804

  def WHERETO      = 0x300
  def ABSTRACT     = 0x304
  def PROGBUF      = 0x340

  // This shows up in HartInfo
  def DATA         = 0x380

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
  *  nAbstractDataWords: Number of 32-bit words for Abstract Commands
  *  nProgamBufferWords: Number of 32-bit words for Program Buffer
  *  hasBusMaster: Whethr or not a bus master should be included
  *    The size of the accesses supported by the Bus Master. 
  *  nSerialPorts : Number of serial ports to instantiate
  *  supportQuickAccess : Whether or not to support the quick access command.
  *  supportHartArray : Whether or not to implement the hart array register.
  **/


case class DebugModuleConfig (
  nDMIAddrSize  : Int,
  nProgramBufferWords: Int,
  nAbstractDataWords : Int,
  //TODO: Use diplomacy to decide if you want this.
  hasBusMaster : Boolean,
  hasAccess128 : Boolean,
  hasAccess64  : Boolean,
  hasAccess32  : Boolean,
  hasAccess16  : Boolean,
  hasAccess8   : Boolean,
  nSerialPorts : Int,
  supportQuickAccess : Boolean,
  supportHartArray   : Boolean
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

  private val maxComponents = 1024
  
  require ((nAbstractDataWords  > 0)  && (nAbstractDataWords  <= 16))
  require ((nProgramBufferWords >= 0) && (nProgramBufferWords <= 16))

  if (supportQuickAccess) {
    // TODO: Check that quick access requirements are met.
  }

}

class DefaultDebugModuleConfig (val xlen:Int /*TODO , val configStringAddr: Int*/)
    extends DebugModuleConfig(
      nDMIAddrSize = 7,
      //TODO use more words to support arbitrary sequences.
      nProgramBufferWords =  15,
      // TODO use less for small XLEN?
      nAbstractDataWords  =  4,
      hasBusMaster = false,
      hasAccess128 = false, 
      hasAccess64 = false, 
      hasAccess32 = false, 
      hasAccess16 = false, 
      hasAccess8 = false, 
      nSerialPorts = 0,
      supportQuickAccess = false,
      supportHartArray = false
        // TODO configStringAddr = configStringAddr
        // TODO: accept a mapping function from HARTID -> HARTSEL
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

/* structure for synchronizing between the "Outer" and "Inner"
 */

class DebugInternalBundle ()(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val resumereq = Bool()
  val hartsel = UInt(10.W)
}

//*****************************************
// Debug ROM
//
// *****************************************

class TLDebugModuleROM()(implicit p: Parameters) extends TLROM(
  base = DsbRegAddrs.ROMBASE, // This is required for correct functionality. It's not a parameter.
  size = 0x800,
  contentsDelayed = DebugRomContents(),
  executable = true,
  beatBytes = p(XLen)/8,
  name = "debug_rom",
  devcompat = Seq("sifive,debug-013"))

// *****************************************
// Debug Module 
// 
// *****************************************

class DebugCtrlBundle (nComponents: Int)(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val debugUnavail    = Vec(nComponents, Bool()).asInput
  val ndreset         = Bool(OUTPUT)
  val debugActive     = Bool(OUTPUT)
}


/** Parameterized version of the Debug Module defined in the
  *  RISC-V Debug Specification 
  *  
  *  DebugModule is a slave to two masters:
  *    The Debug Bus (DMI) -- implemented as a generic Decoupled IO with request
  *                           and response channels. Converted to TL within 'Outer',
  *                           then synchronized within the top level and passed to 'Inner'.
  *  
  *    The System Bus -- implemented as a  RegisterRouter
  *  
  *  DebugModule is responsible for control registers and RAM. ROM is in a seperate module.
  *      to support debug interactions, as well as driving interrupts
  *      to a configurable number of components in the system.
  *      It must also maintain a state machine to track the state of harts
  *      being debugged.
  *      It is also responsible for some reset lines.
  */

// Local reg mapper function : Notify when written, but give the value as well.  
object WNotify {
  def apply(n: Int, value: UInt, set: Bool) : RegField = {
    RegField(n, value, RegWriteFn((valid, data) => {
      set := valid
      when(valid) {value := data}
      Bool(true)
    }))
  }
}

// Local reg mapper function : Notify when accessed either as read or write.
object RWNotify {
    def apply (n: Int, rVal: UInt, wVal: UInt, rNotify: Bool, wNotify: Bool) : RegField = {
      RegField(n,
        RegReadFn ((ready)       => {rNotify := ready ; (Bool(true), rVal)}),
        RegWriteFn((valid, data) => {
          wNotify := valid
          when (valid) {wVal := data}
          Bool(true)
        }
        ))
    }
}



class TLDebugModuleOuter(device: Device)(implicit p: Parameters) extends LazyModule {

  // For Shorter Register Names
  import DMI_RegAddrs._

  val intnode = IntNexusNode(
    numSourcePorts = 1 to 1024,
    numSinkPorts   = 0 to 0,
    sourceFn       = { _ => IntSourcePortParameters(Seq(IntSourceParameters(1, Seq(Resource(device, "int"))))) },
    sinkFn         = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) }
  )

  val dmiNode = TLRegisterNode (
    address = Seq(AddressSet(DMI_DMCONTROL << 2, 0x3)),
    device = device,
    deviceKey = "reg",
    beatBytes = 4,
    executable = false
  )

  lazy val module = new LazyModuleImp(this) {

    val nComponents = intnode.bundleOut.size

    val io = new Bundle {
      val ctrl = (new DebugCtrlBundle(nComponents))
      val tlIn = dmiNode.bundleIn
      val debugInterrupts = intnode.bundleOut
      val innerCtrl = new DecoupledIO(new DebugInternalBundle())
    }

    //----DMCONTROL (The whole point of 'Outer' is to maintain this register on TCK domain, so that it
    //               can be written even if 'Inner' is not being clocked or reset. This allows halting
    //               harts while the rest of the system is in reset. It doesn't really allow any other
    //               register accesses, which will keep returning 'busy' to the JTAG interface.

    val DMCONTROLReset = Wire(init = (new DMCONTROLFields().fromBits(0.U)))
    DMCONTROLReset.authenticated := true.B // Not implemented
    DMCONTROLReset.version       := 1.U
    val DMCONTROLNxt = Wire(init = new DMCONTROLFields().fromBits(0.U))

    val DMCONTROLReg = Wire(init = new DMCONTROLFields().fromBits(AsyncResetReg(updateData = DMCONTROLNxt.asUInt,
      resetData = BigInt(0) | BigInt(1 << 8) /*authenticated*/ | BigInt(1 << 0), /*version*/ // TODO automate DMCONTROLReset
      enable = true.B,
      name = "DMCONTROL"
    )))

    // The below doesn't work in the 'when' unless this intermediate assignment is done.
    val unavailVec = Wire(Vec(nComponents, Bool()))
    unavailVec := io.ctrl.debugUnavail

    val DMCONTROLRdData = Wire(init = DMCONTROLReg)
    when (DMCONTROLReg.hartsel >= nComponents.U) {
      DMCONTROLRdData.hartstatus := DebugModuleHartStatus.NonExistent.id.U
    //}.elsewhen (haltedBitRegs(DMCONTROLReg.hartsel)) { TODO: will not be an issue when this is moved to DMSTATUS.
    // DMCONTROLRdData.hartstatus := DebugModuleHartStatus.Halted.id.U
    }.elsewhen(unavailVec(DMCONTROLReg.hartsel)) {
      DMCONTROLRdData.hartstatus := DebugModuleHartStatus.Unavailable.id.U
    } .otherwise {
      DMCONTROLRdData.hartstatus := DebugModuleHartStatus.Running.id.U
    }

    val DMCONTROLWrDataVal = Wire(init = 0.U(32.W))
    val DMCONTROLWrData = (new DMCONTROLFields()).fromBits(DMCONTROLWrDataVal)
    val DMCONTROLWrEn   = Wire(init = false.B)
    val DMCONTROLRdEn   = Wire(init = false.B)

    val dmactive = DMCONTROLReg.dmactive

    DMCONTROLNxt := DMCONTROLReg

    when (~dmactive) {
      DMCONTROLNxt := DMCONTROLReset
    } .otherwise {
      when (DMCONTROLWrEn) {
        DMCONTROLNxt.reset     := DMCONTROLWrData.reset
        DMCONTROLNxt.haltreq   := DMCONTROLWrData.haltreq
        DMCONTROLNxt.resumereq := DMCONTROLWrData.resumereq
        DMCONTROLNxt.hartsel   := DMCONTROLWrData.hartsel
      }
    }

    // Put this last to override its own effects.
    when (DMCONTROLWrEn) {
      DMCONTROLNxt.dmactive := DMCONTROLWrData.dmactive
    }

    dmiNode.regmap(
      (DMI_DMCONTROL   << 2) -> Seq(RWNotify(32, DMCONTROLRdData.asUInt(), DMCONTROLWrDataVal, DMCONTROLRdEn, DMCONTROLWrEn))
    )

    val debugIntNxt = Wire(init = Vec.fill(nComponents){false.B})
    val debugIntRegs = Wire(init = Vec(AsyncResetReg(updateData = debugIntNxt.asUInt,
      resetData = 0,
      enable = true.B,
      name = "debugInterrupts").toBools))

    debugIntNxt := debugIntRegs

    //--------------------------------------------------------------
    // Interrupt Registers
    //--------------------------------------------------------------

    for (component <- 0 until nComponents) {
      io.debugInterrupts(component)(0) := debugIntRegs(component)
    }

    // Halt request registers are written by write to DMCONTROL.haltreq
    // and cleared by writes to DMCONTROL.resumereq.

    for (component <- 0 until nComponents) {
      when (~dmactive) {
        debugIntNxt(component) := false.B
      }. otherwise {
        when (DMCONTROLWrEn) {
          when (DMCONTROLWrData.hartsel === component.U) {
            debugIntNxt(component) := (debugIntRegs(component) | DMCONTROLWrData.haltreq) &
            ~(DMCONTROLWrData.resumereq)
          }
        }
      }
    }

    io.innerCtrl.valid := DMCONTROLWrEn
    io.innerCtrl.bits.hartsel   := DMCONTROLWrData.hartsel
    io.innerCtrl.bits.resumereq := DMCONTROLWrData.resumereq

    io.ctrl.ndreset := DMCONTROLReg.reset
    io.ctrl.debugActive := DMCONTROLReg.dmactive

  }
}

// Wrapper around TL Debug Module Inner and an Async DMI Sink interface.
// Handles the synchronization of dmactive, which is used as a synchronous reset
// inside the Inner block.
class TLDebugModuleInnerAsync(device: Device, getNComponents: () => Int)(implicit p: Parameters) extends LazyModule{

  val dmInner = LazyModule(new TLDebugModuleInner(device, getNComponents)(p))
  val dmiNode = TLAsyncInputNode()
  val tlNode = TLInputNode()

  dmInner.dmiNode := TLAsyncCrossingSink(depth=1)(dmiNode)
  dmInner.tlNode  := tlNode 

  lazy val module = new LazyModuleImp(this) {

    val io = new Bundle {
      val tl_in = tlNode.bundleIn
      val dmi_in = dmiNode.bundleIn
      val dmactive = Bool(INPUT)
      val innerCtrl = new AsyncBundle(1, new DebugInternalBundle()).flip
    }

    dmInner.module.io.innerCtrl := FromAsyncBundle(io.innerCtrl)
    dmInner.module.io.dmactive := ~ResetCatchAndSync(clock, ~io.dmactive)

  }
}


class TLDebugModuleOuterAsync(device: Device)(implicit p: Parameters) extends LazyModule {

  val dmi2tl = LazyModule(new DMIToTL())
  val dmiXbar = LazyModule (new TLXbar())

  val dmOuter = LazyModule( new TLDebugModuleOuter(device))
  val intnode = IntOutputNode()

  val dmiInnerNode = TLAsyncOutputNode()

  intnode := dmOuter.intnode

  dmiXbar.node := dmi2tl.node
  dmOuter.dmiNode := dmiXbar.node
  dmiInnerNode := TLAsyncCrossingSource()(dmiXbar.node)
  
  lazy val module = new LazyModuleImp(this) {

    val nComponents = intnode.bundleOut.size

    val io = new Bundle {
      val dmi   = new DMIIO()(p).flip()
      val dmiInner = dmiInnerNode.bundleOut
      val ctrl = new DebugCtrlBundle(nComponents)
      val debugInterrupts = intnode.bundleOut
      val innerCtrl = new AsyncBundle(1,new DebugInternalBundle())
    }

    dmi2tl.module.io.dmi <> io.dmi

    io.ctrl <> dmOuter.module.io.ctrl
    io.innerCtrl := ToAsyncBundle(dmOuter.module.io.innerCtrl)
  }
}

class TLDebugModuleInner(device: Device, getNComponents: () => Int)(implicit p: Parameters) extends LazyModule
{

  val dmiNode = TLRegisterNode(
    address = AddressSet.misaligned(0, DMI_RegAddrs.DMI_DMCONTROL << 2) ++
              AddressSet.misaligned((DMI_RegAddrs.DMI_DMCONTROL + 1) << 2, (0x200 - ((DMI_RegAddrs.DMI_DMCONTROL + 1) << 2))),
    device = device,
    deviceKey = "reg",
    beatBytes = 4,
    executable = false
  )

  val tlNode = TLRegisterNode(
    address=Seq(AddressSet(0, 0x7FF)), // This is required for correct functionality, it's not configurable.
    device=device,
    deviceKey="reg",
    beatBytes=p(XLen)/8,
    executable=true
  )

  lazy val module = new LazyModuleImp(this){

    val cfg = p(DMKey)

    val nComponents = getNComponents()

    val io = new Bundle {
      val hart_in = tlNode.bundleIn
      val dmi_in = dmiNode.bundleIn
      val dmactive = Bool(INPUT)
      val innerCtrl = (new DecoupledIO(new DebugInternalBundle())).flip
    }

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

    require (cfg.nSerialPorts == 0)
    require (cfg.hasBusMaster == false)
    require (cfg.supportQuickAccess == false)
    require (cfg.supportHartArray == false)

    //--------------------------------------------------------------
    // Register & Wire Declarations
    //--------------------------------------------------------------

    val haltedBitRegs  = Reg(init=Vec.fill(nComponents){false.B})

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
    // Registers coming from 'CONTROL' in Outer
    //--------------------------------------------------------------

    val selectedHartReg = Reg(init = 0.U(10.W))

    when (io.innerCtrl.fire()){
      selectedHartReg := io.innerCtrl.bits.hartsel
    }

    io.innerCtrl.ready := true.B

    //--------------------------------------------------------------
    // DMI Registers
    //--------------------------------------------------------------

    //----HARTINFO

    val HARTINFORdData = Wire (init = (new HARTINFOFields()).fromBits(0.U))
    HARTINFORdData.dataaccess  := true.B
    HARTINFORdData.datasize    := cfg.nAbstractDataWords.U
    HARTINFORdData.dataaddr    := DsbRegAddrs.DATA.U

    //----HALTSUM (and halted registers)
    val numHaltedStatus = ((nComponents - 1) / 32) + 1
    val haltedStatus   = Wire(Vec(numHaltedStatus, Bits(width = 32)))

    for (ii <- 0 until numHaltedStatus) {
      haltedStatus(ii) := Cat(haltedBitRegs.slice(ii * 32, (ii + 1) * 32).reverse)
    }

    val haltedSummary = Cat(haltedStatus.map(_.orR).reverse)
    val HALTSUMRdData = (new HALTSUMFields()).fromBits(haltedSummary)

    //----ABSTRACTCS

    val ABSTRACTCSReset = Wire(init = (new ABSTRACTCSFields()).fromBits(0.U))
    ABSTRACTCSReset.datacount := cfg.nAbstractDataWords.U

    val ABSTRACTCSReg       = RegInit(ABSTRACTCSReset)
    val ABSTRACTCSWrDataVal = Wire(init = 0.U(32.W))
    val ABSTRACTCSWrData    = (new ABSTRACTCSFields()).fromBits(ABSTRACTCSWrDataVal)
    val ABSTRACTCSRdData    = Wire(init = ABSTRACTCSReg)

    val ABSTRACTCSRdEn = Wire(init = false.B)
    val ABSTRACTCSWrEnMaybe = Wire(init = false.B)

    val ABSTRACTCSWrEnLegal = Wire(init = false.B)
    val ABSTRACTCSWrEn      = ABSTRACTCSWrEnMaybe && ABSTRACTCSWrEnLegal


    val errorBusy        = Wire(init = false.B)
    val errorException   = Wire(init = false.B)
    val errorUnsupported = Wire(init = false.B)
    val errorHaltResume  = Wire(init = false.B)

    val abstractCSClearError = ABSTRACTCSWrEn && (ABSTRACTCSWrData.cmderr === 0.U)

    when(~io.dmactive){
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

    val COMMANDWrDataVal    = Wire(init = 0.U(32.W))
    val COMMANDWrData       = (new COMMANDFields()).fromBits(COMMANDWrDataVal)
    val COMMANDWrEnMaybe    = Wire(init = false.B)
    val COMMANDWrEnLegal    = Wire(init = false.B)
    val COMMANDRdEn  = Wire(init = false.B)

    val COMMANDWrEn = COMMANDWrEnMaybe && COMMANDWrEnLegal
    val COMMANDRdData = COMMANDReg

    when (~io.dmactive) {
      COMMANDReg := COMMANDReset
    }.otherwise {
      when (COMMANDWrEn) {
        COMMANDReg := COMMANDWrData
      }
    }


    // --- Abstract Data

    // These are byte addressible, s.t. the Processor can use
    // byte-addressible instructions to store to them.
    val abstractDataMem       = RegInit(Vec.fill(cfg.nAbstractDataWords*4){0.U(8.W)})
    val abstractDataWords     = List.tabulate(cfg.nAbstractDataWords) { ii =>
      val slice = abstractDataMem.slice(ii * 4, (ii+1)*4)
      slice.reduce[UInt]{ case (x: UInt, y: UInt) => Cat(y, x)
      }
    }

    // --- Program Buffer

    val PROGBUFCSRdData = Wire(init = (new PROGBUFCSFields).fromBits(0.U))
    PROGBUFCSRdData.progsize := cfg.nProgramBufferWords.U

    val programBufferMem    = RegInit(Vec.fill(cfg.nProgramBufferWords*4){0.U(8.W)})

    //--------------------------------------------------------------
    // These bits are implementation-specific bits set
    // by harts executing code.
    //--------------------------------------------------------------

    for (component <- 0 until nComponents) {
      when (~io.dmactive) {
        haltedBitRegs(component) := false.B}
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

    //--------------------------------------------------------------
    // Abstract Data Access (DMI ... System Bus can override)
    //--------------------------------------------------------------

    val dmiAbstractDataRdEn = Wire(init = Vec.fill(cfg.nAbstractDataWords * 4){false.B})
    val dmiAbstractDataWrEn = Wire(init = Vec.fill(cfg.nAbstractDataWords * 4){false.B})

    //--------------------------------------------------------------
    // Program Buffer Access (DMI ... System Bus can override)
    //--------------------------------------------------------------
    val dmiProgramBufferRdEn = Wire(init = Vec.fill(cfg.nProgramBufferWords * 4){false.B})
    val dmiProgramBufferWrEn = Wire(init = Vec.fill(cfg.nProgramBufferWords * 4){false.B})


    dmiNode.regmap(
      (DMI_HARTINFO    << 2) -> Seq(RegField.r(32, HARTINFORdData.asUInt())),
      (DMI_HALTSUM     << 2) -> Seq(RegField.r(32, HALTSUMRdData.asUInt())),
      (DMI_ABSTRACTCS  << 2) -> Seq(RWNotify(32, ABSTRACTCSRdData.asUInt(), ABSTRACTCSWrDataVal, ABSTRACTCSRdEn, ABSTRACTCSWrEnMaybe)),
      (DMI_PROGBUFCS   << 2) -> Seq(RegField.r(32, PROGBUFCSRdData.asUInt())),
      (DMI_COMMAND     << 2) -> Seq(RWNotify(32, COMMANDRdData.asUInt(), COMMANDWrDataVal, COMMANDRdEn, COMMANDWrEnMaybe)),
      (DMI_DATA0       << 2) -> abstractDataMem.zipWithIndex.map{case (x, i) => RWNotify(8, x, x,
        dmiAbstractDataRdEn(i),
        dmiAbstractDataWrEn(i))},
      (DMI_PROGBUF0    << 2) -> programBufferMem.zipWithIndex.map{case (x, i) => RWNotify(8, x, x,
        dmiProgramBufferRdEn(i),
        dmiProgramBufferWrEn(i))},
      (DMIConsts.dmi_haltStatusAddr << 2) -> haltedStatus.map(x => RegField.r(32, x))
    )

    //--------------------------------------------------------------
    // "Variable" ROM Generation
    //--------------------------------------------------------------

    val goProgramBuffer = Wire(init = false.B)
    val goResume        = Wire(init = false.B)
    val goAbstract      = Wire(init = false.B)

    val whereToReg = RegInit(0.U(32.W))

    val jalProgBuf  = Wire(init = (new GeneratedUJ()).fromBits(rocket.Instructions.JAL.value.U))
    jalProgBuf.setImm(PROGBUF - WHERETO)
    jalProgBuf.rd := 0.U

    val jalAbstract  = Wire(init = (new GeneratedUJ()).fromBits(rocket.Instructions.JAL.value.U))
    jalAbstract.setImm(ABSTRACT - WHERETO)
    jalProgBuf.rd := 0.U

    val jalResume  = Wire(init = (new GeneratedUJ()).fromBits(rocket.Instructions.JAL.value.U))
    jalResume.setImm(RESUME - WHERETO)
    jalResume.rd := 0.U

    when (goProgramBuffer) {
      whereToReg := jalProgBuf.asUInt()
    }.elsewhen (goResume) {
      whereToReg := jalResume.asUInt()
    }.elsewhen (goAbstract) {
      whereToReg := jalAbstract.asUInt()
    }

    val goReg            = Reg (init = false.B)
    when (goProgramBuffer | goResume | goAbstract) {
      goReg := true.B
    }.elsewhen (hartGoingWrEn){
      assert(hartGoingId === 0.U, "Unexpected 'GOING' hart: %x, expected %x", hartGoingId, selectedHartReg)
      goReg := false.B
    }

    val goBytes = Wire(init = Vec.fill(nComponents){0.U(8.W)})
    goBytes(selectedHartReg) := Cat(0.U(7.W), goReg)

    //----------------------------
    // Abstract Command Decoding & Generation
    //----------------------------

    val accessRegisterCommandWr  = Wire(init = (new ACCESS_REGISTERFields()).fromBits(COMMANDWrData.asUInt()))
    val accessRegisterCommandReg = Wire(init = (new ACCESS_REGISTERFields()).fromBits(COMMANDReg.asUInt()))

    // TODO: Quick Access

    class GeneratedI extends Bundle {
      val imm    = UInt(12.W)
      val rs1    = UInt(5.W)
      val funct3 = UInt(3.W)
      val rd     = UInt(5.W)
      val opcode = UInt(7.W)
    }

    class GeneratedS extends Bundle {
      val immhi  = UInt(7.W)
      val rs2    = UInt(5.W)
      val rs1    = UInt(5.W)
      val funct3 = UInt(3.W)
      val immlo  = UInt(5.W)
      val opcode = UInt(7.W)
    }

    class GeneratedUJ extends Bundle {
      val imm3    = UInt(1.W)
      val imm0    = UInt(10.W)
      val imm1    = UInt(1.W)
      val imm2    = UInt(8.W)
      val rd      = UInt(5.W)
      val opcode  = UInt(7.W)

      def setImm(imm: Int) : Unit = {
        // TODO: Check bounds of imm.

        require(imm % 2 == 0, "Immediate must be even for UJ encoding.")
        val immWire = Wire(init = imm.S(21.W))
        val immBits = Wire(init = Vec(immWire.toBools))

        imm0 := immBits.slice(1,  1  + 10).asUInt()
        imm1 := immBits.slice(11, 11 + 11).asUInt()
        imm2 := immBits.slice(12, 12 + 8).asUInt()
        imm3 := immBits.slice(20, 20 + 1).asUInt()
      }
    }

    val abstractGeneratedReg = Reg(UInt(32.W))
    val abstractGeneratedI = Wire(new GeneratedI())
    val abstractGeneratedS = Wire(new GeneratedS())

    abstractGeneratedI.opcode := ((new GeneratedI()).fromBits(rocket.Instructions.LW.value.U)).opcode
    abstractGeneratedI.rd     := (accessRegisterCommandReg.regno & 0x1F.U)
    abstractGeneratedI.funct3 := accessRegisterCommandReg.size
    abstractGeneratedI.rs1    := 0.U
    abstractGeneratedI.imm    := DATA.U

    abstractGeneratedS.opcode := ((new GeneratedI()).fromBits(rocket.Instructions.SW.value.U)).opcode
    abstractGeneratedS.immlo  := (DATA & 0x1F).U
    abstractGeneratedS.funct3 := accessRegisterCommandReg.size
    abstractGeneratedS.rs1    := 0.U
    abstractGeneratedS.rs2    := (accessRegisterCommandReg.regno & 0x1F.U)
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

    val ebreakInstruction = Wire(init = rocket.Instructions.EBREAK.value.U)

    //--------------------------------------------------------------
    // System Bus Access
    //--------------------------------------------------------------

    tlNode.regmap(
      // This memory is writable.
      HALTED      -> Seq(WNotify(sbIdWidth, hartHaltedId, hartHaltedWrEn)),
      GOING       -> Seq(WNotify(sbIdWidth, hartGoingId,  hartGoingWrEn)),
      RESUMING    -> Seq(WNotify(sbIdWidth, hartResumingId,  hartResumingWrEn)),
      EXCEPTION   -> Seq(WNotify(sbIdWidth, hartExceptionId,  hartExceptionWrEn)),
      DATA        -> abstractDataMem.map(x => RegField(8, x)),
      PROGBUF     -> programBufferMem.map(x => RegField(8, x)),

      // These sections are read-only.
      //    ROMBASE     -> romRegFields,
      GO          -> goBytes.map(x => RegField.r(8, x)),
      WHERETO     -> Seq(RegField.r(32, whereToReg)),
      ABSTRACT    -> Seq(RegField.r(32, abstractGeneratedReg), RegField.r(32, ebreakInstruction))
     )

    //--------------------------------------------------------------
    // Abstract Command State Machine
    //--------------------------------------------------------------

    object CtrlState extends scala.Enumeration {
      type CtrlState = Value
      val Waiting, CheckGenerate, PreExec, Abstract, PostExec = Value

      def apply( t : Value) : UInt = {
        t.id.U(log2Up(values.size).W)
      }
    }
    import CtrlState._

    val ctrlStateReg = RegInit(CtrlState(Waiting))
    
    // Combo
    val hartHalted   = haltedBitRegs(selectedHartReg)
    val ctrlStateNxt = Wire(init = ctrlStateReg)
    val autoexecVec  = Wire(init = Vec.fill(8){false.B})

    val dmiAbstractDataAccessVec  = Wire(init = Vec.fill(cfg.nAbstractDataWords * 4){false.B})
    dmiAbstractDataAccessVec := (dmiAbstractDataWrEn zip dmiAbstractDataRdEn).map{ case (r,w) => r | w}
    val dmiAbstractDataAccess  = dmiAbstractDataAccessVec.reduce(_ || _ )
    val dmiProgramBufferAccess = dmiProgramBufferWrEn.reduce(_ || _) | dmiProgramBufferRdEn.reduce(_ || _)

    if (cfg.nAbstractDataWords > 0) autoexecVec(0) := (dmiAbstractDataAccessVec(0 * 4)) && ABSTRACTCSReg.autoexec0
    if (cfg.nAbstractDataWords > 1) autoexecVec(1) := (dmiAbstractDataAccessVec(1 * 4)) && ABSTRACTCSReg.autoexec1
    if (cfg.nAbstractDataWords > 2) autoexecVec(2) := (dmiAbstractDataAccessVec(2 * 4)) && ABSTRACTCSReg.autoexec2
    if (cfg.nAbstractDataWords > 3) autoexecVec(3) := (dmiAbstractDataAccessVec(3 * 4)) && ABSTRACTCSReg.autoexec3
    if (cfg.nAbstractDataWords > 4) autoexecVec(4) := (dmiAbstractDataAccessVec(4 * 4)) && ABSTRACTCSReg.autoexec4
    if (cfg.nAbstractDataWords > 5) autoexecVec(5) := (dmiAbstractDataAccessVec(5 * 4)) && ABSTRACTCSReg.autoexec5
    if (cfg.nAbstractDataWords > 6) autoexecVec(6) := (dmiAbstractDataAccessVec(6 * 4)) && ABSTRACTCSReg.autoexec6
    if (cfg.nAbstractDataWords > 7) autoexecVec(7) := (dmiAbstractDataAccessVec(7 * 4)) && ABSTRACTCSReg.autoexec7

    val autoexec = autoexecVec.reduce(_ || _)

    //------------------------

    // DMI Register Control and Status
    abstractCommandBusy := (ctrlStateReg != CtrlState(Waiting))

    ABSTRACTCSWrEnLegal := (ctrlStateReg === CtrlState(Waiting))
    COMMANDWrEnLegal    := (ctrlStateReg === CtrlState(Waiting))

    errorBusy := (ABSTRACTCSWrEnMaybe    && ~ABSTRACTCSWrEnLegal)   ||
                 (COMMANDWrEnMaybe       && ~COMMANDWrEnLegal)      ||
                 (dmiAbstractDataAccess  && abstractCommandBusy)    ||
                 (dmiProgramBufferAccess && abstractCommandBusy)

    // TODO: Maybe Quick Access
    val commandWrIsAccessRegister = (COMMANDWrData.cmdtype === DebugAbstractCommandType.AccessRegister.id.U)
    val commandRegIsAccessRegister = (COMMANDReg.cmdtype === DebugAbstractCommandType.AccessRegister.id.U)

    val commandRegIsUnsupported = Wire(init = true.B)
    val commandRegBadHaltResume = Wire(init = false.B)
    when (commandRegIsAccessRegister) {
      when ((accessRegisterCommandReg.regno >= 0x1000.U && accessRegisterCommandReg.regno <= 0x101F.U)){
        commandRegIsUnsupported := false.B
        commandRegBadHaltResume := ~hartHalted
      }
    }

    val wrAccessRegisterCommand  = COMMANDWrEn && commandWrIsAccessRegister  && (ABSTRACTCSReg.cmderr === 0.U)
    val regAccessRegisterCommand = autoexec    && commandRegIsAccessRegister && (ABSTRACTCSReg.cmderr === 0.U)
    //------------------------
    // Variable ROM STATE MACHINE
    // -----------------------
    

    when (ctrlStateReg === CtrlState(Waiting)){

      when (wrAccessRegisterCommand || regAccessRegisterCommand) {
        ctrlStateNxt := CtrlState(CheckGenerate)
      }.elsewhen(io.innerCtrl.fire() && io.innerCtrl.bits.resumereq) {
        goResume := true.B
      }

    }.elsewhen (ctrlStateReg === CtrlState(CheckGenerate)){

      // We use this state to ensure that the COMMAND has been
      // registered by the time that we need to use it, to avoid
      // generating it directly from the COMMANDWrData.

      when (commandRegIsUnsupported) {
        errorUnsupported := true.B
        ctrlStateNxt := CtrlState(Waiting)
      }.elsewhen (commandRegBadHaltResume){
        errorHaltResume := true.B
        ctrlStateNxt := CtrlState(Waiting)
      }.otherwise {
        when (accessRegisterCommandReg.preexec) {
          ctrlStateNxt    := CtrlState(PreExec)
          goProgramBuffer := true.B
        }.otherwise {
          ctrlStateNxt := CtrlState(Abstract)
          goAbstract := true.B
        }
      }
    }.elsewhen (ctrlStateReg === CtrlState(PreExec)) {

      // We can't just look at 'hartHalted' here, because
      // hartHaltedWrEn is overloaded to mean 'got an ebreak'
      // which may have happened when we were already halted.
      when(goReg === false.B && hartHaltedWrEn && (hartHaltedId === selectedHartReg)){
        ctrlStateNxt := CtrlState(Abstract)
        goAbstract := true.B
      }
      when(hartExceptionWrEn) {
        assert(hartExceptionId === 0.U,  "Unexpected 'EXCEPTION' hart, %x, expected %x", hartExceptionId, 0.U)
        ctrlStateNxt := CtrlState(Waiting)
        errorException := true.B
      }
    }.elsewhen (ctrlStateReg === CtrlState(Abstract)) {

      // We can't just look at 'hartHalted' here, because
      // hartHaltedWrEn is overloaded to mean 'got an ebreak'
      // which may have happened when we were already halted.
      when(goReg === false.B && hartHaltedWrEn && (hartHaltedId === selectedHartReg)){
        when (accessRegisterCommandReg.postexec) {
          ctrlStateNxt := CtrlState(PostExec)
          goProgramBuffer := true.B
        }.otherwise {
          ctrlStateNxt := CtrlState(Waiting)
        }
      }

      when(hartExceptionWrEn) {
        assert(hartExceptionId === 0.U, "Unexpected 'EXCEPTION' hart, %x, expected %x", hartExceptionId, selectedHartReg)
        ctrlStateNxt := CtrlState(Waiting)
        errorUnsupported := true.B
      }

    }.elsewhen (ctrlStateReg === CtrlState(PostExec)) {

      // We can't just look at 'hartHalted' here, because
      // hartHaltedWrEn is overloaded to mean 'got an ebreak'
      // which may have happened when we were already halted.
      when(goReg === false.B && hartHaltedWrEn && (hartHaltedId === selectedHartReg)){
        ctrlStateNxt := CtrlState(Waiting)
      }

      when(hartExceptionWrEn) {
        assert(hartExceptionId === 0.U, "Unexpected 'EXCEPTION' hart, %x, expected %x", hartExceptionId, 0.U)
        ctrlStateNxt := CtrlState(Waiting)
        errorException := true.B
      }
    }

    when (~io.dmactive) {
      ctrlStateReg := CtrlState(Waiting)
    }.otherwise {
      ctrlStateReg := ctrlStateNxt
    }
  }
}

/** Create a version of the TLDebugModule which includes a synchronization interface
  * internally for the DMI. This is no longer optional outside of this module
  *  because the Clock must run when tlClock isn't running or tlReset is asserted.
  */

class TLDebugModule(implicit p: Parameters) extends LazyModule {

  val device = new SimpleDevice("debug-controller", Seq("riscv,debug-013")){
    override val alwaysExtended = true
  }

  val node = TLInputNode()
  val intnode = IntOutputNode()

  val dmOuter = LazyModule(new TLDebugModuleOuterAsync(device)(p))
  val dmInner = LazyModule(new TLDebugModuleInnerAsync(device, () => {intnode.bundleOut.size})(p))

  dmInner.dmiNode := dmOuter.dmiInnerNode
  dmInner.tlNode := node
  intnode := dmOuter.intnode

  lazy val module = new LazyModuleImp(this) {
    val nComponents = intnode.bundleOut.size

    val io = new Bundle {
      val ctrl = new DebugCtrlBundle(nComponents)
      val dmi = new ClockedDMIIO().flip
      val in = node.bundleIn
      val debugInterrupts = intnode.bundleOut
    }

    dmOuter.module.io.dmi <> io.dmi
    dmOuter.module.reset := io.dmi.dmiReset
    dmOuter.module.clock := io.dmi.dmiClock

    dmInner.module.io.innerCtrl := dmOuter.module.io.innerCtrl

    io.ctrl <> dmOuter.module.io.ctrl

  }
}

/** This includes the clock and reset as these are passed through the
  *  hierarchy until the Debug Module is actually instantiated. 
  *  
  */

class ClockedDMIIO(implicit val p: Parameters) extends ParameterizedBundle()(p){
  val dmi      = new DMIIO()(p)
  val dmiClock = Clock(OUTPUT)
  val dmiReset = Bool(OUTPUT)
}

/** Convert DMI to TL. Avoids using special DMI synchronizers and register accesses
  *  
  */

class DMIToTL(implicit p: Parameters) extends LazyModule {

  val node = TLClientNode(TLClientParameters())

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val dmi = new DMIIO()(p).flip()
      val out = node.bundleOut
    }

    val tl = io.out(0)
    val edge = node.edgesOut(0)

    val src  = Wire(init = 0.U)
    val addr = Wire(init = (io.dmi.req.bits.addr << 2))
    val size = (log2Ceil(DMIConsts.dmiDataSize / 8)).U

    val (_,  gbits) = edge.Get(src, addr, size)
    val (_, pfbits) = edge.Put(src, addr, size, io.dmi.req.bits.data)

    tl.a.bits        := Mux((io.dmi.req.bits.op === DMIConsts.dmi_OP_WRITE),  pfbits ,  gbits)
    tl.a.valid       := io.dmi.req.valid
    io.dmi.req.ready := tl.a.ready

    io.dmi.resp.valid      := tl.d.valid
    tl.d.ready             := io.dmi.resp.ready
    io.dmi.resp.bits.resp  := Mux(tl.d.bits.error, DMIConsts.dmi_RESP_FAILURE, DMIConsts.dmi_RESP_SUCCESS)
    io.dmi.resp.bits.data  := tl.d.bits.data

    // Tie off unused channels
    tl.b.ready := false.B
    tl.c.valid := false.B
    tl.e.valid := false.B
  }
}
