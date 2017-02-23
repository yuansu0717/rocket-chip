package uncore.devices

import Chisel._

// This file was auto-generated from the repository at https://github.com/sifive/riscv-debug-spec.git,
// 'make chisel'

object DMI_RegAddrs {
  /* The address of this register will not change in the future, because it
        contains \Fversion.  It has changed from version 0.11 of this spec.

        \begin{commentary}
        There are separate \Fhaltreq and \Fresumereq bits so a debugger can
        write \Rdmcontrol without affecting the current state, without knowing
        what that current state is.
        \end{commentary}
  */
  def DMI_DMCONTROL = UInt(0x00)

  def DMI_HARTINFO = UInt(0x01)

  /* This register contains a summary of which harts are halted.

        Each bit contains the logical OR of 32 halt bits. When there are a
        large number of harts in the system, the debugger can first read this
        register, and then read from the halt region (0x40--0x5f) to determine
        which hart is the one that is halted.
  */
  def DMI_HALTSUM = UInt(0x02)

  def DMI_SBCS = UInt(0x03)

  /* If \Fsbasize is 0, then this register is not present.

        When the system bus master is busy,
	writes to this register will return error
        and \Fsberror is set.

        If \Fsberror is 0 and \Fsbautoread is set then the system bus
	master will start
        to read after updating the address from \Faddress. The access size is
        controlled by \Fsbaccess in \Rdmcontrol.

        If \Fsbsingleread is set, the bit is cleared.
  */
  def DMI_SBADDRESS0 = UInt(0x04)

  def DMI_SBADDRESS1 = UInt(0x05)

  /* If \Fsbasize is less than 65, then this register is not present.
  */
  def DMI_SBADDRESS2 = UInt(0x06)

  /* If all of the {\tt sbaccess} bits in \Rsbcs are 0, then this register
        is not present.

        If \Fsberror isn't 0 then accesses return error, and don't do anything
        else.

        Writes to this register:

        1. If the bus master is busy then accesses set \Fsberror, return error,
        and don't do anything else.

        2. Update internal data.

        3. Start a bus write of the internal data to the internal address.

        4. If \Fsbautoincrement is set, increment the internal address.

        Reads to this register:

        1. If bits 31:0 of the internal data register haven't been updated
        since the last time this register was read, then set \Fsberror, return
        error, and don't do anything else.

        2. ``Return'' the data.

        3. If \Fsbautoincrement is set, increment the internal address.

        4. If \Fsbautoread is set, start another system bus read.
  */
  def DMI_SBDATA0 = UInt(0x07)

  /* If \Fsbaccesssixtyfour and \Fsbaccessonetwentyeight are 0, then this
        register is not present.
  */
  def DMI_SBDATA1 = UInt(0x08)

  /* This register only exists if \Fsbaccessonetwentyeight is 1.
  */
  def DMI_SBDATA2 = UInt(0x09)

  /* This register only exists if \Fsbaccessonetwentyeight is 1.
  */
  def DMI_SBDATA3 = UInt(0x0a)

  /* This register serves as a 32-bit serial port to the authentication
        module.

        When \Fauthbusy is clear, the debugger can communicate with the
        authentication module by reading or writing this register. There is no
        separate mechanism to signal overflow/underflow.
  */
  def DMI_AUTHDATA = UInt(0x0b)

  def DMI_ABSTRACTCS = UInt(0x0e)

  /* Writes to this register cause the corresponding abstract command to be
        executed.

        Writing while an abstract command is executing causes \Fcmderr to be set.

        If \Fcmderr is non-zero, writes to this register are ignored.

        \begin{commentary}
            \Fcmderr inhibits starting a new command to accommodate debuggers
            that, for performance reasons, send several commands to be executed
            in a row without checking \Fcmderr in between. They can safely do
            so and check \Fcmderr at the end without worrying that one command
            failed but then a later command (which might have depended on the
            previous one succeeding) passed.
        \end{commentary}
  */
  def DMI_COMMAND = UInt(0x0f)

  /* Basic read/write registers that may be read or changed by abstract
        commands.

        Accessing them while an abstract command is executing causes \Fcmderr
        to be set.

        The values in these registers may not be preserved after an abstract
        command is executed. The only guarantees on their contents are the ones
        offered by the command in question. If the command fails, no
        assumptions can be made about the contents of these registers.
  */
  def DMI_DATA0 = UInt(0x10)

  def DMI_DATA1 = UInt(0x11)

  def DMI_DATA2 = UInt(0x12)

  def DMI_DATA3 = UInt(0x13)

  def DMI_DATA4 = UInt(0x14)

  def DMI_DATA5 = UInt(0x15)

  def DMI_DATA6 = UInt(0x16)

  def DMI_DATA7 = UInt(0x17)

  def DMI_DATA8 = UInt(0x18)

  def DMI_DATA9 = UInt(0x19)

  def DMI_DATA10 = UInt(0x1a)

  def DMI_DATA11 = UInt(0x1b)

  /* If \Fserialcount is 0, this register is not present.

        This register provides access to the data queues of the serial port
        selected by \Fserial in \Rsercs.

        A read from this register reads the oldest entry in the
        debugger-to-core queue, and removes that entry from the queue.  If the
        queue is empty, then the read fails.

        A write to this register adds the written data to the core-to-debugger
        queue. If that queue is already full or its overflow bit is set, then
        the write fails and the overflow bit becomes set.
  */
  def DMI_SERDATA = UInt(0x1c)

  /* If \Fserialcount is 0, this register is not present.
  */
  def DMI_SERCS = UInt(0x1d)

  def DMI_PROGBUFCS = UInt(0x1f)

  /* The {\tt progbuf} registers provide read/write access to the optional
        program buffer.

        If \Fautoexeczero is set, then after this register is accessed the
        command in \Rcommand is executed again.
  */
  def DMI_PROGBUF0 = UInt(0x20)

  def DMI_PROGBUF1 = UInt(0x21)

  def DMI_PROGBUF2 = UInt(0x22)

  def DMI_PROGBUF3 = UInt(0x23)

  def DMI_PROGBUF4 = UInt(0x24)

  def DMI_PROGBUF5 = UInt(0x25)

  def DMI_PROGBUF6 = UInt(0x26)

  def DMI_PROGBUF7 = UInt(0x27)

  def DMI_PROGBUF8 = UInt(0x28)

  def DMI_PROGBUF9 = UInt(0x29)

  def DMI_PROGBUF10 = UInt(0x2a)

  def DMI_PROGBUF11 = UInt(0x2b)

}

class DMI_DMCONTROLFields extends Bundle {

  /* Halt request signal for the hart selected by \Fhartsel. When 1, the
            hart will halt if it's not currently halted.
            Setting both \Fhaltreq and \Fresumereq leads to undefined behavior.

            Writes apply to the new value of \Fhartsel.
  */
  val haltreq = Bool()

  /* Resume request signal for the hart selected by \Fhartsel. When 1,
            the hart will resume if it's currently halted.
            Setting both \Fhaltreq and \Fresumereq leads to undefined behavior.

            Writes apply to the new value of \Fhartsel.
  */
  val resumereq = Bool()

  val reserved0 = UInt(2.W)

  /* The status of the currently selected hart.

            0: Halted.

            1: Running.

            2: Unavailable (eg. powered down, held in reset).

            3: \Fhartsel specifies a hart that does not exist in this system.
  */
  val hartstatus = UInt(2.W)

  /* The DM-specific index of the hart to select.
  */
  val hartsel = UInt(10.W)

  val reserved1 = UInt(5.W)

  /* This optional bit controls reset to the currently selected hart. To
            perform a reset the debugger writes 1, and then writes 0 to
            deassert the reset signal.

            If this feature is not implemented, the bit always stays 0, so
            after writing 1 the debugger can read the register back to see if
            the feature is supported.
  */
  val hartreset = Bool()

  /* This bit serves as a reset signal for the Debug Module itself.

            0: The module, including authentication mechanism, is held in
            reset.

            1: The module functions normally.

            No other mechanism should exist that may result in resetting the
            Debug Module after power up, including the platform's system reset
            or Debug Transport reset signals.

            A debugger should pulse this bit low to ensure that the Debug
            Module is fully reset and ready to use.

            Implementations may use this bit to aid debugging, for example by
            preventing the Debug Module from being power gated while debugging
            is active.
  */
  val dmactive = Bool()

  /* This bit controls the reset signal from the DM to the rest of the
            system. To perform a reset the debugger writes 1, and then writes 0
            to deassert the reset.
  */
  val reset = Bool()

  /* 0 when authentication is required before using the DM.  1 when the
            authentication check has passed. On components that don't implement
            authentication, this bit must be preset as 1.
  */
  val authenticated = Bool()

  /* 0: The authentication module is ready to process the next
            read/write to \Rauthdata.

            1: The authentication module is busy. Accessing \Rauthdata results
            in unspecified behavior.

            \Fauthbusy only becomes set in immediate response to an access to
            \Rauthdata.
  */
  val authbusy = Bool()

  val reserved2 = UInt(2.W)

  /* 0: There is no Debug Module present.

            1: There is a Debug Module and it conforms to version 0.12 of this
            specification.

            Other values are reserved for future use.
  */
  val version = UInt(4.W)

}

class DMI_HARTINFOFields extends Bundle {

  val reserved0 = UInt(15.W)

  /* 0: The {\tt data} registers are shadowed in the hart by CSR
            registers. Each CSR register is XLEN bits in size, and corresponds
            to a single argument, per Table~\ref{tab:datareg}.

            1: The {\tt data} registers are shadowed in the hart's memory map.
            Each register takes up 4 bytes in the memory map.
  */
  val dataaccess = Bool()

  /* If \Fdataaccess is 0: Number of CSR registers dedicated to
            shadowing the {\tt data} registers.

            If \Fdataaccess is 1: Number of 32-bit words in the memory map
            dedicated to shadowing the {\tt data} registers.
  */
  val datasize = UInt(4.W)

  /* If \Fdataaccess is 0: The number of the first CSR dedicated to
            shadowing the {\tt data} registers.

            If \Fdataaccess is 1: Signed address of RAM where the {\tt data}
            registers are shadowed.
  */
  val dataaddr = UInt(12.W)

}

class DMI_HALTSUMFields extends Bundle {

  val halt1023_992 = Bool()

  val halt991_960 = Bool()

  val halt959_928 = Bool()

  val halt927_896 = Bool()

  val halt895_864 = Bool()

  val halt863_832 = Bool()

  val halt831_800 = Bool()

  val halt799_768 = Bool()

  val halt767_736 = Bool()

  val halt735_704 = Bool()

  val halt703_672 = Bool()

  val halt671_640 = Bool()

  val halt639_608 = Bool()

  val halt607_576 = Bool()

  val halt575_544 = Bool()

  val halt543_512 = Bool()

  val halt511_480 = Bool()

  val halt479_448 = Bool()

  val halt447_416 = Bool()

  val halt415_384 = Bool()

  val halt383_352 = Bool()

  val halt351_320 = Bool()

  val halt319_288 = Bool()

  val halt287_256 = Bool()

  val halt255_224 = Bool()

  val halt223_192 = Bool()

  val halt191_160 = Bool()

  val halt159_128 = Bool()

  val halt127_96 = Bool()

  val halt95_64 = Bool()

  val halt63_32 = Bool()

  val halt31_0 = Bool()

}

class DMI_SBCSFields extends Bundle {

  val reserved0 = UInt(11.W)

  /* When a 1 is written here, triggers a read at the address in {\tt
            sbaddress} using the access size set by \Fsbaccess.
  */
  val sbsingleread = Bool()

  /* Select the access size to use for system bus accesses triggered by
            writes to the {\tt sbaddress} registers or \Rsbdatazero.

	    0: 8-bit

            1: 16-bit

	    2: 32-bit

	    3: 64-bit

	    4: 128-bit

            If an unsupported system bus access size is written here,
	    the DM may not
            perform the access, or may perform the access with any access size
  */
  val sbaccess = UInt(3.W)

  /* When 1, the internal address value (used by the system bus master)
            is incremented by the access size (in bytes) selected in \Fsbaccess
            after every system bus access.
  */
  val sbautoincrement = Bool()

  /* When 1, every read from \Rsbdatazero automatically triggers a system
            bus read at the new address.
  */
  val sbautoread = Bool()

  /* When the debug module's system bus
	  master causes a bus error, this field gets set.
          It remains set until 0 is written to any bit in this field. Until
          that happens, the system bus master is busy and no more accesses can be
            initiated by the debug module.

            0: There was no bus error.

            1: There was a timeout.

            2: A bad address was accessed.

            3: There was some other error (eg. alignment).

            4: The system bus master was busy when a one of the
            {\tt sbaddress} or {\tt sbdata} registers was written,
            or the {\tt sbdata} register was read when it had
            stale data.
  */
  val sberror = UInt(3.W)

  /* Width of system bus addresses in bits. (0 indicates there is no bus
            access support.)
  */
  val sbasize = UInt(7.W)

  /* 1 when 128-bit system bus accesses are supported.
  */
  val sbaccess128 = Bool()

  /* 1 when 64-bit system bus accesses are supported.
  */
  val sbaccess64 = Bool()

  /* 1 when 32-bit system bus accesses are supported.
  */
  val sbaccess32 = Bool()

  /* 1 when 16-bit system bus accesses are supported.
  */
  val sbaccess16 = Bool()

  /* 1 when 8-bit system bus accesses are supported.
  */
  val sbaccess8 = Bool()

}

class DMI_SBADDRESS0Fields extends Bundle {

  /* Accesses bits 31:0 of the internal address.
  */
  val address = UInt(32.W)

}

class DMI_SBADDRESS1Fields extends Bundle {

  /* Accesses bits 63:32 of the internal address (if the system address
            bus is that wide).
  */
  val address = UInt(32.W)

}

class DMI_SBADDRESS2Fields extends Bundle {

  /* Accesses bits 95:64 of the internal address (if the system address
            bus is that wide).
  */
  val address = UInt(32.W)

}

class DMI_SBDATA0Fields extends Bundle {

  /* Accesses bits 31:0 of the internal data.
  */
  val data = UInt(32.W)

}

class DMI_SBDATA1Fields extends Bundle {

  /* Accesses bits 63:32 of the internal data (if the system bus is
            that wide).
  */
  val data = UInt(32.W)

}

class DMI_SBDATA2Fields extends Bundle {

  /* Accesses bits 95:64 of the internal data (if the system bus is
            that wide).
  */
  val data = UInt(32.W)

}

class DMI_SBDATA3Fields extends Bundle {

  /* Accesses bits 127:96 of the internal data (if the system bus is
            that wide).
  */
  val data = UInt(32.W)

}

class DMI_AUTHDATAFields extends Bundle {

  val data = UInt(32.W)

}

class DMI_ABSTRACTCSFields extends Bundle {

  val reserved0 = UInt(16.W)

  val autoexec7 = Bool()

  val autoexec6 = Bool()

  val autoexec5 = Bool()

  val autoexec4 = Bool()

  val autoexec3 = Bool()

  val autoexec2 = Bool()

  val autoexec1 = Bool()

  /* When 1, accesses to \Rdatazero cause the command in \Rcommand to be
            executed again.

            The same is true for other other autoexec bits: When 1, accesses to
            {\tt data}N cause the command in \Rcommand to be executed again.
  */
  val autoexec0 = Bool()

  /* Gets set if an abstract command fails. No abstract command is
            started until the value is reset to 0.

            0 (none): No error.

            1 (busy): An abstract command was executing while \Rcommand or one
            of the {\tt data} registers was accessed.

            2 (not supported): The requested command is not supported. A
            command that is not supported while the hart is running may be
            supported when it is halted.

            3 (exception): An exception occurred while executing the command
            (eg. while executing the Program Buffer).

            4 (halt/resume): An abstract command couldn't execute because the
            hart wasn't in the expected state (running/halted).

            7 (other): The command failed for another reason.
  */
  val cmderr = UInt(3.W)

  /* 1: An abstract command is currently being executed.

            This bit is set as soon as \Rcommand is written, and isn't cleared
            until that command has completed.
  */
  val busy = Bool()

  /* Number of {\tt data} registers that are implemented as part of the
            abstract command interface. If it's 0 then no abstract interface is
            implemented at all.
  */
  val datacount = UInt(4.W)

}

class DMI_COMMANDFields extends Bundle {

  /* The type determines the overall functionality of this
            abstract command.
  */
  val _type = UInt(8.W)

  /* This field is interpreted in a command-specific manner,
            described for each abstract command.
  */
  val control = UInt(24.W)

}

class DMI_DATA0Fields extends Bundle {

  val data = UInt(32.W)

}

class DMI_SERDATAFields extends Bundle {

  val data = UInt(32.W)

}

class DMI_SERCSFields extends Bundle {

  /* Number of supported serial ports.
  */
  val serialcount = UInt(4.W)

  val reserved0 = UInt(9.W)

  /* Select which serial port is accessed by \Rserdata.
  */
  val serial = UInt(3.W)

  val valid7 = Bool()

  val full_overflow7 = Bool()

  val valid6 = Bool()

  val full_overflow6 = Bool()

  val valid5 = Bool()

  val full_overflow5 = Bool()

  val valid4 = Bool()

  val full_overflow4 = Bool()

  val valid3 = Bool()

  val full_overflow3 = Bool()

  val valid2 = Bool()

  val full_overflow2 = Bool()

  val valid1 = Bool()

  val full_overflow1 = Bool()

  /* 1 when the core-to-debugger queue for serial port 0 is not empty.
  */
  val valid0 = Bool()

  /* 1 when the debugger-to-core queue for serial port 0 is either full,
            or has overflowed. Overflow state is sticky, and can be reset by
            writing 0 to this bit.
  */
  val full_overflow0 = Bool()

}

class DMI_PROGBUFCSFields extends Bundle {

  val reserved0 = UInt(28.W)

  /* Size of the Program Buffer, in 32-bit words. Valid sizes are 0 - 12.

            A debugger must not access any Program Buffer locations that
            fall outside the range specified here.

	    TODO: Explain what can be done with each size of the buffer, to suggest
	    why you would want more or less words.
  */
  val progsize = UInt(4.W)

}

class DMI_PROGBUF0Fields extends Bundle {

  val data = UInt(32.W)

}

