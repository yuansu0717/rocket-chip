#include "spike/encoding.h"

	// These are implementation-specific addresses in the Debug Module
	// Probably we don't need them all to be seperate.
#define HALTED    0x100
#define GOING     0x104
#define RESUMING  0x108
#define EXCEPTION 0x10C

// Region of memory where each hart has 1
// byte to read.
#define OK_GO 0x400

        .option norvc
	.global entry
	.global exception
	
	// Entry location on ebreak, Halt, or Breakpoint/
	// It is the same for all harts. They branch when 
	// their OK_GO bit is set.
	
entry:
       jal zero, _entry
resume:
       jal zero, _resume
exception: 
       jal zero, _exception

_entry:
        // This fence is required because the execution may have written something
        // into the Abstract Data or Program Buffer registers. The write to 'HALTED'
        // is used to advance the state machine.
        fence
	csrw CSR_DSCRATCH, s0  // Save s0 to allow signaling MHARTID
	csrr s0, CSR_MHARTID  
	sw   s0, HALTED(zero)

entry_loop:
	lb   s0, OK_GO(s0) // 1 byte flag per hart. Only one hart advances.
	bne  zero, s0, going
	csrr s0, CSR_MHARTID
	jal  zero, entry_loop


_exception:
        sw      zero, EXCEPTION(zero) // Let debug module know you got an exception.
        ebreak

// Below here, some portions are "Variable ROM". From the processor's perspective,
// it is ROM in that it cannot be written. But the Debug Module can change some of 
// the "ROM" contents as needed.

going:
	csrr s0, CSR_DSCRATCH          // Restore s0 here
	sw zero, GOING(zero)           // When debug module sees this write, the OK_GO flag is reset.
        jalr zero, zero, %lo(whereto)  // Rocket-Chip has a specific hack which is that jalr in 
	                               // Debug Mode will flush the I-Cache. We need that so that the 
			               // remainder of the variable instructions will be what Debug Module
	                               // intends.
_resume:
        csrw CSR_DSCRATCH, s0   // Save s0 to allow signaling MHARTID
	csrr s0, CSR_MHARTID  
        sw   s0, RESUMING(zero) // Let the Debug Module know you're not halted anymore.
        csrr s0, CSR_DSCRATCH   // Restore s0
        dret

        // END OF ACTUAL "ROM" CONTENTS. BELOW ARE JUST TO MAKE IT SO I DON'T HAVE TO MANUALLY
        // ASSEMBLE CODE THAT THE HW WILL PRODUCE.
        
.section .whereto
whereto:
                              // Variable "ROM" This is : jal x0 abstract, jal x0 program_buffer, 
	jal zero, resume      //                or jal x0 resume, as desired.	
	jal zero, abstract    //                Debug Module state machine tracks what is 'desired'.
	jal zero, prog_buffer //                We don't need to use jalr here because all of the 
			      //                Variable ROM contents are set by 
			      //                Debug Module before setting the OK_GO byte.

        
.section .abstract
abstract:
	nop                 // Variable "ROM" These are replaced by Debug Module with the instructions needed 
	nop                 //                to implement the abstract GPR access commands.
	
  	ebreak              // Signal completion by going back to hart_N_entry.

.section .prog_buffer
prog_buffer:
	nop	            // Variable "ROM" These are written directly by the debugger.
	nop	            // Variable "ROM"
	nop
	ebreak              // Variable "ROM" This is required to be written by the debugger.
