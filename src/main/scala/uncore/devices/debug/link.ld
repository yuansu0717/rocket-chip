OUTPUT_ARCH( "riscv" )
ENTRY( entry )
SECTIONS
{
    .whereto 0x300 :
    {
        *(.whereto)
    }   
    .abstract 0x304 :
    {
        *(.abstract)
    }
    .prog_buffer 0x340 :
    {
        *(.prog_buffer)
    }   
    . = 0x800;
    .text :
    {
        *(.text)
    }
    _end = .;
}
