OUTPUT_ARCH( "riscv" )
ENTRY( entry )
SECTIONS
{
    .whereto 0x900 :
    {
        *(.whereto)
    }   
    .abstract 0x910 :
    {
        *(.abstract)
    }
    .prog_buffer 0xA00 :
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
