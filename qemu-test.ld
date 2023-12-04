/* Stolen from an old reedos branch */
OUTPUT_ARCH( "riscv" )
ENTRY( _entry )

MEMORY
{
  RAM  (wxa) : ORIGIN = 0x80000000, LENGTH = 128M
}

SECTIONS
{
  . = ORIGIN(RAM);

  .text : {
    *(.text._entry)
    *(.text.entry)
    *(.text .text.*)
  }

  .bss : {
    *(.sbss .sbss.*)
    *(.bss .bss.*)
    . = ALIGN(0x8);
  }

  .data : {
    *(.sdata .sdata.*)
    *(.data .data.*)
  }
  /* data needs to be the last section here */


  PROVIDE(_FORTH_DATA_BOTTOM = .);

  PROVIDE(_FORTH_MEM_MID = ORIGIN(RAM) + (LENGTH(RAM) / 2));

  PROVIDE(_FORTH_MEM_TOP = ORIGIN(RAM) + LENGTH(RAM));
}
