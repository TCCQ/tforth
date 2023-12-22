### This is meant to be a simple wrapper to allow firing up in qemu to
### test with, since I don't have a riscv laptop or whatever that can
### execute natively.
## 16550a UART, adapted from old reedos module
        .option nopic

        .macro save_all_regs_to_fp
        addi  fp,   fp,  -256
        sd    x0,   0(fp)
        sd    x1,   8(fp)
        sd    x2,   16(fp)
        sd    x3,   24(fp)
        sd    x4,   32(fp)
        sd    x5,   40(fp)
        sd    x6,   48(fp)
        sd    x7,   56(fp)
        sd    x8,   64(fp)
        sd    x9,   72(fp)
        sd    x10,  80(fp)
        sd    x11,  88(fp)
        sd    x12,  96(fp)
        sd    x13,  104(fp)
        sd    x14,  112(fp)
        sd    x15,  120(fp)
        sd    x16,  128(fp)
        sd    x17,  136(fp)
        sd    x18,  144(fp)
        sd    x19,  152(fp)
        sd    x20,  160(fp)
        sd    x21,  168(fp)
        sd    x22,  176(fp)
        sd    x23,  184(fp)
        sd    x24,  192(fp)
        sd    x25,  200(fp)
        sd    x26,  208(fp)
        sd    x27,  216(fp)
        sd    x28,  224(fp)
        sd    x29,  232(fp)
        sd    x30,  240(fp)
        sd    x31,  248(fp)
        .endm

        .macro restore_all_regs_from_fp
        ld    x0,   0(fp)
        ld    x1,   8(fp)
        ld    x2,   16(fp)
        ld    x3,   24(fp)
        ld    x4,   32(fp)
        ld    x5,   40(fp)
        ld    x6,   48(fp)
        ld    x7,   56(fp)
        ld    x8,   64(fp)
        ld    x9,   72(fp)
        ld    x10,  80(fp)
        ld    x11,  88(fp)
        ld    x12,  96(fp)
        ld    x13,  104(fp)
        ld    x14,  112(fp)
        ld    x15,  120(fp)
        ld    x16,  128(fp)
        ld    x17,  136(fp)
        ld    x18,  144(fp)
        ld    x19,  152(fp)
        ld    x20,  160(fp)
        ld    x21,  168(fp)
        ld    x22,  176(fp)
        ld    x23,  184(fp)
        ld    x24,  192(fp)
        ld    x25,  200(fp)
        ld    x26,  208(fp)
        ld    x27,  216(fp)
        ld    x28,  224(fp)
        ld    x29,  232(fp)
        ld    x30,  240(fp)
        ld    x31,  248(fp)
        addi  fp,   fp,  256
        .endm


        .data
welcome_msg:
        .asciz "Hello and Welcome to third!\n"

        .text
### program start entry. Do setup and handoff to forth
        .global _entry
_entry:
        ## just to be sure, only hart0 is allowed
        csrr t1, mhartid
        bnez t1, panic

        ## stack/register setup, see linker script and baked-ins
        la fp, _FORTH_MEM_TOP
        la sp, _FORTH_MEM_MID
        .extern data_stack_next_byte
        la t0, data_stack_next_byte
        .extern current_dict_entry
        la gp, current_dict_entry
        mv tp, x0               #not executing anything yet
        mv ra, x0               #haven't been anywhere
        ## do init now that we have stacks
        call enable_ints        #specifically which we want
        call init_uart
        call init_plic

        csrr t1, mstatus
        li t2, 1
        slli t2, t2, 3
        or t1, t1, t2
        csrw mstatus, t1
        ## globally enable interrupts

        ## pass it off to forth
        la a0, welcome_msg
        ## call output_string

        .extern interpret_entry
        j interpret_entry
        mret


        ## procedure to get the uart to behave as we expect. ripped from reedos
        .set IER_OFFSET, 1                      # Interrupt Enable Register
        .set LCR_OFFSET, 3                      # Line Control Register (baud rate stuff)
        .set FCR_OFFSET, 2                      # FIFO Control Register (see uart layout in reference)
init_uart:
        li t1, 1
        sll t1, t1, 28
        ## t1 is uart base addr
        sb x0, IER_OFFSET(t1) #disable int
        li t3, 1
        slli t3, t3, 7
        sb t3, LCR_OFFSET(t1) #mode to set baud
        li t3, 3
        sb t3, (t1)             #LSB tx
        sb x0, 1(t1)            #MSB rx
        li t3, 3
        sb t3, LCR_OFFSET(t1) #8bit words, no parity
        li t3, 0x7
        sb t3, FCR_OFFSET(t1) #enable and clear fifo
        li t3, 0x3
        sb t3, IER_OFFSET(t1) #enable interupts
        ret

        ## set out machine csrs to allow uart input events and that's it
enable_ints:
        csrw medeleg, x0
        csrw mideleg, x0
        la t1, int_handler
        srli t1, t1, 2
        slli t1, t1, 2          #zero bottom two bits
        csrw mtvec, t1          #set handler
        li t1, 1
        slli t1, t1, 11
        csrw mie, t1            #machine ext int enable
        ret

        ## try to enable uart (10) with priority 1, and set hart 0 to
        ## have threshold 0, all in context 0 (hart 0, M mode)
init_plic:
        li t1, 0x0c
        slli t1, t1, 24
        ## t1 has plic base address

        li t2, 1
        sw t2, 40(t1)           #word gran. enable for UART irq (priority 1)

        li t2, 1
        slli t2, t2, 10         #uart irq bit mask
        li t3, 0x2000           #0x2000, base for enable bits for context 0
        add t3, t1, t3
        sw t2, (t3)             #mask location for hart 0, S mode

        li t3, 0x200000         #get 0x200000, threshold for context 0
        add t3, t1, t3
        sw x0, (t3)             #has priority threshold 0
        ret

        .balign 0x8
        ## where stvec should send us, in direct mode
int_handler:
        save_all_regs_to_fp
        csrr s1, mcause
        li s2, 1
        slli s2, s2, 63         #set top bit
        ori s2, s2, 0xB         #machine ext int
        sub s1, s1, s2
        .extern panic
        bnez s1, panic

        call plic_claim
        addi s1, a0, -10         #uart irq
        bnez s1, panic

        ## rad, we can read a new character
        call read_char_blocking_uart #into a0
        mv a1, a0
        call write_char_uart    #echo from a1
        .extern input_character
        call input_character    #from a0

        li a0, 10
        call plic_complete

        restore_all_regs_from_fp
        sret


        ## this and the next one have hart id offsets, but we are
        ## fixed to hart 0, so we don't bother. They also use s1-2
        ## without care since they are only ever called from inside
        ## the int_handler

        ## places irq number in a0. redudant since there should only
        ## ever be one
plic_claim:
        ## only a single hart, but we need to do it anyway
        li s1, 0x0c
        slli s1, s1, 24         #base addr
        li s2, 0x200000
        addi s2, s2, 4
        add s1, s1, s2          #base + 0x200004
        lw a0, (s1)
        ret

        ## sakes irq number in a0
plic_complete:
        li s1, 0x0c
        slli s1, s1, 24         #base addr
        li s2, 0x200000
        addi s2, s2, 4
        add s1, s1, s2          #base + 0x200004
        sw a0, (s1)
        ret

write_char_uart: # get char in a1
        li t1, 1
        sll t1, t1, 28
        sb a1, (t1)
        ret

read_char_blocking_uart:                 # put char in a0
        li t1, 1
        sll t1, t1, 28
rcbu_loop:
        lb t2, 5(t1)
        li t3, 1
        and t2, t2, t3
        bnez t2, rcbu_done
        j rcbu_loop
rcbu_done:
        lb a0, (t1)
        ret

### expected explicitly in third.s

        ## takes single char in a1
        .global output_char
output_char:
        j write_char_uart

        ## take ptr to NT-string in a0
        .global output_string
output_string:
        addi fp, fp, -8
        sd ra, (fp)
out_str_loop:
        lb a1, (a0)
        beqz a1, out_str_done
        call write_char_uart
        addi a0, a0, 1
        j out_str_loop
out_str_done:
        ld ra, (fp)
        addi fp, fp, 8
        ret
