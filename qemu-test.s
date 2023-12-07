### This is meant to be a simple wrapper to allow firing up in qemu to
### test with, since I don't have a riscv laptop or whatever that can
### execute natively.
## 16550a UART, adapted from old reedos module
        .option nopic

        .set UART_BASE, 0x10000000
        .set IER_OFFSET, 1                      # Interrupt Enable Register
        .set LCR_OFFSET, 3                      # Line Control Register (baud rate stuff)
        .set FCR_OFFSET, 2                      # FIFO Control Register (see uart layout in reference)
                                                # const LSR: usize = 2; // Line Status Register (ready to rx, ready to tx signals)

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
        ## do init not that we have stacks
        call init_uart
        call enable_ints
        call init_plic

        csrr t1, mstatus
        li t2, 1
        slli t2, t2, 3
        or t1, t1, t2
        csrw mstatus, t1
        ## enable interupts

        ## pass it off to forth
        la a0, welcome_msg
        call output_string

        .extern interpret_entry
        j interpret_entry

init_uart:
        li t1, 1
        sll t1, t1, 28
        ## t1 is uart base addr
        sb x0, IER_OFFSET(t1) #disable int
        li t3, 1
        sll t3, t3, 7
        lb t3, LCR_OFFSET(t1) #mode to set baud
        li t3, 3
        lb t3, (t1)             #LSB tx
        mv t3, x0
        lb t3, 1(t1)          #MSB rx
        li t3, 3
        lb t3, LCR_OFFSET(t1) #8bit words, no parity
        li t3, 0x7
        lb t3, FCR_OFFSET(t1) #enable fifo
        li t3, 0x3
        lb t3, IER_OFFSET(t1) #enable interupts
        ret

        ## set out machine csrs to allow uart input events and that's it
enable_ints:
        csrw medeleg, x0
        csrw mideleg, x0
        la t1, int_handler
        srli t1, t1, 2
        slli t1, t1, 2          #zero bottom two bits
        csrw mtvec, t1          #set handler
        ## csrw stvec, t1          # for both modes
        li t1, 1
        slli t1, t1, 11
        csrw mie, t1            #machine ext itnerupt enable
        ## srli t1, t1, 2
        ## csrw sie, t1            #for both modes
        ret
        ## TODO do plic init to allow for uart inputs to come in?

init_plic:
        li t1, 0x0c
        slli t1, t1, 24
        ## t1 has plic base address
        li t2, 1
        sw t2, 40(t1)           #word gran. enable for UART irq (priority 1)
        li t2, 1
        slli t2, t2, 10         #uart irq bit mask
        lui t3, 0x2             #0x2000, base for enable bits for context 0
        add t3, t1, t3
        sw t2, (t3)             #mask location for hart 0, 0x2000 + 0x100 * hartid
        ## enabled for hart 0
        lui t3, 0x200           #get 0x200000
        add t3, t1, t3
        sw x0, (t3)             #has priority threshold 0
        ret

        ## where mtvec should send us, in direct mode
int_handler:
        addi fp, fp, -32
        sd ra, (fp)
        sd s1, 8(fp)
        sd s2, 16(fp)
        sd a0, 24(fp)
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

        .extern input_character
        call input_character    #from a0

        li a0, 10
        call plic_complete

        ld ra, (fp)
        ld s1, 8(fp)
        ld s2, 16(fp)
        ld a0, 24(fp)
        addi fp, fp, 32
        mret


        ## this and the next one have hart id offsets, but we are
        ## fixed to hart 0, so we don't bother. They also use s1-2
        ## without care since they are only ever called from inside
        ## the int_handler

        ## places irq number in a0. redudant since there should only
        ## ever be one
plic_claim:
        ## only a single hart, but we need to do it anyway
        li s1, 0xc0
        slli s1, s1, 12         #base addr
        lui s2, 0x200
        addi s2, s2, 4
        add s1, s1, s2          #base + 0x200004
        lw a0, (s1)
        ret

        ## sakes irq number in a0
plic_complete:
        li s1, 0xc0
        slli s1, s1, 12         #base addr
        lui s2, 0x200
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
