### This is meant to be a simple wrapper to allow firing up in qemu to
### test with, since I don't have a riscv laptop or whatever that can
### execute natively.

## 16550a UART, adapted from old reedos module


        .set UART_BASE, 0x10000000
        .set IER_OFFSET, 1                      # Interrupt Enable Register
        .set LCR_OFFSET, 3                      # Line Control Register (baud rate stuff)
        .set FCR_OFFSET, 2                      # FIFO Control Register (see uart layout in reference)
                                                # const LSR: usize = 2; // Line Status Register (ready to rx, ready to tx signals)


init_uart:
        li t1, 1
        sll t1, t1, 24
        ## t1 is uart base addr
        sc x0, (t1), IER_OFFSET #disable int
        li t3, 1
        sll t3, t3, 7
        lc t3, (t1), LCR_OFFSET #mode to set baud
        li t3, 3
        lc t3, (t1)             #LSB tx
        mv t3, x0
        lc t3, (t1), 1          #MSB rx
        li t3, 3
        lc t3, (t1), LCR_OFFSET #8bit words, no parity
        li t3, 0x7
        lc t3, (t1), FCR_OFFSET #enable fifo
        li t3, 0x3
        lc t3, (t1), IER_OFFSET #enable interupts
        ret
    ## pub fn init(&mut self) {
    ##     // https://mth.st/blog/riscv-qemu/AN-491.pdf <-- inclues 16650A ref
    ##     let ptr = self.base_address as *mut u8;
    ##     // Basic semantics:
    ##     // `ptr` is a memory address.
    ##     // We want to write certain values to 'registers' located
    ##     // at specific offsets, calculated by ptr + register_offset.
    ##     // Then, we perform volatile writes to that location in memory
    ##     // to configure the specific parameters of the Qemu virt machine
    ##     // uart device without altering our base address.
    ##     unsafe {
    ##         // Disable interrupts first.
    ##         ptr.add(IER).write_volatile(0x0);
    ##         // Mode in order to set baud rate.
    ##         ptr.add(LCR).write_volatile(1 << 7);
    ##         // baud rate of 38.4k
    ##         ptr.add(0).write_volatile(0x03); // LSB (tx side)
    ##         ptr.add(1).write_volatile(0x00); // MST (rx side)
    ##         // 8 bit words (no parity)
    ##         ptr.add(LCR).write_volatile(3);
    ##         // Enable and clear FIFO
    ##         ptr.add(FCR).write_volatile(1 << 0 | 3 << 1);
    ##         // Enable tx and rx interrupts
    ##         ptr.add(IER).write_volatile(1 << 1 | 1 << 0);
    ##     }
    ## }

write_char_uart: # get char in a1
        li t1, 1
        sll t1, t1, 24
        lc a1, (t1)
        ret
    ## pub fn put(&mut self, c: u8) {
    ##     let ptr = self.base_address as *mut u8;
    ##     unsafe {
    ##         ptr.add(0).write_volatile(c);
    ##     }
    ## }

read_char_blocking_uart:                 # put char in a0
        li t1, 1
        sll t1, t1, 24
rcbu_loop:
        lc t2, (t1), 5
        li t3, 1
        and t2, t2, t3
        bnez rcbu_done
        j rcbu_loop
rcbu_done:
        lc a0, (t1)
        ret

##     pub fn get(&mut self) -> Option<u8> {
##         let ptr = self.base_address as *mut u8;
##         unsafe {
##             if ptr.add(5).read_volatile() & 1 == 0 {
##                 // The DR bit is 0, meaning no data
##                 None
##             } else {
##                 // The DR bit is 1, meaning data!
##                 Some(ptr.add(0).read_volatile())
##             }
##         }
##     }

### expected explicitly in third.s

        ## takes single char in a1
output_char:
        j write_char_uart

        ## take ptr to NT-string in a0
output_string:
        subi fp, fp, 8
        sd ra, (fp)
out_str_loop:
        lc a1, (a0)
        beqz a1, out_str_done
        call write_char_uart
        addi a0, a0, 1
        j out_str_loop
out_str_done:
        sd ra, (fp)
        addi fp, fp, 8
        ret

        ## where mtvec should send us, in direct mode
int_handler:
        subi fp, fp, 32
        sd ra, (fp)
        sd s1, (fp), 8
        sd s2, (fp), 16
        sd a0, (fp), 24
        csrr s1, mcause
        li s2, 1
        slli s2, s2, 63         #set top bit
        ori s2, s2, 0xB         #machine ext int
        bneq s1, s2, panic
        ## rad, we can read a new character
        call read_char_blocking_uart #into a0

        .extern input_character
        call input_character    #from a0
        ld ra, (fp)
        ld s1, (fp), 8
        ld s2, (fp), 16
        ld a0, (fp), 24
        addi fp, fp, 32
        mret

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
        csrw mie, t1            #machine ext itnerupt enable
        ret

### program start entry. Do setup and handoff to forth
_entry:
        ## stack/register setup, see linker script and baked-ins
        la fp, _FORTH_MEM_TOP
        la sp, _FORTH_MEM_MID
        la t0, data_stack_next_byte
        la gp, current_dict_entry
        mv tp, x0               #not executing anything yet
        mv ra, x0               #haven't been anywhere
        ## do init not that we have stacks
        call init_uart
        call enable_ints
        ## pass it off to forth
        .global interpret_entry
        jr interpret_entry

