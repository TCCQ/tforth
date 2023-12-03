### This is a minimal FORTH for riscv-64-g. It isn't beholden to any
### particular spec, but tries to be a generally predictable FORTH. It
### makes a lot of simplifying assumptions, so read on for those. They
### are noted as relevant in the source.
###
### It should be compiled with GNU as, which is part of the standard
### GNU toolchain

### All numbers are signed and everything is in 8byte words

### The register assignemnt is as follows:
###
### all stack pointers point to the topmost used byte/word on the
### stack unless otherwise noted
###
### sp is the main argument stack (grows down)
### gp points to the top dictionary definiton
### t0 points to the first unallocated byte of the data stack (grows up)
###
### tp is a indirect pointer to the currently executing word. It
### points to the word in the body of the caller function that caused
### to current subroutine to be called. So derefing twice gives the
### head of the word. But derefing once lets you peek at the
### surrounding context. We need this for literals. This only has real
### meaning during the execution of colon definitions, or other
### compiled words.
###
### ra is the topmost return address, as normal
### fp is the top of the return stack (grows down)

### The dict format is as follows
###
### pointer points here:
### 8byte ptr to the null terminated string name of this entry
### 8byte ptr to run time code
### 8byte ptr to compile time code
### 8byte ptr to next dict definition
### body of the dict starts here, makeup is definition dependent.
###
### Most entries have their null terminated name on the data stack
### immediately prior to them, but that is not enforced explicitly

        .bss
word_buffer:
        .skip 256, 0

mode:                           #zero for runtime, non-zero for compile time
        .skip 8, 0

        .data
succ_text:
        .anciz "ok.\n"

### Main code

        .text

panic:
        j panic


### Code for arithmetic
add_l:
        addi sp, sp, 8
        ld t1, sp, -8
        ld t2, sp, 0
        add t2, t1, t2
        sd t2, (sp), 0
        ret

sub_l:
        addi sp, sp, 8
        ld t1, sp, -8
        ld t2, sp, 0
        sub t2, t1, t2
        sd t2, (sp), 0
        ret

mul_l:
        addi sp, sp, 8
        ld t1, sp, -8
        ld t2, sp, 0
        mul t2, t1, t2
        sd t2, (sp), 0
        ret
mulh_l:                         #high on top, low bits second on stack
        ld t1, sp, 0
        ld t2, sp, 8
        mul t2, t1, t2
        mulh t1, t1, t2
        sd t1, (sp), 0
        sd t2, (sp), 8
        ret

div_l:
        addi sp, sp, 8
        ld t1, sp, -8
        ld t2, sp, 0
        div t2, t1, t2
        sd t2, (sp), 0
        ret
rem_l:
        addi sp, sp, 8
        ld t1, sp, -8
        ld t2, sp, 0
        rem t2, t1, t2
        sd t2, (sp), 0
        ret
divr_:                          #quot on top, remainder second
        ld t1, sp, 0
        ld t2, sp, 8
        div t1, t1, t2
        rem t2, t1, t2
        sd t1, (sp), 0
        sd t2, (sp), 8
        ret

### stack manip

drop_l:
        addi sp, sp, 8
        ret

drop_twice_l:
        addi sp, sp, 16
        ret

dup_l:
        subi sp, sp, 8
        ld t1, (sp), 8
        sd t1, (sp)
        ret

swap_l:
        ld t1, (sp)
        ld t2, (sp), 8
        sd t2, (sp)
        sd t1, (sp), 8
        ret

rot_l:                          #(a b c -- b c a)
        ld t1, (sp)
        ld t2, (sp), 8
        ld t3, (sp), 16
        sd t3, (sp)
        sd t1, (sp), 8
        sd t2, (sp), 16

nip_l:                          #(a b -- b)
        addi sp, sp, 8
        ld t1, (sp) -8
        sd t1, (sp)
        ret

tuck_l:                         #(a b -- a b a)
        subi sp, sp, 8
        ld t1, (sp) -16
        sd t1, (sp)
        ret

### return stack manip

mov_to_ret_l:                   #stack to ret stack
        addi sp, sp, 8
        subi fp, fp, 8
        ld t1, (sp), -8
        sd t1, (fp)
        ret

mov_from_ret_l:                 #ret stack to stack
        subi sp, sp, 8
        addi fp, fp, 8
        ld t1, (fp), -8
        sd t1, (sp)
        ret

copy_from_ret_l:                #ret stack to stack, preserve ret
        subi sp, sp, 8
        ld t1, (fp)
        sd t1, (sp)
        ret

### code for data stack manipulation

variable_addr_l:                # the code for runtime vars
        ld t1, (tp)
        addi t1, t1, 32         #point to current body
        subi sp, sp, 8
        sd t1, (sp)
        ret

write_l:                   #write a word to an addr. (val addr -- )
        addi sp, sp, 16
        ld t1, (sp), -16         #addr
        ld t2, (sp), -8          #val
        sd t2 (t1)
        ret

read_l:                         #read word from addr to stack (addr -- val)
        subi sp, sp, 8
        ld t1, (sp), 8              #addr
        ld t1, (t1)
        sd t1, (sp)
        ret


## read a number from the stack, and allocate that many 8byte words on
## the data stack, pushing the address to the stack. Using negative
## numbers works for deallocating, but you probably should drop the
## pushed value
alloc_l:
        ld t1, (sp)
        mv t2, t0
        addi t0, t0, t1
        sd t2, (sp)
        ret

        ## should be the default compile time semantics. Inserts the
        ## currently executing definition (which has these ct
        ## semantics)'s runtime ptr in the body of the word being
        ## defined currently
self_insert_l:
        ld t1, (tp)
        addi t1, t1, 8
        ld t1, t1
        sd t1, (t0)     #append to data field
        addi t0, t0, 8          #increment the data field (of the current def)
        ret

comma_l:                        #pop a val off the stack and push it to the data stack
        addi sp, sp, 8
        addi t0, t0, 8
        ld t1, (sp), -8
        sd t1, (t0)
        ret

### string words

show_l:                         # (a -- ) print the top of the stack as a number
        ld a0, (sp)
        addi sp, sp, 8
        subi fp, fp, 8
        sd ra, (fp)
        call output_num
        ld ra, (fp)
        addi fp, fp, 8          #TODO we can do this on the main stack, and skip some of arithmetic
        ret

### TODO ct string storage
### stringprint from NT-ptr (just expose output_string as word)
### comments (just parens should be fine)
### re-update baked-in words at the bottom of this file
###
### TODO remember to ensure that ra is consitently saved to return stack and not main stack

### Macros for text stuff

### code aware of the input stream, ie terminal / source code

        .bss

        ## lines are null terminated, and cannot occur mid-word
line_dry_dock:                  #where you build a new line
        .skip 4096, 0

line_dd_offset:                 #where are we in the line we are building
        .skip 8, 0

line_buffer:                    #the line being parsed
        .skip 4096, 0

line_offset:                    #where are we in the line
        .skip 8, 0

line_ready:                     #is the consumer ready for the commit to happen?
        .skip 8, 0

        .text

        ## does a length check, copies drydock to buffer, and resets cursor
commit_line:
        mv t1, 0
cl_loop:
        lc t2, line_dry_dock, t1
        sc t2, line_buffer, t1
        addi t1, t1, 1
        seqz t2, t2
        bnez t2, cl_done
        lui t2, 1               #4096
        bge t1, t2, panic
        j cl_loop
cl_done:
        la t1, line_offset
        sd x0 (t1)
        ret


        ## Takes a pointer to a null terminated, space seperated array
        ## of characters, and places the first word into the global
        ## word buffer. A word is anything seperated by whitespace or
        ## a null
        ##
        ## a0 is said pointer. It is incremented to point to the first
        ## unconsumed character, after a single null or space
read_word_into_buffer:
        mv t1, x0
        add t2, t1, a0
rwib_loop:
        lb t3, t2
        sb t3, word_buffer, t1
        addi t1, t1, 1
        li t3, 255
        bge t1, t3, panic
        add t2, t1, a0
        beqz t3, rwib_done           #null
        li t4, 0x20
        subi t3, t3, t4
        beqz t3, rwib_done
        j rwib_loop
rwib_done:
        subi t2, t1, 1          #cursor over last char
        sb x0, word_buffer, t2
        ## normalize to null terminated
        add a0, t1, a0
        ret

        ## places a new word in the word buffer from the line buffer,
        ## and waits if one is not available
get_word_safe:
        la t1, line_offset
        ld t2, (t1)
        la t1, line_buffer
gws_zap_space_loop:
        addi t3, t1, t2         #addr in line with offset
        lc t3, (t3)
        li t4, 0x20
        subi t3, t3, t4
        bnez t3, gws_word_or_null
        addi t2, t2, 1
        j gws_zap_space_loop
gws_word_or_null:
        la t3, line_offset
        sd t2, (t3)      # writeback the offset since we might have zapped spaces
        add t3, t1, t2
        lc t3, (t3)
        seqz t3, t3
        bnez t3, gws_wait           #we are at the end of the line.
        ## we must have found a word
        mv t6, ra               #not safe, but I know rwib doesn't use it
        add a0, t1, t2
        call read_word_into_buffer
        ## a0 now has new ptr
        mv ra, t6
        subi t2, t3, t1         #regain offset
        la t3, line_offset
        sd t2, (t3)             #update offset
        ret
gws_wait:                       # we have to wait for a commit
        la t3, line_ready
        li t4, 1
        sd t4, (t3)             # mark ready
gws_wait_loop:
        ## t2 is the offset we came in with
        la t3, line_offset
        ld t3, (t3)
        sub t3, t2, t3          #non-zero if there has been a change
        bnez t3, get_word_safe  #try again
        ## keep waiting, interupt that commits can save us
        j gws_wait_loop

        ## whatever our input is, we just got a new character. Gets the new character in a0
input_character:
        subi sp, sp, 32
        sd s1, (sp), 24
        sd s2, (sp), 16
        sd s3, (sp), 8
        sd s4, (sp)
        ## this is likely in an interupt, so we need some room to safely breathe
        li s1, 0x0A             #newline
        sub s2, a0, s1
        beqz ic_commit
        li s1, 0x0C             #carriage return
        sub s2, a0, s1
        beqz ic_commit
        li s1, 0x03             #end of text
        sub s2, a0, s1
        beqz ic_commit
        li s1, 0x04             #end of transmission
        sub s2, a0, s1
        beqz ic_commit
        li s1, 0x08             #backspace
        sub s2, a0, s1
        beqz ic_backspace
        la s1, line_dd_offset   #test and prevent overwrite. Still allow commit and backspace
        ld s1, (s1)
        li s2, 4095\
        bge s1, s2, ic_done
        ## *ALL* others treated literally. There are lots of other controls, but we pretend not to see them
        la s1, line_dry_dock
        la s2, line_dd_offset
        ld s3, (s2)
        add s4, s1, s3
        sc a0, (s4)             #write char
        addi s3, s3, 1
        ld s3, (s2)             #update offset
        j ic_done
ic_commit:
        ## we finished a line, we can commit if the reader is ready, otherwise just ignore
        la s1, line_ready
        ld s2, (s1)
        beqz s2, ic_done        #not ready
        ## we are ready
        mov t6, ra              #not safe, but I know commit_line doesn't use it
        call commit_line
        mov ra, t6
        j ic_done
ic_backspace:
        ## remove a character, but don't go past the beginning
        la s2, line_dd_offset
        ld s3, (s2)
        beqz s3, ic_done        #don't go past begininning
        subi s3, s3, 1
        sd s3, (s2)             #decrement offset
ic_done:
        addi sp, sp, 32
        ret

### TODO you can expect `output_char` and `output_string`, but you
### should check their calling convention.
###
### in general output_char should take char in a1, and output_string
### an addr of a null-terminated string in a0

### words that need to do input stream stuff

        ## returns the current word_buffer length in a0
current_word_length:
        la t1, word_buffer
cwl_loop:
        lc t2, (t1)
        beqz t2, cwl_done
        addi t1, t1, 1
        j cwl_loop
cwl_done:
        la t2, word_buffer
        sub a0, t1, t2
        addi a0, a0, 1          #space for null
        ret

create_l:                       # ( -- ), reads name from input,
                                # creates a definition for it, gives
                                # some basic default semantics.
        subi sp, sp, 8
        subi fp, fp, 8
        sd ra, (fp)
        call get_word_safe
        sd a0, (sp)
        call current_word_length
        ld ra, (fp)
        addi fp, fp, 8
        mv t4, t0               #where the name starts
        addi t5, t0, a0         #allocation for name, t5 is def addr
        addi t0, t5, 32         #allocation for entry, with empty data
                                #field at the top of the data stack
        ld a0, (sp)
        addi sp, sp, 8
        la t2, word_buffer
        mv t1, t4
cr_loop:
        lc t3, (t2)
        sc t3, (t1)             #copy to data sec for name
        beqz t3, cr_name_done   #hit null
        addi t2, t2, 1
        addi t1, t1, 1
        j cr_loop
cr_name_done:
        ## name is copied, populate fields
        ld t4, (t5)             #name ptr field
        la t2, variable_addr_l
        ld t2, (t5), 8          #code pushes data ptr to stack
        la t2, self_insert_l
        ld t2, (t5), 16         #comp time, just appends self to def
        ld gp, (t5), 24         #dict next link
        ## mv gp, t5               #add to dict, do this in semicolon
        ret


### TODO figure out what the deal is with DOES>, and write it out
### here. I know what is it supposed to do, but not sure how to encode
### that info. I think I need to pass more in from colon possibly.



colon_def_runtime_l:                #runtime sem. for colon def'd words
        subi fp, fp, 32
        sd s1, (fp)
        sd s2, (fp), 8
        sd ra, (fp), 16
        sd tp, (fp), 24
        ld s1, (tp)
        addi s1, s1, 32
cdr_loop:
        ld s2, (s1)
        mv tp, s1
        jalr ra, s2             #jump into compiled ptrlist, leave path back with ra
        addi s1, tp, 8          #careful to use tp here, so callees can alter order (see literal)
        j cdr_loop
        ## we rely on the final word in the def to get us out

colon_l:                        #rs for the colon word itself
        subi fp, fp, 8
        sd ra, (fp)
        call create_l
        ld ra, (fp)
        addi fp, fp, 8
        subi t1, t0, 32         #top of the most recent, unlinked def
        addi t1, t1, 8
        la t2, colon_def_runtime_l
        sd t2, (t1)             #overwrite runtime sem
        la t1, mode
        li t2, 1
        sd t2 (t1)              #enable compile mode
        ret

semicolon_ct_l:                 #cs for semicolon, undo colon_def_runtime_l
        la t1, mode
        sd x0, (t1)             #back to runtime mode
        ld s1, (fp)
        ld s2, (fp), 8
        ld ra, (fp), 16 # clobber ra to stack unwind out of the compile loop
        ld tp, (fp), 24
        addi fp, fp, 32
        ret

### central interpretation and execution stuff

## takes two null terminated string pointers in a1, a2, puts 1 in a1
## if they match, zero otherwise. Nonstandard calling convention to
## mesh with find. Clobbers both inputs.
cmpstr:
        lc t1, (a1)
        lc t2, (a2)
        beqz t1, cmpstr_end
        beqz t2, cmpstr_end
        bneq t1, t2, cmpstr_fail
        addi a1, a1, 1
        addi a2, a2, 1
        j cmpstr
cmpstr_end:
        bneq t1, t2, cmpstr_fail #both zero
        li a1, 1
        ret
cmpstr_fail:
        mv a1, x0
        ret

## a0 is a pointer to a null terminated string. Returns the top of the
## def in a0, or zero if there is none
find_by_string:
        subi sp, sp, 16
        sd ra, (sp)
        sd s1, (sp), 8
        mv s1, gp
fbs_loop:
        mv a1, a0
        ld a2, (s1)
        call cmpstr
        bnez a1, fbs_found
        ld s1, (s1), 24
        beqz s1, fbs_found      #we return zero anyway
        j fbs_loop
fbs_found:
        mv a0, s1
        ld ra, (sp)
        ld s1, (sp), 8
        addi sp, sp, 16
        ret

##for the find word. returns the input str and zero on fail, or head
##of def and 1 on success
find_l:
        subi sp, sp, 16
        sd ra, (sp), 8
        mv a0, sp
        call find_by_string
        ld ra, (sp)
        addi sp, sp, 8
        beqz a0, find_fail
        sd a0, (sp), 8
        li t1, 1
        sd t1, (sp)
        ret
find_fail:
        sd x0, (sp)
        ret

## pop an xt and execute its runtime semantics.
execute_l:
        ld t1, (sp)
        addi sp, sp, 8
        subi fp, fp, 16
        sd ra, (fp)
        sd tp, (fp)
        addi tp, sp, 8          #tp points to xt on the stack: the "body" that did this call
        addi t1, t1, 8
        jalr ra, t1
        ld ra, (fp)
        ld tp, (fp)
        addi fp, fp, 16
        ret

        ## included in compilation by interpret when you give a literal number
literal_rt_l:
        ## examine tp to get the next word baked into defintion, push it, then increment tp
        addi tp, tp, 8          #next word, compiled value addr
        subi sp, sp, 8
        ld t1, (tp)             #baked in literal
        sd t1, (sp)
        ret                     #interal tp inc here skips the literal in colon def loop

        ## is the current word in the buffer a number? 1 in a0 if so, 0 else
is_number:
        li a0, 1
        la t1, word_buffer
        lc t2, (t1)
        li t3, 0x2D             #minus
        bneq t2, t3, is_num_loop
        ## minus, we want to skip that one
        addi t1, t1, 1
is_num_loop:
        lc t2, (t1)
        beqz t2, is_num_done
        li t3, 0x30             #low bound ascii digit
        blt t2, t3, is_num_fail
        li t3, 0x39             #high bound ascii digit
        bgt t2, t3, is_num_fail
        addi t1, t1, 1
        j is_num_loop
is_num_fail:
        mv a0, x0
is_num_done:
        ret

        ## NT digit only string ptr in a0, return value in
        ## a0. Optional leading minus for negative numbers. No
        ## overflow or is_number checking
string_to_num:
        lc t6, (a0)
        subi t6, t6, 0x2D       #minus
        seqz t6, t6             #sign bit
        beqz t6, stn_loop
        addi a0, a0, 1          #consume sign
stn_loop:
        ## use t5 as accumulator
        lc t1, (a0)
        beqz t1, stn_apply_sign
        subi t1, t1, 0x30       #has digit val in binary
        mv t2, t5               #copy
        sll t5, t2, 3
        add t5, t5, t2
        add t5, t5, t2          #(self * 8) + self + self = self * 10
        addi t5, t5, t1         #add new lowest digit
        addi a0, a0, 1
        j stn_loop
stn_apply_sign:
        beqz t6, stn_done
        neg t5, t5
stn_done:
        mv a0, t5
        ret

        ## output a number given in a0 as a string, clobbers a1
        ##
        ## we do some stack stuff to reverse the digit order, since
        ## it's nicer to break down LSD first, but we need to print
        ## MSD first
        ##
        ## this is wasteful in main stack, but I want things to be word aligned
output_num:
        subi fp, fp, 24
        sd ra, (fp)
        sd s1, (fp), 8
        sd s2, (fp), 16
        li s1, 10
        mv s2, sp               #save original stack location so we know when to stop
        bgez a0, on_loop
        ## negative val, emit minus and neg val
        li a1, 0x2D
        call output_char
        neg a0, a0
on_loop:                        #safely positive, don't worry about signs
        rem a1, a0, s1          #mod bottom digit
        addi a1, a1, 0x30       #binary val to ascii val
        subi sp, sp, 8
        sd a1, (sp)
        div a1, a1, s1
        beqz a1, on_pre_print
        j on_loop
on_pre_print:
        ## the main stack from s2 down to sp has each ascii digit in a word, print and pop
        mv s1, s2
        subi s1, s1, 8
on_print_loop:
        ld a1, (s1)
        call output_char
        subi s1, s1, 8
        blt s1, sp on_done
        j on_print_loop
on_done:
        mv sp, s2               #pop all at once
        ld ra, (fp)
        ld s1, (fp), 8
        ld s2, (fp), 16
        addi fp, fp, 24
        ret


## This is the main loop of the interpreter. It doesn't have a natural
## exit, but it pushes a return address to the return stack so it can
## be stack unrolled out of
interpret_entry:
        subi fp, fp, 8
        sd ra, (fp)
int_loop:
        call get_word_safe
        ## there is a word in the buffer now, no matter what it is
        call is_number
        bnez a0, int_handle_num
        la a0, word_buffer
        call find_by_string
        beqz a0, panic
        la t1, mode
        ld t1, (t1)
        bnez int_comptime
        ## int_runtime #TODO where should tp point during top-level runtime?
        addi t2, a0, 8
        jalr ra, t2
        la a0, succ_text
        call output_string
        j int_loop
int_comptime:
        addi t2, a0, 16
        jalr ra, t2
        j int_loop
int_handle_num:
        la a0, word_buffer
        call string_to_num
        ## a0 has val now
        la t1, mode
        ld t1, (t1)
        bnez int_num_comptime
        ## int_num_runtime
        subi sp, sp, 8
        sd a0, (sp)             #push val to stack
        j int_loop
int_num_comptime:
        ## this basically says that the CT semantics of numbers is to
        ## push literal_ct_l and then its value to the body of the
        ## current definition.
        la t1, literal_rt_l
        sd t1, (t0)
        sd a0, (t0), 8
        addi t0, t0, 16
        j int_loop

### baked initial definitions, These need to be the final thing in the data section

        .data
        .set data_stack_next_byte, . #what to initalize the top of the data stack to

        .set last_dict_entry, 0 #bottom points to null
        .set current_dict_entry, 0

        ## use like 'bake_define "ADD", add_label, self_insert_l'
        ## resets the section to text on use
        .macro bake_define name, rcode, ccode
        .rodata
        .set dict_name_l, .
        .asciz name
        .data
        .set current_dict_entry, .
        .int dict_name_l
        .int rcode
        .int ccode
        .int last_dict_entry
        .set last_dict_entry, current_dict_entry
        .set data_stack_next_byte, .
        .text
        .endm

        bake_define "+", add_l, self_insert_l
        bake_define "-", sub_l, self_insert_l
        bake_define "*", mul_l, self_insert_l
        bake_define "*h", mulh_l, self_insert_l
        bake_define "/", div_l, self_insert_l
        bake_define "mod", rem_l, self_insert_l
        bake_define "/r", divr_l, self_insert_l

        bake_define "drop", drop_l, self_insert_l
        bake_define "drop2", drop2_l, self_insert_l
        bake_define "dup", dup_l, self_insert_l
        bake_define "swap", swap_l, self_insert_l
        bake_define "rot", rot_l, self_insert_l
        bake_define "nip", nip_l, self_insert_l
        bake_define "tuck", tuck_l, self_insert_l

        bake_define "r>", mov_from_ret_l, self_insert_l
        bake_define ">r", mov_to_ret_l, self_insert_l
        bake_define "r@", copy_from_ret_l, self_insert_l

        bake_define "!", write_l, self_insert_l
        bake_define "@", read_l, self_insert_l

        bake_define "allot", alloc_l, self_insert_l
        bake_define ",", comma_l, self_insert_l

        bake_define "CREATE", create_l, self_insert_l
        bake_define ":", colon_l, panic
        bake_define ";", panic, semicolon_ct_l
