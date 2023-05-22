### This is a version of Forth based on itsy forth.
###
### This is for x86_64
###
### It should be compiled with GNU as, which is part of the standard
### GNU toolchain

.intel_syntax
.altmacro




### The dictionary entry format on the data stack is as follows
###
### 64b, ptr to next entry or null for the last entry
### 64b, length of the string name in bytes, excluding trailing null
### 64b, ptr to null terminated string of entry name
### 64b, ptr to compilation semantics (asm label)
### 64b, ptr to runtime semantics (asm label)
### [? bits] contents of the entry (forth entry pointers or other data)

        ## This keeps track of the last entry defined during compilation
        .set link, 0

### This macro should be used only after all the other setup, as every
### piece of data after it will be included as part of the definition
### of the last macro invocation. Also the initial setting of the data
### stack last entry (rbp) must happen after all the invocations of
### this macro, as the comp-time symbol `link` will only reflect the
### final entry after all of them have been defined.
        .macro entry name, compile_time, run_time
        LOCAL string_loc, .
        ## write the string name as a constant right above the header,
        ## and link as normal
        .string name
        LOCAL string_len, . - string_loc - 1
        LOCAL head, .
        ## write the bytes for the header
        .balign 8
        .long link
        .long string_len
        .long string_loc
        .long compile_time
        .long run_time
        .set link, head
        .set name, head         #terrible hack, defines `name` to be a label for the entry
        .endm

### a convience callers for the above. The same rules apply
        .macro primitive name
        entry name, default_compile, primitive_runtime
        .endm

        .macro variable name
        entry name, default_compile, var_runtime
        .endm

### The register layout will be (with stacks growing downwards)
###
### rsp, top of the primary argument stack (last full byte, grows down)
### rax, return stack top (last full byte, grows down)
### r12, Forth Instruction Pointer (currently executing forth word, ptr to ptr to entry)
### rbx, top of data stack (ptr to last full byte, grows up)
### rbp, data stack (head of the last entry in the stack)

.section data
        dq state                # 0 for compiletime execution, 1 for runtime

.section text

        ## find a matching entry for a string
        ##
        ## Args:
        ## rdi, ptr to null terminated test string
        ## rsi, length of string without null
        ##
        ## Returns:
        ## rdx, ptr of head of matching entry or null
        ##
        ## clobbers rcx, r8, r9
find_entry:
        mov rdx, rbp            #head of first entry

test_entry:
        cmp rdx, 0
        jeq find_exit
        ## there is another entry

        cmp rsi, qword ptr [rdx + 8] #cmp length
        jeq char_test
        mov rdx, qword ptr [rdx] #load next reference from top of header
        j test_entry

char_test:
        mov rcx, qword ptr [rdx + 16] #ptr to entry char
        mov r9, rdi

char_loop:
        mov r8b, byte ptr [rcx]
        cmp r8b, byte ptr [r9]
        jeq char_cnt
        ## don't match
        mov rdx, qword ptr [rdx] #load next reference from top of header
        j test_entry

char_cnt:
        cmp 0, byte ptr [rcx]   #on top of null char that matches, this is a match entry
        jeq find_exit           #rdx is alread set for us

        ## otherwise increment and repeat
        add rcx, 8
        add r9, 8
        j char_loop

find_exit:
        ret
        ## TODO is this the right way to return from this call?


        ## default compile time word semantics. This should be pointed
        ## to by most words in the forth dictionary, in their
        ## compile-time spot. As such, it should not be invoked with
        ## `call` and it will return with `j next`, which continues
        ## Forth code flow.
        ##
        ## This pushes the current entry to the end of the data stack
        ## (the end of the entry currently being defined
        ## presumably). This is called by the interpreter when a word
        ## with this compile-time semantics is encountered inside a
        ## colon definition or other compile-state evironment.
        ##
        ## The currently "executing" forth instruction is in r12, so
        ## dereferencing that once should give a ptr to the entry. We
        ## want to push that to the end of the data stack (current
        ## word being defined)
        ##
        ## clobbers rcx
default_compile:
        add rbx, 8
        mov rcx, qword ptr [r12]
        mov qword ptr [rbx], rcx
        j next

        ## default runtime semantics for variables. Pushes the address
        ## of the start of the contents of the entry. It is the
        ## Forth-level programmer's responisbility to ensure there are
        ## no out of bounds errors from incorrect size assumptions
        ## about the size of the entry contents, as that information
        ## is not known at a language level.
        ##
        ## This call exits with `j next` and thus should not be evoked
        ## with `call`. We use the currently executing forth word
        ## (r12) to get the addr, then push it to the main stack.
        ##
        ## clobbers rcx
var_runtime:
        mov rcx, qword ptr [r12]
        add rcx, 40             #offset for contents
        push rcx
        j next

        ## this is the default runtime semantics for primitive
        ## words. A primitive word is one whose (runtime) semantics
        ## are not defined in terms of standard forth structure but
        ## are instead supplied in assembly. A variable word is a
        ## special kind of primitive. All other primitives simply
        ## transfer execution to their contents, which contain literal
        ## machine code for their semantics.
        ##
        ## clobbers rcx
primitive_runtime:
        mov rcx, qword ptr [r12]
        add rcx, 40             #offset for contents
        j [rcx]

        ## a non-primitive, non-variable word likely has runtime
        ## semantics `nest`

        ## enter a non-primitive forth function. Push the forth
        ## instruction ptr to the return stack (without incrementing,
        ## that happens on the other end) and set the forth
        ## instruction ptr to the start of the contents of the current
        ## entry
nest:
        add rax, 8
        mov qword ptr [rax], r12
        mov r12, qword ptr [rcx] #entry ptr
        add r12, 40              #start of contents
        j next

        ## return from a non-primitive forth function. Restore the
        ## forth instruction ptr from the return stack
unnest:
        mov r12, qword ptr [rax]
        sub rax, 8
        j next                  # increment of r12 happens in next

        ## increment the forth instruction pointer and follows the
        ## runtime/compiletime semantics of the encountered word
        ## according to the global `state` variable
        ##
        ## clobbers rcx, r8
next:
        add r12, 8
        mov rcx, qword ptr [r12] # entry ptr
        lea r8, state
        mov r8, qword ptr [r8]  # 0 for comp, 1 for runtime
        add rcx 24
        sll r8, 3               # {0,1} * 8 # this avoids cmp / branch
        add rcx, r8             # -> {cmp,run} semantics
        j [rcx]
