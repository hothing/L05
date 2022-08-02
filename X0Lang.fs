module X0Lang
    // Grammar:
    // register ::= rsp | rbp | rax | rbx | rcx | rdx | rsi | rdi | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
    // arg ::= (int int) | (reg register) | (deref register int)
    // instr ::= (addq arg arg) | (subq arg arg) | (movq arg arg) | (retq) | (negq arg) | (callq label) | (pushq arg) | (popq arg)
    // x86_0 ::= (program int instr+)

    type X0Register = Rsp | Rbp | Rax | Rbx | Rcx | Rdx | Rsi | Rdi | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
    type X0Arg = X0Arg of int | X0ArgReg of X0Register | X0ArgDeref of X0Register * int
    type X0Instr = AddQ of X0Arg * X0Arg 
                 | SubQ of X0Arg * X0Arg 
                 | MovQ of X0Arg * X0Arg 
                 | RetQ 
                 | NegQ of X0Arg
                 | CallQ of string
                 | PushQ of X0Arg
                 | PopQ of X0Arg

    