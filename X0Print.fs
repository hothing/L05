module X0Print

    open X0Lang
    
    let x0printReg reg =
        match reg with
        | Rsp -> "rsp"
        | Rbp -> "rbp" 
        | Rax -> "rax" 
        | Rbx -> "rbx" 
        | Rcx -> "rcx" 
        | Rdx -> "rdx" 
        | Rsi -> "rsi" 
        | Rdi -> "rdi" 
        | R8  -> "r8"
        | R9  -> "r9"
        | R10 -> "r10" 
        | R11 -> "r11" 
        | R12 -> "r12" 
        | R13 -> "r13" 
        | R14 -> "r14" 
        | R15 -> "r15"

    let x0printArg arg =
        match arg with
        | X0Int(v) -> $"$%d{v}"
        | X0Reg(reg) -> $"%%{x0printReg reg}"
        | X0Deref(reg, offs) -> $"%d{offs}(%%{x0printReg reg})"
        | X0Var(vname) -> vname

    let x0printCell cell =
        match cell with
        | X0TReg(reg) -> $"%%{x0printReg reg}"
        | X0TDeref(reg, offs) -> $"%d{offs}(%%{x0printReg reg})"
        | X0TVar(vname) -> vname

    let x0printInstr stmt =
        match stmt with
        | MovQ(arg, cell) -> $"movq {x0printArg arg}, {x0printCell cell}"
        | AddQ(arg, cell) -> $"addq {x0printArg arg}, {x0printCell cell}"
        | SubQ(arg, cell) -> $"subq {x0printArg arg}, {x0printCell cell}"
        | MulQ(arg, cell) -> $"mulq {x0printArg arg}, {x0printCell cell}"
        | DivQ(arg, cell) -> $"divq {x0printArg arg}, {x0printCell cell}"
        | NegQ(cell) -> $"negq {x0printCell cell}"
        | PushQ(arg)-> $"pushq {x0printArg arg}"
        | PopQ(cell) -> $"popq {x0printCell cell}"
        | CallQ(fname) -> $"call _{fname}"
        | RetQ -> $"ret"

    let print stmts =
        String.concat "\n" (List.map (fun i -> $"{x0printInstr i}") stmts)