module X0Homes
    open X0Lang

    exception ItemNotFound of string

    exception ItemAlreadyExists of string

    let x0assignHomes nVars stmts =
        let transArg arg vars =
            match arg with
            | X0Var(name) -> 
                match Map.tryFind name vars with
                | Some(x) ->
                    match x with
                    | X0RR (reg) -> X0Reg reg
                    | X0RM (reg, offset) -> X0Deref(reg, offset)
                    | X0RV (vname) -> X0Var vname                    
                | None -> raise (ItemNotFound(name))
            | _ -> arg

        let transCell cell vars =
            match cell with
            | X0TVar(name) -> 
                match Map.tryFind name vars with
                | Some(x) -> 
                    match x with
                    | X0RR (reg) -> X0TReg reg
                    | X0RM (reg, offset) -> X0TDeref(reg, offset)
                    | X0RV (vname) -> X0TVar vname                    
                | None -> raise (ItemNotFound(name))
            | _ -> cell

        let translate stmt =
            match stmt with 
            | MovQ(arg, cell) -> MovQ(transArg arg nVars, transCell cell nVars)
            | AddQ(arg, cell) -> AddQ(transArg arg nVars, transCell cell nVars)
            | SubQ(arg, cell) -> SubQ(transArg arg nVars, transCell cell nVars)
            | MulQ(arg, cell) -> MulQ(transArg arg nVars, transCell cell nVars)
            | DivQ(arg, cell) -> DivQ(transArg arg nVars, transCell cell nVars)
            | NegQ(cell) -> NegQ(transCell cell nVars)
            | PushQ(arg) -> PushQ(transArg arg nVars)
            | PopQ(cell) -> PopQ(transCell cell nVars)
            | _ -> stmt

        List.map translate stmts

    let assignHomes allocator prg =
        match prg with
        | X0ProgramAbs(vars, stmts) ->
            let allocVars = allocator vars
            let nStmts = x0assignHomes allocVars stmts
            X0ProgramImp(allocVars, nStmts)
        | X0ProgramImp (allocVars, stmts) ->
            let nStmts = x0assignHomes allocVars stmts
            X0ProgramImp(allocVars, nStmts)
