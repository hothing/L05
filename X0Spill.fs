module X0Spill

    open X0Lang

    (*
        and X0Arg = X0Int of int | X0Reg of X0Register | X0Deref of X0Register * int | X0Var of X0Variable
        and X0Cell = X0TReg of X0Register | X0TVar of X0Variable | X0TDeref of X0Register * int
    *)

    type X0Reference = X0RR of X0Register | X0RD of X0Register * int | X0RV of X0Variable | X0RNone

    let uncoverLive prg =
        ([], prg)


    let argToRef arg =
        match arg with
        | X0Int (v) -> X0RNone
        | X0Reg (reg) -> X0RR(reg)
        | X0Deref (reg, offs) -> X0RD(reg, offs) 
        | X0Var (vname) -> X0RV(vname)

    let cellToRef cell =
        match cell with
        | X0TReg (reg) -> X0RR(reg)
        | X0TDeref (reg, offs) -> X0RD(reg, offs) 
        | X0TVar (vname) -> X0RV(vname)

    let x0stmtSpill stmt liveArea =
        let includeArg arg liveArea =
            if arg <> X0RNone then Set.add arg liveArea else liveArea
        let excludeCell cell liveArea =
            if cell <> X0RNone then Set.remove cell liveArea else liveArea
        match stmt with 
        | MovQ(arg, cell) ->
            liveArea|> excludeCell (cellToRef cell) |> includeArg (argToRef arg)
        | AddQ(arg, cell) -> liveArea|> includeArg (argToRef arg)
        | SubQ(arg, cell) -> liveArea|> includeArg (argToRef arg)
        | MulQ(arg, cell) -> liveArea|> includeArg (argToRef arg)
        | DivQ(arg, cell) -> liveArea|> includeArg (argToRef arg)
        | NegQ(cell) -> liveArea
        | PushQ(arg) -> liveArea|> includeArg (argToRef arg)
        | PopQ(cell) -> liveArea|> excludeCell (cellToRef cell)
        | _ -> liveArea

    let x0spilling liveArea stmts  =
        let stSpill stmt la  =
            let lx = x0stmtSpill stmt la
            ((stmt, la), lx)
        List.mapFoldBack stSpill stmts liveArea