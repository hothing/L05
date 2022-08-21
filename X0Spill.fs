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
        match stmt with 
        | MovQ(arg, cell) ->            
            Set.add (argToRef arg) (Set.remove (cellToRef cell) liveArea)
        | AddQ(arg, cell) -> Set.add (argToRef arg) liveArea
        | SubQ(arg, cell) -> Set.add (argToRef arg) liveArea
        | MulQ(arg, cell) -> Set.add (argToRef arg) liveArea
        | DivQ(arg, cell) -> Set.add (argToRef arg) liveArea
        | NegQ(cell) -> liveArea
        | PushQ(arg) -> Set.add (argToRef arg) liveArea
        | PopQ(cell) -> Set.remove (cellToRef cell) liveArea
        | _ -> liveArea

    let x0spilling liveArea stmts  =
        let stSpill stmt la  =
            let lx = x0stmtSpill stmt la
            ((stmt, la), lx)
        List.mapFoldBack stSpill stmts liveArea