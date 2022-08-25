module X0Spill

    open X0Lang
    open ALGraph

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

    let includeRef ref liveArea =
        if ref <> X0RNone then Set.add ref liveArea else liveArea
    
    let excludeRef ref liveArea =
        if ref <> X0RNone then Set.remove ref liveArea else liveArea

    let x0stmtSpill stmt liveArea =
        match stmt with 
        | MovQ(arg, cell) ->
            liveArea|> excludeRef (cellToRef cell) |> includeRef (argToRef arg)
        | AddQ(arg, cell) -> liveArea|> includeRef (argToRef arg)
        | SubQ(arg, cell) -> liveArea|> includeRef (argToRef arg)
        | MulQ(arg, cell) -> liveArea|> includeRef (argToRef arg)
        | DivQ(arg, cell) -> liveArea|> includeRef (argToRef arg)
        | NegQ(cell) -> liveArea
        | PushQ(arg) -> liveArea|> includeRef (argToRef arg)
        | PopQ(cell) -> liveArea|> excludeRef (cellToRef cell)
        | _ -> liveArea

    let x0spilling liveArea stmts  =
        let stSpill stmt la  =
            let lx = x0stmtSpill stmt la
            ((stmt, la), lx)
        List.mapFoldBack stSpill stmts liveArea

    let x0interference stmt liveArea aGraph = 
        let addIFR rS rD g rV = 
                if (rV <> rS) && (rV <> rD) then 
                    addEdge rD rV g
                else 
                    g
        match stmt with 
        | MovQ(arg, cell) ->            
            Set.fold (addIFR (argToRef arg) (cellToRef cell)) aGraph liveArea            
        | AddQ(arg, cell)
        | SubQ(arg, cell)
        | MulQ(arg, cell)
        | DivQ(arg, cell) -> 
            let refC = (cellToRef cell)
            Set.fold (addIFR refC refC) aGraph liveArea
        | NegQ(cell) -> 
            let refC = (cellToRef cell)
            Set.fold (addIFR refC refC) aGraph liveArea
        | PushQ(arg) -> 
            // WARNING! May be not right, to be clarify
            aGraph
        | PopQ(cell) -> 
            // WARNING! May be not right, to be clarify
            let refC = (cellToRef cell)
            Set.fold (addIFR refC refC) aGraph liveArea
        | CallQ(fname) -> 
            // FIXME: the reference registor is HARDCODED! The mapping function must be used.
            Set.fold (fun g rV -> addEdge (X0RR Rax) rV g) aGraph liveArea
        | _ -> aGraph

    let x0makeInterGraph agStmts =
        List.fold (fun g x -> x0interference (fst x) (snd x) g) (makeGraph [X0RNone]) agStmts
    
