module X0BuildInterferences

    open X0Lang
    open ALGraph

    let uncoverLive prg =
        ([], prg)


    let includeArg arg liveArea =
        match arg with
        | X0Int (v) -> liveArea
        | X0Reg (reg) -> Set.add (X0RR(reg)) liveArea
        | X0Deref (reg, offs) -> Set.add (X0RM(reg, offs)) liveArea // 
        | X0Var (vname) -> Set.add (X0RV(vname)) liveArea //

    let includeCell cell liveArea =
        match cell with
        | X0TReg (reg) -> Set.add (X0RR(reg)) liveArea
        | X0TDeref (reg, offs) -> Set.add (X0RM(reg, offs)) liveArea
        | X0TVar (vname) -> Set.add (X0RV(vname)) liveArea

    let cellToRef cell =
        match cell with
        | X0TReg (reg) -> X0RR(reg)
        | X0TDeref (reg, offs) -> X0RM(reg, offs)
        | X0TVar (vname) -> X0RV(vname)

    let excludeRef ref liveArea =
        Set.remove ref liveArea

    let x0stmtXRef stmt liveArea =
        match stmt with 
        | MovQ(arg, cell) ->
            liveArea|> excludeRef (cellToRef cell) |> includeArg arg
        | AddQ(arg, cell) -> liveArea|> includeArg arg
        | SubQ(arg, cell) -> liveArea|> includeArg arg
        | MulQ(arg, cell) -> liveArea|> includeArg arg
        | DivQ(arg, cell) -> liveArea|> includeArg arg
        | NegQ(cell) -> liveArea
        | PushQ(arg) -> liveArea|> includeArg arg
        | PopQ(cell) -> liveArea|> excludeRef (cellToRef cell)
        | _ -> liveArea

    let x0crossRef liveArea stmts  =
        let stSpill stmt la  =
            let lx = x0stmtXRef stmt la
            ((stmt, la), lx)
        List.mapFoldBack stSpill stmts liveArea
 
    let x0interference stmt liveArea aGraph = 
        let addInterRef rS rD g rV = 
                if (rV <> rS) && (rV <> rD) then 
                    addEdge rD rV g
                else 
                    g
        let addC cell g rV =
            let refC = (cellToRef cell)
            addInterRef refC refC g rV

        let addAC arg cell g rV = 
            match arg with
            | X0Int(_) -> //WARNING!! Should be used only cell reference???
                addC cell g rV
            | X0Reg (reg) -> addInterRef (X0RR(reg)) (cellToRef cell) g rV
            | X0Deref (reg, offs) -> addInterRef (X0RM(reg, offs)) (cellToRef cell) g rV 
            | X0Var (vname) -> addInterRef (X0RV(vname)) (cellToRef cell) g rV
        
        match stmt with 
        | MovQ(arg, cell) ->            
            Set.fold (addAC arg cell) aGraph liveArea            
        | AddQ(arg, cell)
        | SubQ(arg, cell)
        | MulQ(arg, cell)
        | DivQ(arg, cell) -> 
            Set.fold (addC cell) aGraph liveArea
        | NegQ(cell) -> 
            Set.fold (addC cell) aGraph liveArea
        | PushQ(arg) -> 
            // WARNING! May be not right, to be clarify
            aGraph
        | PopQ(cell) -> 
            // WARNING! May be not right, to be clarify
            Set.fold (addC cell) aGraph liveArea
        | CallQ(fname) -> 
            // FIXME: the reference registor is HARDCODED! The mapping function must be used.
            Set.fold (fun g rV -> addEdge (X0RR Rax) rV g) aGraph liveArea
        | _ -> aGraph

    let x0makeInterGraph agStmts =
        let refType = X0RR(Rax)
        let aGraph = makeGraph [refType] |> removeVertice refType
        List.fold (fun g x -> x0interference (fst x) (snd x) g) aGraph agStmts
    

    let buildInterferences prg = 
        match prg with
            | X0ProgramAbs(vars, stmts) -> 
                x0crossRef (Set.empty) stmts |> fst |> x0makeInterGraph
            | X0ProgramImp(alloc, stmts) ->
                x0crossRef (Set.empty) stmts |> fst |> x0makeInterGraph