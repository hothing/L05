module X0Homes
    open X0Lang

    exception ItemNotFound of string

    exception ItemAlreadyExists of string

    let lookup name env =
        let pred x = (fst x) = name
        if List.exists pred env then
            let _, item = List.find pred env
            Some(item)
        else
            None

    let rec genHomes mapper baseOffset nVars vars =
        match vars with
        | v::rest -> 
            let dr = mapper baseOffset v
            let nOffset, nR = dr
            genHomes mapper nOffset ((v, dr)::nVars) rest
        | [] -> (baseOffset, nVars)

    let x0assignHomes baseReg baseOffset vars stmts =
        let endOffset, nVars = genHomes (fun offset v -> (offset + 8, baseReg)) baseOffset [] vars
        let transArg arg vars =
            match arg with
            | X0Var(name) -> 
                match lookup name vars with
                | Some(offs, reg) -> X0Deref(reg, offs)
                | None -> raise (ItemNotFound(name))
            | _ -> arg

        let transCell cell vars =
            match cell with
            | X0TVar(name) -> 
                match lookup name vars with
                | Some(offs, reg) -> X0TDeref(reg, offs)
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

        (endOffset - baseOffset, List.map translate stmts)

    let assignHomes baseReg baseOffset x0prg =
        match x0prg with
        | X0Program(vars, stmts) -> x0assignHomes baseReg baseOffset vars stmts