module R1Uniquify
    open R1Lang

    exception ItemNotFound of string

    exception ItemAlreadyExists of string

    let lookup name env =
        let pred x = (fst x) = name
        if List.exists pred env then
            let _, item = List.find pred env
            item
        else
            raise (ItemNotFound(name))

    let rec r1uniquify exp map =
        match exp with
        | EInt v -> (exp, map)
        | Read -> (exp, map)
        | Var vname ->
            let xname = lookup vname (snd map)
            (Var(xname), map)
        | Unary (op, e1) -> 
            let ne1, nmap = r1uniquify e1 map               
            (Unary(op, ne1), nmap)
        | Binary (op, e1, e2) -> 
            let r1, nMap = r1uniquify e1 map
            let r2, nMap2 = r1uniquify e2 nMap
            (Binary(op, r1, r2), nMap2)
        | Let (vname, expInit, expBody) ->
            let nExpInit, nMap = r1uniquify expInit map
            let index = (fst map) + 1
            let xname = "t." + index.ToString()
            let nMap  = (index, (vname, xname)::(snd map))
            let nExpBody, nMap2 = r1uniquify expBody nMap
            (Let(xname, nExpInit, nExpBody), nMap2)
        
    let uniquify prg =
        match prg with 
        | Program exp -> 
            let nExp, vMap = r1uniquify exp (0, [])
            (Program nExp, vMap)
