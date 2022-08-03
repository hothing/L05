module R1Flatten

    open R1Lang
    open C0Lang

    let rec r1flatten exp vars =
        match exp with
        | EInt v -> 
            let index = (fst vars) + 1
            let tname = "m." + index.ToString()
            let nVars = (index, tname::(snd vars))
            ()
        | Read -> 
            ()
        | Var vname ->
            let index = (fst vars) + 1
            let nVars = (index, vname::(snd vars))
            ()
            
        | Unary (op, e1) -> 
            ()
        | Binary (op, e1, e2) -> 
            (*
            let r1, nInputs = r1flatten e1 vars
            let r2, nInputs2 = r1flatten e2 vars
            *)
            match op with
            | Add ->
                ()
            | Sub ->
                ()
            | Mul ->
                ()
            | Div ->
                ()
        | Let (vname, expInit, expBody) ->
            (*
            let iValue, nInputs = r1interp expInit env inputs 
            let nEnv = (vname, iValue)::env
            *)
            r1flatten expBody (0, [])