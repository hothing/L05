module R1Interpreter
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

    let rec r1interp exp env inputs =
        match exp with
        | EInt v -> (v, inputs)
        | Read -> 
            (List.head inputs, List.tail inputs)
        | Var vname ->
            (lookup vname env, inputs)
        | Unary (op, e1) -> 
            let res, nInputs = r1interp e1 env inputs               
            match op with
            | Plus -> 
                (res, nInputs)
            | Minus -> 
                (0 - res, nInputs)
        | Binary (op, e1, e2) -> 
            let r1, nInputs = r1interp e1 env inputs
            let r2, nInputs2 = r1interp e2 env nInputs
            match op with
            | Add ->
                (r1 + r2, nInputs2)
            | Sub ->
                (r1 - r2, nInputs2)
            | Mul ->
                (r1 * r2, nInputs2)
            | Div ->
                (r1 / r2, nInputs2)
        | Let (vname, expInit, expBody) ->
            let iValue, nInputs = r1interp expInit env inputs 
            let nEnv = (vname, iValue)::env
            r1interp expBody nEnv nInputs
        
    let interpreter prg inputs =
        match prg with 
        | Program exp -> r1interp exp [] inputs
