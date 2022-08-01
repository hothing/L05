module R0PartialEval
    open R0Lang

    let rec peval exp =
        match exp with
        | EInt v -> exp
        | Read -> exp
        | Unary (op, e1) -> 
            let ne1 = peval e1
            match ne1 with
            | EInt v ->
                match op with
                | Plus -> ne1
                | Minus -> EInt(0 - v)
            | _ -> 
                match op with
                | Plus -> ne1
                | Minus -> Unary(op, ne1)
        | Binary (op, e1, e2) -> 
            let r1 = peval e1
            let r2 = peval e2
            match (r1, r2) with
            | (EInt v1, EInt v2) ->
                match op with
                | Add -> EInt(v1 + v2)
                | Sub -> EInt(v1 - v2)
                | Mul -> EInt(v1 * v2)
                | Div -> EInt(v1 / v2)
            | (_, _) -> Binary (op, r1, r2)
        
    let partialEvaluator prg =
        match prg with 
        | Program exp -> peval exp
