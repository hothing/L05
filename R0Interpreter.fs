module R0Interpreter
    open R0Lang

    let rec interp1 exp inputs =
        match exp with
        | EInt v -> (v, inputs)
        | Read -> 
            (List.head inputs, List.tail inputs)
        | Unary (op, e1) -> 
            let res, next_inp = interp1 e1 inputs               
            match op with
            | Plus -> 
                (res, next_inp)
            | Minus -> 
                (0 - res, next_inp)
        | Binary (op, e1, e2) -> 
            let r1, next_inp = interp1 e1 inputs
            let r2, next_inp2 = interp1 e2 next_inp
            match op with
            | Add ->
                (r1 + r2, next_inp2)
            | Sub ->
                (r1 - r2, next_inp2)
            | Mul ->
                (r1 * r2, next_inp2)
            | Div ->
                (r1 / r2, next_inp2)
        
    let interpreter prg inputs =
        match prg with 
        | Program exp -> interp1 exp inputs
