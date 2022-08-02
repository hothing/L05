module R0PartialEval
    open R0Lang

    // with grammar: 
    //    exp ::= int | (read) | (- exp) | (+ exp exp)
    //    R0 ::= (program exp)
    
    let hasConst exp =
        match exp with
        | EInt v -> true
        | Read -> false
        | Unary (op, e1) -> 
            match e1 with
            | EInt v -> true
            | _ -> false
        | Binary (op, e1, e2) -> 
            match (e1, e2) with
            | (EInt v1, _) -> true
            | (_, EInt v2) -> true
            | (_, _) -> false
        
    let rec expSort exp =
        match exp with
        | EInt v -> exp
        | Read -> exp
        | Unary (op, e1) -> 
            Unary (op, (expSort e1))            
        | Binary (op, e1, e2) -> 
            let r1 = expSort e1
            let r2 = expSort e2
            match op with
                | Add -> 
                    match (r1, r2) with
                    | (EInt _, EInt _) -> Binary (op, r1, r2)
                    | (_, EInt _) -> Binary (op, r2, r1)
                    | (_, _) -> Binary (op, r1, r2)
                | _ -> Binary (op, r1, r2)
                (*
                | Sub -> 
                    match (r1, r2) with
                    | (EInt _, EInt _) -> Binary (op, r1, r2)
                    | (EInt v, _) -> Binary (Add, r2, EInt -v)
                    | (_, _) -> Binary (op, r1, r2)
                | Mul -> Binary(op, r1, r2)
                | Div -> Binary(op, r1, r2)
                *)

    let rec foldAdd exp =
        //printfn "<foldAdd> %A" exp
        match exp with
        | EInt v -> exp
        | Read -> exp
        | Unary (op, e1) -> 
            match op with
            | Plus -> foldAdd e1
            | Minus -> Unary (Minus, foldAdd e1)
        | Binary (op, e1, e2) -> 
            //printfn "<foldAdd:BOP> %A with %A" e1 e2 
            match (e1, e2) with
            | (EInt v1, _) ->                
                match e2 with
                | EInt v2 ->
                    match op with
                    | Add -> EInt(v1 + v2)
                    | Sub -> EInt(v1 - v2)
                    | _ -> exp
                | Binary (subOp, subE1, subE2) -> 
                    match subE1 with
                    | EInt r1 ->
                        let new_exp = 
                            match (op, subOp) with
                            | (Add, Add) -> Binary (Add, EInt (v1 + r1), subE2)
                            | (Add, Sub) -> Binary (Sub, EInt (v1 + r1), subE2)
                            | (Sub, Sub) -> Binary (Add, EInt (v1 - r1), subE2)
                            | (Sub, Add) -> Binary (Sub, EInt (v1 - r1), subE2)
                            | (_,_) -> exp
                        foldAdd new_exp
                    | _ -> exp                  
                | _ -> exp
            | (_, _) -> Binary(op, foldAdd e1, foldAdd e2)
                
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
        | Program exp -> Program(expSort exp |> foldAdd |> peval)
        
    
    (*
    let sort prg =
        match prg with 
        | Program exp -> Program(expSort exp)

    let foldA prg =
        match prg with 
        | Program exp -> Program(foldAdd exp)
    *)