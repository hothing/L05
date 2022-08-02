module R0PartialEval
    open R0Lang
    
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

    let rec foldAdd exp =
        printfn "<foldAdd> %A" exp
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
                    //printfn "<foldAdd:BOP> const"
                    match op with
                    | Add -> EInt(v1 + v2)
                    | Sub -> EInt(v1 - v2)
                    | _ -> exp
                | Binary (subOp, subE1, subE2) ->
                    //printfn "<foldAdd:BOP> sub on %A with %A" subE1 subE2
                    match subE1 with
                    | EInt r1 ->
                        match (op, subOp) with
                        | (Add, Add) -> foldAdd (Binary (Add, EInt (v1 + r1), subE2))
                        | (Add, Sub) -> foldAdd (Binary (Sub, EInt (v1 + r1), subE2))
                        | (Sub, Sub) -> foldAdd (Binary (Add, EInt (v1 - r1), subE2))
                        | (Sub, Add) -> foldAdd (Binary (Sub, EInt (v1 - r1), subE2))
                        | (_,_) -> exp                        
                    | _ -> exp                
                | _ -> 
                    //printfn "<foldAdd:BOP> default"
                    Binary(op, foldAdd e1, foldAdd e2)
            | (_, _) -> Binary(op, foldAdd e1, foldAdd e2)

    let rec foldMul exp =
        printfn "<foldMul> %A" exp
        match exp with
        | Unary (op, e1) -> 
            match op with
            | Plus -> foldMul e1
            | Minus -> Unary (Minus, foldMul e1)
        | Binary (op, e1, e2) -> 
            printfn "<foldMul:BOP> %A with %A" e1 e2 
            match (e1, e2) with
            | (EInt v1, _) ->                
                match e2 with
                | Binary (subOp, subE1, subE2) ->
                    printfn "<foldMul:BOP> sub on %A with %A" subE1 subE2
                    match subE1 with
                    | EInt r1 ->
                        match (op, subOp) with
                        | (Mul, Mul) -> 
                            printfn "<foldMul:BOP:Mul>"
                            foldMul (Binary (Mul, EInt (v1 * r1), subE2))
                        | (_,_) -> 
                            printfn "<foldMul:BOP:default>"
                            Binary (op, e1, foldMul e2)
                    | _ -> exp
                | _ -> 
                    exp
            | (_, _) -> Binary(op, foldMul e1, foldMul e2)
        | _ -> exp

    let partialEvaluator prg =
        match prg with 
        | Program exp -> Program(expSort exp |> peval |> foldAdd)
    
