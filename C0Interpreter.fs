module C0Interpreter
    open C0Lang

    exception ItemNotFound of string

    exception ItemAlreadyExists of string

    let lookup name env =
        let pred x = (fst x) = name
        if List.exists pred env then
            let _, item = List.find pred env
            item
        else
            raise (ItemNotFound(name))

    let argValue arg env =
            match arg with
            | C0Int v -> v
            | C0Var vname -> lookup vname env

    let c0ExpInterp exp env inputs =
        match exp with
        | C0Arg arg -> 
            ((argValue arg env), inputs)
        | C0Read -> 
            (List.head inputs, List.tail inputs)
        | C0Minus (arg)->         
            (- (argValue arg env), inputs)
        | C0Add (e1, e2) -> 
            let v1=argValue e1 env
            let v2=argValue e2 env
            (v1 + v2, inputs)    
        | C0Sub (e1, e2) -> 
            let v1=argValue e1 env
            let v2=argValue e2 env
            (v1 - v2, inputs)    
        | C0Mul (e1, e2) -> 
            let v1=argValue e1 env
            let v2=argValue e2 env
            (v1 * v2, inputs)    
        | C0Div (e1, e2) -> 
            let v1=argValue e1 env
            let v2=argValue e2 env
            (v1 / v2, inputs)        
    
    let c0StmtInterp stmt env inputs =
        match stmt with
        | C0Assign (vname, exp) -> 
            let iValue, nInputs = c0ExpInterp exp env inputs             
            let varIdx = List.tryFindIndex (fun x -> (fst x) = vname) env
            let nEnv = 
                match varIdx with
                | Some (id) -> 
                    (vname, iValue)::(List.removeAt id env)
                | None -> raise (ItemNotFound(vname))
            (true, iValue, nEnv, nInputs)
        | C0Return (exp) -> (false, (argValue exp env), env, inputs)

    let c0MakeEnv vars =
        List.map (fun name -> (name, 0)) vars

    let c0Interpreter prg inputs =
        match prg with 
        | C0Program (vars, stmts) -> 
            let env = c0MakeEnv vars
            let stmtFolder ctx stmt = 
                let doNext, value, env, inputs = ctx
                if doNext then
                    c0StmtInterp stmt env inputs
                else
                    ctx
            let _, value, env, res = List.fold stmtFolder (true, 0, env, inputs) stmts
            (value, res)
