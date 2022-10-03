module R1Flatten

    open R1Lang
    open C0Lang
    open R1Uniquify

    let rec r1flatten exp assigments vars =
        let genName vars =
            let index = (fst vars) + 1
            let tname = "m." + index.ToString()
            let nVars = (index, tname::(snd vars))
            (tname, nVars)

        let addName vars name =
            (fst vars, name::(snd vars))

        match exp with
        | EInt v -> 
            //printfn "<int> = %A" exp 
            (C0Int(v), assigments, vars) // C0Int
        | Read -> 
            //printfn "<read> = %A" exp 
            let tname, nVars = genName vars
            let nAssigments = (C0Assign(tname, C0Read))::assigments
            (C0Var(tname), nAssigments, nVars)           
        | Var vname ->
            //printfn "<var> = %A" exp 
            (C0Var(vname), assigments, vars) // C0Arg:C0Var            
        | Unary (op, e1) -> 
            //printfn "<unary> = %A" exp 
            let cexp, nAssigments, nVars = r1flatten e1 assigments vars
            let tname, nVars = genName nVars
            let stmt = 
                match op with
                | Plus -> C0Assign(tname, C0Arg(cexp))
                | Minus -> C0Assign(tname, C0Minus(cexp))
            let nAssigments = stmt::nAssigments
            (C0Var(tname), nAssigments, nVars)
        | Binary (op, e1, e2) -> 
            //printfn "<binary> = %A" exp 
            let arg1, nAssigments, nVars = r1flatten e1 assigments vars
            let arg2, nAssigments, nVars = r1flatten e2 nAssigments nVars
            let tname, nVars = genName nVars
            let cexp =  
                match op with
                | Add -> C0Add(arg1, arg2)
                | Sub -> C0Sub(arg1, arg2)
                | Mul -> C0Mul(arg1, arg2)
                | Div -> C0Div(arg1, arg2)
            let nAssigments = (C0Assign(tname, cexp))::nAssigments
            (C0Var(tname), nAssigments, nVars)
        | Let (vname, expInit, expBody) ->
            //printfn "<let> = %A" exp 
            let arg1, nAssigments, nVars = r1flatten expInit assigments vars
            let nAssigments = (C0Assign(vname, C0Arg(arg1)))::nAssigments
            let nVars = addName nVars vname
            let arg2, nAssigments, nVars = r1flatten expBody nAssigments nVars
            let tname, nVars = genName nVars
            let nAssigments = (C0Assign(tname, C0Arg(arg2)))::nAssigments            
            (C0Var(tname), nAssigments, nVars)

    let flattenX prg =
        match prg with 
        | Program exp -> 
            let nExp, vMap = r1uniquify exp (0, [])
            let arg, nAssigments, nVars = r1flatten nExp [] (0, [])
            (C0Program((snd nVars), List.rev ((C0Return(arg))::nAssigments)), vMap)

    let flatten prg =
        match prg with 
        | Program exp -> 
            let nExp, vMap = r1uniquify exp (0, [])
            let arg, nAssigments, nVars = r1flatten nExp [] (0, [])
            C0Program((snd nVars), List.rev ((C0Return(arg))::nAssigments))