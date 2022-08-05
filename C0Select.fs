module C0Select
    open C0Lang
    open X0Lang

    let c0SelectInstr exp env =
        X0Program([], [])

    (*
        (assign x (+ y z)) ⇒ [(movq (var y) (var x)) ; (addq (var z) (var x))]
        (assign x (+ 10 32)) ⇒ [(movq (int 10) (var x)) ; (addq (int 32) (var x))]
        (assign x (+ y 10)) ⇒ [(movq (var y) (var x)) ; (addq (int 10) (var x))]
        (assign x (+ 10 y)) ⇒ [(movq (int 10) (var x)) ; (addq (var y) (var x))]
        (assign x (+ 10 x)) ⇒ [(addq (int 10) (var x))]
        (assign x (+ x 10)) ⇒ [(addq (int 10) (var x))]
        (assign x (+ x x)) ⇒ [(addq (var x) (var x))]

        (assign x (- y z)) ⇒ [(movq (var y) (var x)) ; (subq (var z) (var x))]
        (assign x (- 10 32)) ⇒ [(movq (int 10) (var x)) ; (subq (int 32) (var x))]
        (assign x (- y 10)) ⇒ [(movq (var y) (var x)) ; (subq (int 10) (var x))]
        (assign x (- 10 y)) ⇒ [(movq (int 10) (var x)) ; (subq (var y) (var x))]
        (assign x (- 10 x)) ⇒ [(addq (int -10) (var x))]
        (assign x (- x 10)) ⇒ [(subq (int 10) (var x))]
        (assign x (- x x)) ⇒ [(subq (var x) (var x))]
    *)

    type VarPosition = VP_None | VP_Left | VP_Right | VP_Both

    let hasArgName arg name =
        match arg with
        | C0Int _ -> false
        | C0Var vname -> name = vname
    
    let hasExpVar exp name =
        match exp with
        | C0Read -> false
        | C0Arg arg
        | C0Minus arg -> hasArgName arg name
        | C0Add (arg1, arg2)
        | C0Sub (arg1, arg2)
        | C0Mul (arg1, arg2)
        | C0Div (arg1, arg2) -> hasArgName arg1 name || hasArgName arg2 name

    let expVarPos exp name =
        match exp with
        | C0Read -> VP_None
        | C0Arg arg 
        | C0Minus arg -> 
            if hasArgName arg name then
                VP_Left
            else
                VP_None
        | C0Add (arg1, arg2) 
        | C0Sub (arg1, arg2) 
        | C0Mul (arg1, arg2) 
        | C0Div (arg1, arg2) -> 
            let e1 = hasArgName arg1 name
            let e2 = hasArgName arg2 name
            match (e1, e2) with
            | (false, false) -> VP_None
            | (false, true) -> VP_Right
            | (true, false) -> VP_Left
            | (true, true) -> VP_Both
        
    let isAsgmtShort stmt =
        match stmt with
        | C0Assign (vname, exp) -> 
            hasExpVar exp vname
        | C0Return (_) -> false


