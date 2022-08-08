module X0Select
    open C0Lang
    open X0Lang

    
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

    let x0genArg arg =
        match arg with
        | C0Int v -> X0Int v
        | C0Var vname -> X0Var vname

    let x0gen tvar exp =
        match exp with
        | C0Add (arg1, arg2) -> 
            match expVarPos exp tvar with
            | VP_None -> [MovQ(x0genArg arg1, X0Var tvar); AddQ(x0genArg arg2, X0Var tvar)]
            | VP_Left -> [AddQ(x0genArg arg2, X0Var tvar)]
            | VP_Right | VP_Both -> [AddQ(x0genArg arg1, X0Var tvar)]
        | C0Sub (arg1, arg2) -> 
            match expVarPos exp tvar with
            | VP_None -> [MovQ(x0genArg arg1, X0Var tvar); SubQ(x0genArg arg2, X0Var tvar)]
            | VP_Left | VP_Both -> [SubQ(x0genArg arg2, X0Var tvar)]
            | VP_Right -> [NegQ(X0Var tvar); AddQ(x0genArg arg1, X0Var tvar)]
        | _ -> []

    let x0SelectInstr codes stmt env =
        match stmt with
        | C0Assign(vname, exp) -> x0gen vname exp
        | C0Return(vname) -> codes // TODO

    let selectInstruction c0prg =
        match c0prg with
        | C0Program (vars, stmt) -> []
