module X0Patch

    open X0Lang

    let x0patchMovQ rreg stmt =
        match stmt with
        | MovQ(arg, cell) ->
            match (arg, cell) with
            | X0Var(_), X0TVar(_)
            | X0Deref(_), X0TDeref(_) -> [MovQ(arg, X0TReg rreg); MovQ(X0Reg rreg, cell)] 
            | _ -> [stmt]
        | _ -> [stmt]

    let x0patchAddQ rreg stmt =
        let testMemOp arg cell = 
            match (arg, cell) with
            | X0Var(_), X0TVar(_)
            | X0Deref(_), X0TDeref(_) -> true 
            | _ -> false
        match stmt with
        | AddQ(arg, cell) ->
            if  (testMemOp arg cell) then [MovQ(arg, X0TReg rreg); AddQ(X0Reg rreg, cell)] 
            else [stmt]
        | SubQ(arg, cell) ->
            if  (testMemOp arg cell) then [MovQ(arg, X0TReg rreg); SubQ(X0Reg rreg, cell)] 
            else [stmt]
        | _ -> [stmt]

    let x0patch patcher stmts =
        List.collect (patcher Rax) stmts

    let patching prg =
        match prg with
        | X0ProgramAbs (vars, stmts) ->
            X0ProgramAbs (vars, x0patch x0patchMovQ stmts |> x0patch x0patchAddQ)
        | X0ProgramImp (vars, stmts) ->            
            X0ProgramImp (vars, x0patch x0patchMovQ stmts |> x0patch x0patchAddQ) 
        