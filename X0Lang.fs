module X0Lang
    
    type X0Language = X0ProgramAbs of X0Variable list * X0Instruction list 
                    | X0ProgramImp of Map<X0Variable, X0Reference> * X0Instruction list
    and X0Variable = string
    and X0Reference = X0RR of X0Register | X0RD of X0Register * int | X0RV of X0Variable | X0RNone
    and X0Instruction = AddQ of X0Arg * X0Cell 
                        | SubQ of X0Arg * X0Cell
                        | MulQ of X0Arg * X0Cell
                        | DivQ of X0Arg * X0Cell
                        | MovQ of X0Arg * X0Cell
                        | RetQ 
                        | NegQ of X0Cell
                        | CallQ of string
                        | PushQ of X0Arg
                        | PopQ of X0Cell
    and X0Arg = X0Int of int | X0Var of X0Variable | X0Reg of X0Register | X0Deref of X0Register * int
    and X0Cell = X0TVar of X0Variable | X0TReg of X0Register | X0TDeref of X0Register * int
    and X0Register = Rsp | Rbp | Rax | Rbx | Rcx | Rdx | Rsi | Rdi | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

    
    