module R1Lang

    type UnaryOp = Plus | Minus
    type ArithOp = Add | Sub | Mul | Div 

    type Language = Program of Expression
    and Expression =   EInt of int 
                        | Read 
                        | Var of string
                        | Let of string * Expression * Expression
                        | Unary of UnaryOp * Expression 
                        | Binary of ArithOp * Expression * Expression
