module R0Lang

    type UnaryOp = Plus | Minus
    type ArithOp = Add | Sub | Mul | Div 

    type Language = Program of Expression
    and Expression =   EInt of int 
                        | Read 
                        | Unary of UnaryOp * Expression 
                        | Binary of ArithOp * Expression * Expression
