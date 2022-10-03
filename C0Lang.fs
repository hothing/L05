module C0Lang

    // Grammar:
    // arg ::= int | var
    // exp ::= arg | (read) | (- arg) | (+ arg arg)
    // stmt ::= (assign var exp) | (return arg)
    // C0 ::= (program (var∗) stmt+)

    type C0Language = C0Program of C0Variable list * C0Statement list
    and C0Variable = string
    and C0Statement = C0Assign of C0Variable * C0Expression | C0Return of C0Argument
    and C0Expression = C0Arg of C0Argument 
                        | C0Read 
                        | C0Minus of C0Argument 
                        | C0Add of C0Argument * C0Argument
                        | C0Sub of C0Argument * C0Argument
                        | C0Mul of C0Argument * C0Argument
                        | C0Div of C0Argument * C0Argument
    and C0Argument = C0Int of int | C0Var of C0Variable

