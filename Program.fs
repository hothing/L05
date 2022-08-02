open R0Lang
open R0Interpreter
open R0PartialEval

printfn "Hello from F#: Nanopass compiler book exercises"

let inputs = [5]

// (program (- (- (+ 12 -2) (read))))
let e1 = Binary(Add, EInt 12, EInt -2)
let e2 = Binary(Sub, e1, Read)
let e3 = Unary(Plus, e2)
let prg = Program(e3)

interpreter prg inputs |> printfn ">> %A"

prg |> printfn "[0]> %A"
//sort prg |> foldA |> foldA |> printfn "[0]~> %A"
partialEvaluator prg |> printfn "[0]--> %A"

// Expr : (1 - (1 + (1 + R)))
let e4 = Binary(Add, EInt 1, Read)
let e5 = Binary(Add, EInt 1, e4)
let e6 = Binary(Sub, EInt 1, e5)
//let e7 = Binary(Add, EInt 1, e4)
//let e8 = Binary(Add, EInt 1, e4)
let prg2 = Program(e6)

prg2 |> printfn "[1]> %A"
//foldA prg2 |> printfn "[1]~> %A"
partialEvaluator prg2 |> printfn "[1]--> %A"

// Expr : (1 - (1 + ((2  * 3) + R))) == -6 + R
let prg3 = Program(Binary(Sub, EInt 1, Binary(Add, EInt 1, Binary(Add, Binary(Mul, EInt 2, EInt 3), Read))))

prg3 |> printfn "[2]> %A"
partialEvaluator prg3 |> printfn "[2]--> %A"

// Expr : (1 - (2 * ((1 * 2) * R))) == -5 + R
let prg4 = Program(Binary(Sub, EInt 1, Binary(Mul, EInt 1, Binary(Mul, Binary(Mul, EInt 1, EInt 2), Read))))

let partialEvaluator prg =
        match prg with 
        | Program exp -> Program(expSort exp |> peval |> foldAdd |> foldMul)

prg4 |> printfn "[3]> %A"
partialEvaluator prg4 |> printfn "[3]--> %A"
