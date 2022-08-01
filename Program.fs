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
partialEvaluator prg |> printfn "[0]--> %A"

let e4 = Binary(Add, EInt 1, Read)
let e5 = Binary(Add, EInt 1, e4)
let prg2 = Program(e5)

prg2 |> printfn "[1]> %A"
partialEvaluator prg2 |> printfn "[1]--> %A"
