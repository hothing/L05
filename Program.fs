open R0Lang
open R0Interpreter

printfn "Hello from F#: Nanopass compiler book exercises"

let inputs = [5]

// (program (- (- (+ 12 -2) (read))))
let e1 = Binary(Add, EInt 12, EInt -2)
let e2 = Binary(Sub, e1, Read)
let e3 = Unary(Minus, e2)
let prg = Program(e3)

let res = interpreter prg inputs

res |> printfn ">> %A"