open R1Lang
open R1Interpreter
open R1Uniquify
open R1Flatten

printfn "Hello from F#: Nanopass compiler book exercises"

let inputs = [5]

// (program (- (- (+ 12 -2) (read))))
let e1 = Binary(Add, EInt 12, EInt -2)
let e2 = Binary(Sub, e1, Read)
let e3 = Unary(Minus, e2)
let prg = Program(e3)

interpreter prg inputs |> printfn "[0]>> %A"

prg |> printfn "[0]> %A"

let prg2 = Program(Let("x", EInt 13, Var("x")))
interpreter prg2 inputs |> printfn "[1]>> %A"

let prg3 = Program(Let("x", EInt 13, Binary(Sub, Var("x"), Read)))
interpreter prg3 inputs |> printfn "[2]>> %A"

// (program (let ([x 32]) (+ (let ([x 10]) x) x)))
// (program (let ([x1 32]) (+ (let ([x2 10]) x2) x1)))

let prg4 = Program(Let("x", EInt 32, Binary(Add, Let("x", EInt 10, Var("x")), Var("x"))))
interpreter prg4 inputs |> printfn "[3]>> %A"

// (program (let ([x (read)]) (let ([y (read)]) (- x y))))
let prg5 = Program(Let("x", Read, Let("y", Read, Binary(Sub, Var("x"), Var("y")))))
interpreter prg4 [52; 10] |> printfn "[4]>> %A"

// uniquify tests 
uniquify prg |> printfn "[5.0]>> %A"
uniquify prg2  |> printfn "[5.1]>> %A"
uniquify prg3  |> printfn "[5.2]>> %A"
uniquify prg4  |> printfn "[5.3]>> %A"
uniquify prg5  |> printfn "[5.4]>> %A"

// flatten tests 
prg |> printfn "[6.0]~~ %A"
flatten prg |> printfn "[6.0]>> %A"
(*
flatten prg2  |> printfn "[6.1]>> %A"
flatten prg3  |> printfn "[6.2]>> %A"
flatten prg4  |> printfn "[6.3]>> %A"
flatten prg5  |> printfn "[6.4]>> %A"
*)