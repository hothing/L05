open R1Lang
open R1Interpreter
open R1Uniquify
open R1Flatten
open C0Lang
open C0Interpreter
open X0Select

printfn "Hello from F#: Nanopass compiler book exercises"

let inputs = [5]


// (program (- (- (+ 12 -2) (read))))
let e1 = Binary(Add, EInt 12, EInt -2)
let e2 = Binary(Sub, e1, Read)
let e3 = Unary(Minus, e2)
let prg = Program(e3)

interpreter prg inputs |> printfn "[0.0]>> %A"

prg |> printfn "[0]> %A"

let prg2 = Program(Let("x", EInt 13, Var("x")))
interpreter prg2 inputs |> printfn "[0.1]>> %A"

let prg3 = Program(Let("x", EInt 13, Binary(Sub, Var("x"), Read)))
interpreter prg3 inputs |> printfn "[0.2]>> %A"

// (program (let [x 32] (+ (let [x 10] x) x)))
// (program (let [x1 32] (+ (let [x2 10] x2) x1)))

let prg4 = Program(Let("x", EInt 32, Binary(Add, Let("y", EInt 10, Var("y")), Var("x"))))
interpreter prg4 inputs |> printfn "[0.3]>> %A"

let prg4' = Program(Let("x", EInt 32, Binary(Add, Let("x", EInt 10, Var("x")), Var("x"))))
interpreter prg4' inputs |> printfn "[0.3.1]>> %A"

// (program (let ([x (read)]) (let ([y (read)]) (- x y))))

let prg5 = Program(Let("x", Read, Let("y", Read, Binary(Sub, Var("x"), Var("y")))))
interpreter prg5 [52; 10] |> printfn "[0.4]>> %A"

// (program (let [x 1] (let [x -x] (let [x (+ x 10)] x))))
let prg6 = Program(Let("x", EInt 1, Let("x", Unary(Minus, Var("x")), Let("x", Binary(Add, Var("x"), EInt 10), Var("x")))))
interpreter prg6 [] |> printfn "[0.5]>> %A"

(*
// uniquify tests 
uniquify prg |> printfn "[5.0]>> %A"
uniquify prg2  |> printfn "[5.1]>> %A"
uniquify prg3  |> printfn "[5.2]>> %A"
uniquify prg4  |> printfn "[5.3]>> %A"
uniquify prg4' |> printfn "[5.3.1]>> %A"
uniquify prg5  |> printfn "[5.4]>> %A"

// flatten tests 
flatten prg |> printfn "[6.0]>> %A"
flatten prg2 |> printfn "[6.1]>> %A"
flatten prg3 |> printfn "[6.2]>> %A"
*)

(*
flatten prg4 |> printfn "[6.3]>> %A"
    BUG:
    (C0Program
        (["m.3"; "m.2"; "m.1"; "t.2"; "t.1"],
            [C0Assign ("t.1", C0Arg (C0Int 32)); 
            C0Assign ("t.2", C0Arg (C0Int 10));
            C0Assign ("m.1", C0Arg (C0Var "t.2")); <-- HERE
            C0Assign ("m.2", C0Add (C0Var "m.1", C0Var "t.2")); <-- HERE
            C0Assign ("m.3", C0Arg (C0Var "m.2")); C0Return (C0Var "m.3")]),
        (2, [("x", "t.2"); ("x", "t.1")]))
*)
(*
flatten prg4' |> printfn "[6.3.1]>> %A"
    (C0Program
        (["m.3"; "m.2"; "m.1"; "t.2"; "t.1"],
            [C0Assign ("t.1", C0Arg (C0Int 32)); 
            C0Assign ("t.2", C0Arg (C0Int 10));
            C0Assign ("m.1", C0Arg (C0Var "t.2"));
            C0Assign ("m.2", C0Add (C0Var "m.1", C0Var "t.2"));
            C0Assign ("m.3", C0Arg (C0Var "m.2")); C0Return (C0Var "m.3")]),
        (2, [("x", "t.2"); ("x", "t.1")]))

flatten prg5 |> printfn "[6.4]>> %A"
*)

(*
//flatten prg6 |> printfn "[7.1]>> %A"

// C0 interpreter test
let c0prg, _ = flatten prg
c0Interpreter c0prg inputs |> printfn "[8.0]>> %A"

let c0prg2, _ = flatten prg2
c0Interpreter c0prg2 inputs |> printfn "[8.1]>> %A"

let c0prg3, _ = flatten prg3
c0Interpreter c0prg3 inputs |> printfn "[8.2]>> %A"

let c0prg4, _ = flatten prg4
c0prg4 |> printfn "[8.3.0]~~ %A"
c0Interpreter c0prg4 inputs |> printfn "[8.3.0]>> %A" // BUG: wrong result, because of error in R1Flatten!

let c0prg4', _ = flatten prg4'
c0prg4' |> printfn "[8.3.1]~~ %A"
c0Interpreter c0prg4' inputs |> printfn "[8.3.1]>> %A"

let c0prg5, _ = flatten prg5
c0Interpreter c0prg5 [52; 10] |> printfn "[8.4]>> %A"

let c0prg6, _ = flatten prg6
c0Interpreter c0prg6 [] |> printfn "[8.5]>> %A"
*)

(*
x0gen "z" (C0Add(C0Var("y"), C0Var("x"))) |> printfn "[X0][0.0] = %A"
x0gen "x" (C0Add(C0Int(1), C0Int(2))) |> printfn "[X0][0.1] = %A"
x0gen "x" (C0Add(C0Int(1), C0Var("y"))) |> printfn "[X0][0.2] = %A"
x0gen "x" (C0Add(C0Var("y"), C0Int(1))) |> printfn "[X0][0.3] = %A"
x0gen "x" (C0Add(C0Int(1), C0Var("x"))) |> printfn "[X0][0.4] = %A"
x0gen "x" (C0Add(C0Var("x"), C0Int(1))) |> printfn "[X0][0.5] = %A"

x0gen "z" (C0Sub(C0Var("y"), C0Var("x"))) |> printfn "[X0][1.0] = %A"
x0gen "x" (C0Sub(C0Int(1), C0Int(2))) |> printfn "[X0][1.1] = %A"
x0gen "x" (C0Sub(C0Int(1), C0Var("y"))) |> printfn "[X0][1.2] = %A"
x0gen "x" (C0Sub(C0Var("y"), C0Int(1))) |> printfn "[X0][1.3] = %A"
x0gen "x" (C0Sub(C0Int(1), C0Var("x"))) |> printfn "[X0][1.4] = %A"
x0gen "x" (C0Sub(C0Var("x"), C0Int(1))) |> printfn "[X0][1.5] = %A"

x0gen "z" (C0Div(C0Var("y"), C0Var("x"))) |> printfn "[X0][3.0] = %A"
x0gen "x" (C0Div(C0Int(1), C0Int(2))) |> printfn "[X0][3.1] = %A"
x0gen "x" (C0Div(C0Int(1), C0Var("y"))) |> printfn "[X0][3.2] = %A"
x0gen "x" (C0Div(C0Var("y"), C0Int(1))) |> printfn "[X0][3.3] = %A"
x0gen "x" (C0Div(C0Int(1), C0Var("x"))) |> printfn "[X0][3.4] = %A"
x0gen "x" (C0Div(C0Var("x"), C0Int(1))) |> printfn "[X0][3.5] = %A"

x0gen "z" (C0Minus(C0Var("y"))) |> printfn "[X0][4.0] = %A"
x0gen "y" (C0Minus(C0Var("y"))) |> printfn "[X0][4.1] = %A"
x0gen "z" (C0Minus(C0Int(1))) |> printfn "[X0][4.2] = %A"

x0gen "z" (C0Read) |> printfn "[X0][5.0] = %A"

x0gen "y" (C0Arg(C0Var("z"))) |> printfn "[X0][6.0] = %A"
x0gen "y" (C0Arg(C0Var("y"))) |> printfn "[X0][6.1] = %A"
x0gen "z" (C0Arg(C0Int(1))) |> printfn "[X0][6.2] = %A"
*)

let c0prg6, _ = flatten prg6
prg6 |> printfn "[X0][10.0] ~~> %A"
c0prg6 |> printfn "[X0][10.0] -> %A"
selectInstruction c0prg6 |> printfn "[X0][10.0] = %A"

(*
    (Let
     ("x", EInt 1,
      Let
        ("x", Unary (Minus, Var "x"),
         Let ("x", Binary (Add, Var "x", EInt 10), Var "x"))))
    ---------------
    (["m.5"; "m.4"; "m.3"; "t.3"; "m.2"; "t.2"; "m.1"; "t.1"],
   [C0Assign ("t.1", C0Arg (C0Int 1)); 
    C0Assign ("m.1", C0Minus (C0Var "t.1"));
    C0Assign ("t.2", C0Arg (C0Var "m.1"));
    C0Assign ("m.2", C0Add (C0Var "t.2", C0Int 10));
    C0Assign ("t.3", C0Arg (C0Var "m.2")); 
    C0Assign ("m.3", C0Arg (C0Var "t.3"));
    C0Assign ("m.4", C0Arg (C0Var "m.3")); 
    C0Assign ("m.5", C0Arg (C0Var "m.4"));
    C0Return (C0Var "m.5")])
    --------------
    (["m.5"; "m.4"; "m.3"; "t.3"; "m.2"; "t.2"; "m.1"; "t.1"],
   [MovQ (X0Int 1, X0TVar "t.1"); 
    MovQ (X0Var "t.1", X0TVar "m.1");
    NegQ (X0TVar "m.1"); 
    MovQ (X0Var "m.1", X0TVar "t.2");
    MovQ (X0Var "t.2", X0TVar "m.2"); 
    AddQ (X0Int 10, X0TVar "m.2");
    MovQ (X0Var "m.2", X0TVar "t.3"); 
    MovQ (X0Var "t.3", X0TVar "m.3");
    MovQ (X0Var "m.3", X0TVar "m.4"); 
    MovQ (X0Var "m.4", X0TVar "m.5");
    MovQ (X0Var "m.5", X0TReg Rax)])
*)