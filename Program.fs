open R1Lang
open R1Interpreter
open R1Uniquify
open R1Flatten
open C0Lang
open C0Interpreter
open X0Lang
open X0Select
open X0Patch
open X0Homes
open X0Print
open X0Spill
open ALGraph
open GraphColoring

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

let tie f x =
    f x
    x

let c0prg6 = flatten prg6
prg6 |> printfn "[X0][10.0] ~~> %A"
c0prg6 |> printfn "[X0][10.0] -> %A"
let x0prg6 = selectInstruction c0prg6 
x0prg6 |> printfn "[X0][10.0] = %A"

let moveRed instr1 instr2 =
    match (instr1, instr2) with
    | (MovQ(arg1, cell1), MovQ(arg2, cell2)) -> 
        match cell1, arg2 with 
        | (X0TVar(tvar), X0Var(svar)) -> 
            if tvar = svar then Some(MovQ(arg1, cell2))
            else None
        | (X0TDeref(treg, tofs), X0Deref(sreg, sofs)) ->
            if (treg = sreg) && (tofs = sofs) then Some(MovQ(arg1, cell2))
            else None
        | _ -> None
    | _ -> None

reduction moveRed x0prg6 |> printfn "%A"

prg2 |> flatten |> selectInstruction 
     |> reduction moveRed |> patching |>  assignHomes Rbp 0 
     |> snd |> print
     |> printfn "[Z2] %A"
prg3 |> flatten |> selectInstruction 
     |> reduction moveRed |> patching |>  assignHomes Rbp 0 
     |> snd |> print
     |> printfn "[Z3] %A"
prg4 |> flatten |> selectInstruction 
     |> reduction moveRed |> patching |>  assignHomes Rbp 0 
     |> snd |> print
     |> printfn "[Z4] %A"
prg5 |> flatten |> selectInstruction 
     |> reduction moveRed |> patching |>  assignHomes Rbp 0 
     |> tie (printfn "[W5] %A")
     |> snd |> print
     |> printfn "[Z5] %A"

(*
    (program (v w x y z t.1 t.2)
(movq (int 1) (var v))
(movq (int 46) (var w))
(movq (var v) (var x))
(addq (int 7) (var x))
(movq (var x) (var y))
(addq (int 4) (var y))
(movq (var x) (var z))
(addq (var w) (var z))
(movq (var y) (var t.1))
(negq (var t.1))
(movq (var z) (var t.2))
(addq (var t.1) (var t.2))
(movq (var t.2) (reg rax)))
*)
let x0prg = X0Program(["v"; "w"; "x"; "y"; "z"; "t.1"; "t.2"], [
    MovQ(X0Int 1, X0TVar "v");
    MovQ(X0Int 46, X0TVar "w");
    MovQ(X0Var "v", X0TVar "x");
    AddQ(X0Int 7, X0TVar "x");
    MovQ(X0Var "x", X0TVar "y");
    AddQ(X0Int 4, X0TVar "y");
    MovQ(X0Var "x", X0TVar "z");
    AddQ(X0Var "w", X0TVar "z");
    MovQ(X0Var "y", X0TVar "t.1");
    NegQ(X0TVar "t.1");
    MovQ(X0Var "z", X0TVar "t.2");
    AddQ(X0Var "t.1", X0TVar "t.2");
    MovQ(X0Var "t.2", X0TReg Rax);
]) 

match x0prg with
| X0Program(vars, stmts) -> x0spilling (Set.empty) stmts |> printfn "[V5] %A"

(*
    [
        (MovQ (X0Int 1, X0TVar "v"), set [X0RV "v"; X0RNone]);
        (MovQ (X0Int 46, X0TVar "w"), set [X0RV "v"; X0RV "w"; X0RNone]);
        (MovQ (X0Var "v", X0TVar "x"), set [X0RV "w"; X0RNone]);
        (AddQ (X0Int 7, X0TVar "x"), set [X0RV "w"; X0RV "x"; X0RNone]);
        (MovQ (X0Var "x", X0TVar "y"), set [X0RV "w"; X0RV "x"; X0RNone]);
        (AddQ (X0Int 4, X0TVar "y"), set [X0RV "w"; X0RV "x"; X0RV "y"]);
        (MovQ (X0Var "x", X0TVar "z"), set [X0RV "w"; X0RV "y"]);
        (AddQ (X0Var "w", X0TVar "z"), set [X0RV "y"; X0RV "z"]);
        (MovQ (X0Var "y", X0TVar "t.1"), set [X0RV "z"]);
        (NegQ (X0TVar "t.1"), set [X0RV "t.1"; X0RV "z"]);
        (MovQ (X0Var "z", X0TVar "t.2"), set [X0RV "t.1"]);
        (AddQ (X0Var "t.1", X0TVar "t.2"), set [X0RV "t.2"]);
        (MovQ (X0Var "t.2", X0TReg Rax), set [])
    ]
*)


let g1 = match x0prg with
            | X0Program(vars, stmts) -> 
                x0spilling (Set.empty) stmts |> fst |> x0makeInterGraph 
    
g1 |> printfn "[V5.1] %A"

(*
    [
        (X0RV "t.1", set [X0RV "t.2"; X0RV "z"]); 
        (X0RV "t.2", set [X0RV "t.1"]);
        (X0RV "v", set [X0RV "w"]);
        (X0RV "w", set [X0RV "v"; X0RV "x"; X0RV "y"; X0RV "z"]);
        (X0RV "x", set [X0RV "w"; X0RV "y"]);
        (X0RV "y", set [X0RV "w"; X0RV "x"; X0RV "z"]);
        (X0RV "z", set [X0RV "t.1"; X0RV "w"; X0RV "y"]); 
        (X0RNone, set [])
    ]
*)

let gc1 = makeColoredGraph 9 g1

gc1 
|> printfn "[V5.2] %A"

coloring gc1 |> printfn "[V5.3] %A"