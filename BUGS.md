# BUGS list

## Uniquify bug 1

let prg4 = Program(Let("x", EInt 32, 
    Binary(Add, 
        Let("y", EInt 10, Var("y")), 
        Var("x"))))
let prg4' = Program(Let("x", EInt 32, Binary(Add, Let("x", EInt 10, Var("x")), Var("x"))))

Hello from F#: Nanopass compiler book exercises
[0.3]>> (42, [5])
[0.3.1]>> (42, [5])
[5.3]>> (Program
   (Let
      ("t.1", EInt 32, 
        Binary (Add, Let ("t.2", EInt 10, Var "t.2"), 
        Var "t.1"))),
 (2, [("y", "t.2"); ("x", "t.1")]))
[5.3.1]>> (Program
   (Let
      ("t.1", EInt 32, 
        Binary (Add, Let ("t.2", EInt 10, Var "t.2"), 
        Var "t.2"))),
 (2, [("x", "t.2"); ("x", "t.1")]))

[8.3.0]~~ C0Program
  (["m.3"; "m.2"; "m.1"; "t.2"; "t.1"],
   [C0Assign ("t.1", C0Arg (C0Int 32)); 
    C0Assign ("t.2", C0Arg (C0Int 10));
    C0Assign ("m.1", C0Arg (C0Var "t.2"));
    C0Assign ("m.2", C0Add (C0Var "m.1", C0Var "t.1"));
    C0Assign ("m.3", C0Arg (C0Var "m.2")); C0Return (C0Var "m.3")])        
[8.3.0]>> (42, [5])
[8.3.1]~~ C0Program
  (["m.3"; "m.2"; "m.1"; "t.2"; "t.1"],
   [C0Assign ("t.1", C0Arg (C0Int 32)); 
    C0Assign ("t.2", C0Arg (C0Int 10));
    C0Assign ("m.1", C0Arg (C0Var "t.2"));
    C0Assign ("m.2", C0Add (C0Var "m.1", C0Var "t.2")); <-- BUG HERE
    C0Assign ("m.3", C0Arg (C0Var "m.2")); C0Return (C0Var "m.3")])        
[8.3.1]>> (20, [5])

