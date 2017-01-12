open Asml




let () = 
let exp4 =
    LetLabelEq ("succ",["x";"y";"z";"u"], LetIdentEq("t",Neg("1"),Exp( Add( "x" , Ident "t" ))),  (* x +t *)
     LetUnderscEQ(LetIdentEq("y",Neg("1"),Exp (CallLabel("succ ",[Int 1;Int 3;Ident "x";Int 4])) )))   in  (*pb succ*)
Asml.test exp4 ;
let h3 = Register_alloc.allocate exp4 in () ;
