open Asml




let () = 
let exp2 =
    LetLabelEq ("f",["x";"y"], Exp( Add( "x" , Int (0) ) ), LetUnderscEQ(Exp ( CallLabel ( "f",[Ident"x";Ident "y"]) ) )) in 
Asml.test exp2 ;

let h = Register_alloc.allocate exp2 in ();
