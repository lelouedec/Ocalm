open Asml




let () = 
let exp3 =
    LetLabelEq ("f",["x"], LetIdentEq("z",Add( "x" , Int(1)),Exp( Sub( "z" , Int(2)) )),  (* x +t *)
     LetUnderscEQ(Exp ( CallLabel ( "f",[Ident""]) )))   in  (*pb succ*)
Asml.test exp3 ;
let h2 = Register_alloc.allocate exp3 in () ;
