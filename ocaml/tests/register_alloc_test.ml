open Asml




let () = 
let exp3 =
    LetLabelEq ("f",["x"], LetIdentEq("z",Add( "x" , Int(1)),Exp( Sub( "z" , Int(2)) )),  (* x +t *)
     LetUnderscEQ(Exp ( CallLabel ( "f",[Ident""]) )))   in  (*pb succ*)
Asml.test exp3 ;
let h2 = Register_alloc.allocate exp3 in () ;

let exp2 =
    LetLabelEq ("f",["x";"y"], Exp( Add( "x" , Int (0) ) ), LetUnderscEQ(Exp ( CallLabel ( "f",[Ident"x";Ident "y"]) ) )) in 
Asml.test exp2 ;

let h = Register_alloc.allocate exp2 in ();

let exp4 =
    LetLabelEq ("succ",["x";"y";"z";"u"], LetIdentEq("t",Neg("1"),Exp( Add( "x" , Ident "t" ))),  (* x +t *)
     LetUnderscEQ(LetIdentEq("y",Neg("1"),Exp (CallLabel("succ ",[Int 1;Int 3;Ident "x";Int 4])) )))   in  (*pb succ*)
Asml.test exp4 ;
let h3 = Register_alloc.allocate exp4 in () ;

let exp5 =
    LetUnderscEQ(LetIdentEq("a",Neg("2"),
      LetIdentEq("b",Neg("1"),LetIdentEq("c",Neg("0"),Exp (IfEq("a",Ident "b",Add( "s" , Ident "a" ),Add( "s" , Ident "b" ))   ) )))) in 
Asml.test exp5 ;
let h3 = Register_alloc.allocate exp5 in () 
