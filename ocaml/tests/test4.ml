open Asml




let () = 
let exp5 =
    LetUnderscEQ(LetIdentEq("a",Neg("2"),
      LetIdentEq("b",Neg("1"),LetIdentEq("c",Neg("0"),Exp (IfEq("a",Ident "b",Add( "s" , Ident "a" ),Add( "s" , Ident "b" ))   ) )))) in 
Asml.test exp5 ;
let h3 = Register_alloc.allocate exp5 in () ;
