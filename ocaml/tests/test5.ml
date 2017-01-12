open Asml




let () = 
let exp6 =
    LetLabelEq (
      "Sum",
      ["x";"y";"z"],
      LetIdentEq("a",Add( "x" , Ident "y"),Exp (Add( "a" , Ident "z"))),
        LetLabelEq(
          "Diff",
          ["x";"y"],
          Exp (Sub("x",Ident "y")),
            LetUnderscEQ (LetIdentEq("a",Neg("10"),
              LetIdentEq ("x",Neg("1"),LetIdentEq("y",Neg("2"),LetIdentEq("z",Neg("3"),
                LetIdentEq("u",CallLabel ( "f",[Ident"x";Ident "y"]),Exp ( CallLabel ( "diff",[Ident"a";Ident "u"]) ) ) ) )))))) in 
Asml.test exp6 ;
let h4 = Register_alloc.allocate exp6 in () ;
