open Asml
open Printf

let case1 () =
  let e =
    LetLabelEq (
      "f", 
      ["x"], 
      LetIdentEq (
        "z",
        Add ( "x", Int(1) ),
        Exp ( Sub ( "z", Int(2) ) )
      ),  (* x +t *)
      LetUnderscEQ(
        Exp ( 
          CallLabel ( 
            "f",
            [""]
          ) 
        )
      )
    ) in  (*pb succ*)
  ignore(Register_alloc.allocate e)

let case2 () =
  let e =
    LetLabelEq (
      "f",
      ["x"; "y"], 
      Exp ( Add( "x", Int (0) ) ), 
      LetUnderscEQ ( 
        Exp ( 
          CallLabel ( 
            "f",
            ["x"; "y"] 
          ) 
        ) 
      )
    ) in 
  ignore(Register_alloc.allocate e)

let case3 () = 
  let e =
    LetLabelEq (
      "succ" ,
      ["x"; "y"; "z"; "u"], 
      LetIdentEq (
        "t" , 
        Neg ("1") ,
        Exp( Add ( "x" , Ident "t" ) ) 
      ),  (* x +t *)
      LetUnderscEQ (
        LetIdentEq ("y",
          Neg ("1"),
          Exp (
            CallLabel(
              "succ ",
              ["x" ;"y"]
            )
          ) 
        )
      )
    ) in  (*pb succ*)
  ignore(Register_alloc.allocate e)

let case4 () =
  let e =
    LetUnderscEQ (
      LetIdentEq (
        "a",
        Neg ("2"),
        LetIdentEq (
          "b",
          Neg ("1"),
          LetIdentEq (
            "c",
            Neg ("0"),
            Exp (
              IfEq (
                "a",
                Ident "b",
                Exp(Add("c", Ident "a")),
                Exp(Add("c", Ident "b"))
              )   
            ) 
          )
        )
      )
    ) in 
  ignore(Register_alloc.allocate e)


let case5 () =
  let e =
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
                LetIdentEq("u",CallLabel ( "f",["x";"y"]),Exp ( CallLabel ( "diff",["a"; "u"]) ) ) ) )))))) in 
ignore(Register_alloc.allocate e)

let case6 () =
  let e =
	  LetUnderscEQ ( 
      LetIdentEq (
        "x",
        Int(2),
        Exp (Add ("x", Ident "y") )
	    )
	  ) in 
  ignore(Register_alloc.allocate e)

let case7() = 
 let e = 
  LetLabelEq("f",["x"],
            Exp(
                IfEq("x",
                      Int(0),
                      Exp(Int(0)),
                      LetIdentEq("x", Sub("x",Int(1)),  LetIdentEq("x",Add("x",Ident"x"),Exp(CallLabel("f",["x"]))))
                )
            ),
            LetUnderscEQ(
                        LetIdentEq("z",
                          Int(4),
                          Exp(CallLabel("f",["z"]))
                        )
                        ) 
            )
 in 
 ignore(Register_alloc.allocate e)


let () = 
  print_string "Register allocation tests... ";
  case1 ();
  case2 ();
  case3 ();
  case4 ();
  case5 ();
  case6 ();
  case7 ();
  print_endline "assertions required"

