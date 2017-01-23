open OUnit2;;
open Printf
open Asml


let e1 =
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
                LetIdentEq("u",CallLabel ( "f",["x";"y"]),Exp ( CallLabel ( "diff",["a"; "u"]) ) ) ) ))))))


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
let function_has = Register_alloc.allocate e




(*Testing Function_Number*)

let actual_function_number=function_has#statistics.num_bindings;;
let test1 test_ctxt = assert_equal !counter (actual_function_number);;


(*testing the nb of variables per function*)
let counter =ref 0;;
Hashtbl.iter (fun key value -> (  counter:=!counter+1 ) )function_has#get_hast




(* Name the test cases and group them together *)
let suite =
"suite">:::
 [
  "test1">:: test1]
;;





let () =
  print_int !counter;;

  run_test_tt_main suite
;;