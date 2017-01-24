open OUnit2;;
open Printf



let function1 = 
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



(*Testing function 1*)
let function_has = Register_alloc.allocate function1
(*Testing Number of functions*)
let actual_function_number=function_has#statistics.num_bindings;;
let test1 test_ctxt = assert_equal 2 (actual_function_number);;

(*testing the nb of variables and registers allocated per function*)
  (*Function f*)
let fVariables =ref 0;;
let fu = function_has#look_for "f" ;;
Hashtbl.iter(fun key value -> fVariables:=!fVariables+1 )fu#get_hast;;
let test2 test_ctxt = assert_equal 1 (!fVariables);;
     (*nb registers*)
(*Function main*)
let mainVariables =ref 0;;
let fu = function_has#look_for "_" in
Hashtbl.iter(fun key value -> mainVariables:=!mainVariables+1 )fu#get_hast;;
let test3 test_ctxt = assert_equal 1 (!mainVariables);;





(* Name the test cases and group them together *)
let f1 =
"function 1">:::
 ["nb functions test">:: test1;
  "nb variables in function f">:: test2;
  "nb variables un function main">:: test3]
;;





let () =

  print_int !fVariables;

  print_string "***** Testing function 1 ******";
  run_test_tt_main f1
;;