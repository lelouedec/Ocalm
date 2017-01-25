open OUnit2;;
open Printf
open Asml

  let function1 =
    LetLabelEq (
      "Sum",
      ["x";"y";"z";"p1";"p2";"p3";"p4";"p5";"p6";"p7";"p8";"p9"],
      LetIdentEq("a",Add( "x" , Ident "y"),Exp (Add( "a" , Ident "z"))),
        LetLabelEq(
          "Diff",
          ["x";"y"],
          Exp (Sub("x",Ident "y")),
            LetUnderscEQ (LetIdentEq("a",Neg("10"),
              LetIdentEq ("x",Neg("1"),LetIdentEq("y",Neg("2"),LetIdentEq("z",Neg("3"),
                LetIdentEq("u",CallLabel ( "f",["x";"y"]),Exp ( CallLabel ( "diff",["a"; "u"]) ) ) ) ))))))



  




(***************Testing function 1*******************)
let function_has = Register_alloc.allocate function1;;
(*Testing Number of functions*)
let actual_function_number=function_has#statistics.num_bindings;;
let test1 test_ctxt = assert_equal ~msg:"Wrong number of functions stored in hashtable" 3 (actual_function_number);;

(*testing the nb of variables  per function*)
  (*Function Sum*)
let fVariables =ref 0;;
let fu = function_has#look_for "Sum" ;;
Hashtbl.iter(fun key value -> fVariables:=!fVariables+1  )fu#get_hast;;
let test2 test_ctxt = assert_equal ~msg:"Wrong number of variables stored in the hashtable for function Sum" 13 (!fVariables);;

(*Testing the nubmer of variables allocated in registers and numbers of variables in the stack*)
let register_variable =ref 0;;
let stack_variable =ref 0;;

  (*Variables on registers & stack*)
print_string "function Sum \n";;
Hashtbl.iter(fun key value -> if (value#get_is_in_stack =0) then register_variable:=!register_variable+1 else  stack_variable:=!stack_variable+1 )fu#get_hast;;
let test3 test_ctxt = assert_equal ~msg:"Wrong number of variables in the stack in function Sum"10 (!register_variable);;
let test4 test_ctxt = assert_equal ~msg:"Wrong number of variables in the stack in function Sum" 3 (!stack_variable);;

Hashtbl.iter(fun key value ->  print_string key ; print_string " "; print_string value#get_reg ; print_string " " ; print_int value#get_is_in_stack ;  print_string " Time slice    " ; value#display_timeslice ;  print_string "\n")fu#get_hast;;



(*Function Diff*)
let fVariables =ref 0;;
let fu = function_has#look_for "Diff" ;;
Hashtbl.iter(fun key value -> fVariables:=!fVariables+1  )fu#get_hast;;
let test8 test_ctxt = assert_equal ~msg:"Wrong number of variables stored in the hashtable for function Diff" 2(!fVariables);;

(*Testing the nubmer of variables allocated in registers and numbers of variables in the stack*)
let register_variable =ref 0;;
let stack_variable =ref 0;;

  (*Variables on registers & stack*)
print_string "function Diff \n";;
Hashtbl.iter(fun key value -> if (value#get_is_in_stack =0) then register_variable:=!register_variable+1 else  stack_variable:=!stack_variable+1 )fu#get_hast;;
let test9 test_ctxt = assert_equal ~msg:"Wrong number of variables in registers in function Diff" 2  (!register_variable);;
let test10 test_ctxt = assert_equal ~msg:"Wrong number of variables in the stack in function Diff" 0 (!stack_variable);;

Hashtbl.iter(fun key value ->  print_string key ; print_string " "; print_string value#get_reg ; print_string " " ; print_int value#get_is_in_stack ;  print_string " Time slice    " ; value#display_timeslice ;  print_string "\n")fu#get_hast;;






(*Function main*)
let mainVariables =ref 0;;
let fu = function_has#look_for "_" ;;
Hashtbl.iter(fun key value -> mainVariables:=!mainVariables+1 )fu#get_hast;;
let test5 test_ctxt = assert_equal 5 (!mainVariables);;

(* Variables on registers & stack *)
print_string "function main \n";;
let register_variable =ref 0;;
let stack_variable =ref 0;;
Hashtbl.iter(fun key value -> if (value#get_is_in_stack =0) then register_variable:=!register_variable+1 else  stack_variable:=!stack_variable+1 )fu#get_hast;;

let test6 test_ctxt = assert_equal 5 (!register_variable);;
let test7 test_ctxt = assert_equal ~msg:"Er" 0 (!stack_variable);;

Hashtbl.iter(fun key value ->  print_string key ; print_string " "; print_string value#get_reg ; print_string " " ; print_int value#get_is_in_stack ;  print_string " Time slice    " ; value#display_timeslice ;  print_string "\n")fu#get_hast;;

      
     



(* Grouping the test cases together *)
let f1 =
"function 1">:::
 ["checking the number of  functions stored in the program ">:: test1;
  "Checking the number of  variables stored in function Sum">:: test2;
  "Checking the number of  variables stored in function Diff">:: test8;
  "Checking the number of  variables stored in function main">:: test5;
  "Checking the number of  variables stored in registers for function Sum">:: test3;
  "Checking the number of  variables stored in the stack for function Sum">:: test4;
  "Checking the number of  variables stored in the stack for function Diff">:: test9;
  "Checking the number of  variables stored in on the stack for function Diff">:: test10;
  "Checking the number of  variables stored in on registers for function main">:: test6;
  "Checking the number of  variables stored in on the stack for function main">:: test7]
;;

let () =
  print_string "***** Testing function 1 ******";
  run_test_tt_main f1
;;