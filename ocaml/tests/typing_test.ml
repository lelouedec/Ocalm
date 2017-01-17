open Typing
open Syntax

let case1 = 
  let e = 
    Let (
      (
        "x",
        Type.Var (ref (None))
      ),
      Int 1, 
			Unit) in

  let _exp = f e in
  assert((St.find "x" !Typing.st) = Type.Var(ref (Some Type.Int)))

let case2 = 
  let e = 
    Let (
      (
        "y",
        Type.Var (ref (None))
      ),
      Float 1.0,
      LetRec (
        {
          name = ( "f", Type.Var (ref (None)) );
          args = [("x", Type.Var (ref (None)))];
          body =
            FAdd ( Var ("x"), Var ("y") )
        },
			  Unit)) in

  let _exp = f e in
  assert((St.find "y" !Typing.st) = Type.Var(ref (Some Type.Float)));
  assert((St.find "x" !Typing.st) = Type.Var(ref (Some Type.Float)))(*;
  assert((St.find "f" !Typing.st) = Type.Var(ref (Some Type.Float)))*)

(* let x = 1. in print_int (x + 1) *)
let case3 =
  let e =
    Let (
      ("x", Type.Var (ref None)),
      Float 1.,
      App (
        Var "print_int",
        [Add (Var("x"), Int(1))]
      )
    ) in
  try
    let _exp = f e in
    assert false
  with e -> print_endline "type error as expected"

let _ =
  print_string "Type inference tests...";
  case1;
  case2;
  case3;
  print_endline "passed"
