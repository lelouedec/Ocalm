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
  let t = St.find "x" !Typing.st in
  assert(
    t = Type.Var(ref (Some Type.Int))
    || t = Type.Int
  )

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
  let ty = St.find "y" !Typing.st in
  let tx = St.find "x" !Typing.st in
  let tf = St.find "f" !Typing.st in
  assert((Type.to_string ty) = "float");
  assert((Type.to_string tx) = "float");
  assert((Type.to_string tf) = "(float -> float)");
  ()

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
