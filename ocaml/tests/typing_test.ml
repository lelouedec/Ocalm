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

let _ =
  case1;
  case2;
  print_endline "Type inference tests... passed";
