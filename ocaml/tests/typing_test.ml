open Typing

let case1 = 
  print_endline ">> case 1";
  let e = 
    Syntax.Let (
      (
        "x",
        Type.Var (ref (None))
      ),
      Syntax.Int 1, 
			Syntax.Unit) in

  let _exp = f e in ()

let _ =
  print_endline "Type inference tests";
  case1
