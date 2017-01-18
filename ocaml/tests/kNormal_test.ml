open KNormal

let case1 =
  let e = 
    Syntax.Let (
      (
       "x",
        Type.Var (ref (Some Type.Int))
      ),
      Syntax.Add (
        Syntax.Var "x",
        Syntax.Add (
          Syntax.Var "x",
          Syntax.Sub (
            Syntax.Int 2,
            Syntax.Int 3
          )
        )
      ),
			Syntax.Unit) in
  let _exp = f e in ()

(* TODO case IfEq / IfLE *)

let _ =
  print_string "K-Normalization tests... ";
  case1;
  print_endline "passed"
