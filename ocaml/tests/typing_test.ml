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

  let type_eq = genenerate e St.empty St.empty Type.Unit in
		print_endline "type equations :";
    List.iter
      to_string (type_eq);
    List.iter
      unify (type_eq);
		print_endline "solved equations :";
    List.iter
      to_string (type_eq)

let _ =
  print_endline "Type inference tests";
  case1
