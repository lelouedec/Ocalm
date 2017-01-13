open KNormal

(* let rec f x = x + 1 in let g = x + y in f y *)
let case1 () =
  print_endline ">> case 1";
  let knormed =
    LetRec (
      {
        name = ("v8", Type.Var (ref (Some Type.Int)));
        args = [("v9", Type.Int)];
        body = Add ("v9", "1")
      },
      Let (
        ("v10", Type.Var (ref (Some Type.Int))),
        Add ("x", "y"),
        App ("v8", ["y"])
      )
    ) in
  let inline_ed = Inline.f knormed in
  print_endline (KNormal.to_string inline_ed)


let () =
	print_endline "Inline expansion test";
	case1()