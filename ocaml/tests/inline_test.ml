open KNormal

(* let rec f x = x + 1 in let g = x + y in f y *)
let case1 () =
  print_endline ">> case 1";
  let knormed =
    LetRec (
      {
        name = ("f", Type.Var (ref (Some Type.Int)));
        args = [("x", Type.Int)];
        body = Add ("x", "1")
      },
      Let (
        ("g", Type.Var (ref (Some Type.Int))),
        Add ("x", "y"),
        App ("f", ["y"])
      )
    ) in
  let inline_ed = Inline.f knormed in
  print_endline (KNormal.to_string inline_ed)

(* let rec f x = (let y = ( let v1 = 1 in ( let v2 = 2 in v1 + v2 ) ) in x + y) in (let x = 2 in f x)*)
let case2 () =
  print_endline ">> case 2";
  let knormed =
    LetRec (
      {
        name = ( "f", Type.Var (ref (Some Type.Int)) );
        args = [("x", Type.Int)];
        body = Let (
          ( "y", Type.Var (ref (Some Type.Int)) ),
          Let (
						(
							"v1",
							Type.Var (ref (Some Type.Int))
						),
						Int 1,
						Let (
							(
							"v2",
								Type.Var (ref (Some Type.Int))
							),
							Int 2,
							Add ( "v1", "v2" )
						)
					),
          Add ( "x", "y" )
        )
      },
      Let (
        ( "x", Type.Var (ref (Some Type.Int)) ),
        Int 2,
        App( "f", ["x"])
      )
    ) in
  let inline_ed = Inline.f knormed in
  print_endline (KNormal.to_string inline_ed)

let () =
	print_endline "Inline expansion test";
	case1 ();
	case2 ()