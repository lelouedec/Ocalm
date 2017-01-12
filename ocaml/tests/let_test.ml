open KNormal

(* let x = (let y = x + y in x2) in x + x2  *)
let case1 () = 
	print_endline ">> case 1";
	let knormed = 
		Let (
			(
				"x",
				Type.Var (ref (Some Type.Float))
			),
			Let (
				(
					"y",
					Type.Var (ref (Some Type.Int))
				),
				Add (
          "x",
          "y"
        ),
        Var "x2"
			),
			Add (
        "x",
        "x3"
      )
		) in
	let letreduction_ed = Let.f knormed in
	print_endline (KNormal.to_string letreduction_ed)

let () =
  print_endline "Nested let reduction tests";
  case1 ()
