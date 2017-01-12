open KNormal

(* let x = (let y = v1 + v2 in x2) in x + x2  *)
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


(* let x = ( let y = ( let v1 = 1 in ( let v2 = 2 in v1 + v2 ) ) in y ) in v3 *)
let case2 () =
	print_endline ">> case 2";
	let knormed = 
		Let (
			(
				"x",
				Type.Var (ref (Some Type.Int))
			),
			Let (
				(
					"y",
					Type.Var (ref (Some Type.Int))
				),
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
				Var "y"
			),
			Var "v3"
		) in
	let letreduction_ed = Let.f knormed in
	print_endline (KNormal.to_string letreduction_ed)


let () =
  print_endline "Nested let reduction tests";
  case1 ();
  case2 ()
