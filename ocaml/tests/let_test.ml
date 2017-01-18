open KNormal

(* x + y  *)
let case0 () = 
	print_endline ">> case 0";
	let knormed = 
			Add (
        "x",
        "y"
      ) in
	let letreduction_ed = Let.f knormed in
	print_endline (KNormal.to_string letreduction_ed)


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
          "v1",
          "v2"
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
					Type.Var (ref (Some Type.Float))
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
							Type.Var (ref (Some Type.Float))
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

(* let rec f x = (let y = ( let v1 = 1 in ( let v2 = 2 in v1 + v2 ) ) in x + y) in (let x = 2 in f x)*)
let case3 () =
  print_endline ">> case 3";
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
  let letreduction_ed = Let.f knormed in
  print_endline (KNormal.to_string letreduction_ed)



let () =
  print_endline "Nested let reduction tests";
	case0 ();
  case1 ();
  case2 ();
  case3 ()
