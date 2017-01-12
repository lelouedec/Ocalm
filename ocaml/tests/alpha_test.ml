open KNormal

(* variable replacement expected if declared in let *)
let case1 () =
  print_endline ">> case 1";
  (* let x = 1 in (let _ = (let x = 2. in x + y) in x) *)
  let knormed =
    Let (
      ( "x", Type.Var (ref (Some Type.Int)) ),
      Int 1,
      Let (
        ( "_", Type.Var (ref (Some Type.Unit)) ),
        Let (
          ( "x", Type.Var (ref (Some Type.Float)) ),
          Float 2.,
          Add ( "x", "y" )
        ),
        Var "x"
      )
    ) in
  let alpha_ed = Alpha.f knormed in
  print_endline (KNormal.to_string alpha_ed);
  assert (not (alpha_ed = knormed))

let () =
  print_endline "Alpha-conversion tests";
  case1 ()
