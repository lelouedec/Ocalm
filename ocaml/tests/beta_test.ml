open KNormal

let case1 () =
  print_endline ">> case 1";
  (* let x = 1 in (let _ = (let x = 2. in x + y) in x) *)
  let knormed =
    Let (
      (
        "x",
        Type.Var (ref (Some Type.Int))
      ),
      Var "y",
      Add (
        "x",
        "y"
      )
    ) in
  let beta_ed = Beta.f knormed in
  print_endline (KNormal.to_s beta_ed)

let () =
  print_endline "Beta-reduction tests";
  case1 ()
