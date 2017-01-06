let case1 () =
  (* let x = 1 in x *)
  let knormed =
    KNormal.Let (
      (
        "x",
        Type.Var (ref (Some Type.Int))
      ),
      KNormal.Int(1),
      KNormal.Var("x")
    ) in
  let alpha_ed = Alpha.f knormed in
  assert (alpha_ed = knormed)

let () =
  case1 ()
