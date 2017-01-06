open KNormal

(* simple let, no variable replacement expected *)
let case1 () =
  (* let x = 1 in x *)
  let knormed =
    Let (
      (
        "x",
        Type.Var (ref (Some Type.Int))
      ),
      Int 1,
      Var "x"
    ) in
  let alpha_ed = Alpha.f knormed in
  assert (alpha_ed = knormed)

(* nested let with same name variables, variable replacement expected *)
let case2 () =
  (* let x = 1 in (let _ = (let x = 2. in x) in x) *)
  let knormed =
    Let (
      (
        "x",
        Type.Var (ref (Some Type.Int))
      ),
      Int 1,
      Let (
        (
          "_",
          Type.Var (ref (Some Type.Unit))
        ),

        Let (
          (
            "x",
            Type.Var (ref (Some Type.Float))
          ),
          Float 2.,
          Var "x"
        ),

        Var "x"
      )
    ) in
  let alpha_ed = Alpha.f knormed in
  assert (not (alpha_ed = knormed))

let () =
  case1 ();
  case2 ()
