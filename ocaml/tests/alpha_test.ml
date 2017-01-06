open KNormal

(* simple let, no variable replacement expected *)
let case1 () =
  print_endline ">> case 1";
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
  print_endline (KNormal.to_s alpha_ed);
  assert (alpha_ed = knormed)

(* nested let with same name variables, variable replacement expected *)
let case2 () =
  print_endline ">> case 2";
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
  print_endline (KNormal.to_s alpha_ed);
  assert (not (alpha_ed = knormed))

let () =
  case1 ();
  case2 ()
