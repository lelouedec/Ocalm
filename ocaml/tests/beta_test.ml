open KNormal

let case1 () =
  print_endline ">> case 1";
  (* let x = y in x + y *)
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
  print_endline (KNormal.to_string beta_ed)

let case2 () =
  print_endline ">> case 2";
  (* let z = 1. in (let x = y +. z in x -. y) *)
  let knormed =
    Let (
      ( "z", Type.Var (ref (Some Type.Float)) ),
      Float 1.,
      Let (
        ( "x", Type.Var (ref (Some Type.Float)) ),
        FMul ( "y", "z" ),
        FDiv ( "x", "y" )
      )
    ) in
  let beta_ed = Beta.f knormed in
  print_endline (KNormal.to_string beta_ed)

let case3 () =
  print_endline ">> case 3";
  (* let z = (let x = y in x) in z *)
  let knormed =
    Let (
      ( "z", Type.Var (ref (Some Type.Int)) ),
      Let (
        ( "x", Type.Var (ref (Some Type.Int)) ),
        Var "y",
        Var "x"
      ),
      Neg "z"
    ) in
  let beta_ed = Beta.f knormed in
  print_endline (KNormal.to_string beta_ed)

let case4 () =
  print_endline ">> case 4";
  (* let f x = (let z = y in x + z) in (let v = t in f v) *)
  let knormed =
    LetRec (
      {
        name = ("f", Type.Fun ([Type.Int], Type.Int));
        args = [("x", Type.Int)];
        body = Let (
          ("z", Type.Var (ref (Some Type.Int))),
          Var "y",
          Add ("x", "z")
        )
      },
      Let (
        ("v", Type.Var (ref (Some Type.Int))),
        Var "t",
        App ("f", ["t"])
      )
    ) in
  let beta_ed = Beta.f knormed in
  print_endline (KNormal.to_string beta_ed)

let () =
  print_endline "Beta-reduction tests";
  case1 ();
  case2 ();
  case3 ();
  case4 ()
