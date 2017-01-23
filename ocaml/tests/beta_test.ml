open KNormal

let case1 () =
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
  assert(beta_ed = Add("y", "y"))

let case2 () =
  (* let z = 1. in (let x = y *. z in x /. y) *)
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
  assert(beta_ed = knormed)

let case3 () =
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
  assert(beta_ed = Neg ("y"))

let case4 () =
  (* let f x = (let z = y in x + z) in (let v = t in f v) *)
  let knormed =
    LetRec (
      {
        name = ("f", Type.Var (ref (Some Type.Int)));
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
  assert(beta_ed = LetRec ( 
    { name = ("f", Type.Var (ref (Some Type.Int)));
      args = [("x", Type.Int)];
      body = Add ("x", "y")
    },
    App ("f", ["t"])))

let () =
  print_string "Beta-reduction tests... ";
  case1 ();
  case2 ();
  case3 ();
  case4 ();
  print_endline "passed"
