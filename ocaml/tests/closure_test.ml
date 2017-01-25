open Closure

let case1 () =
  let knormed =
    KNormal.Let (
      ( "z", Type.Var (ref (Some Type.Int)) ),
      KNormal.Int 1,
      KNormal.Let (
        ( "x", Type.Var (ref (Some Type.Int)) ),
        KNormal.Add ( "y", "z" ),
        KNormal.Add ( "x", "y" )
      )
    ) in
  let closured = f knormed in
  ()

let case2 () =
  (* let rec double x = x + x in (let y = 1 in double y) *)
  let knormed =
    KNormal.LetRec (
      {
        KNormal.name = ("double", Type.Var (ref (Some Type.Int)));
        KNormal.args = [("x", Type.Int)];
        KNormal.body = KNormal.Add ("x", "x")
      },
      KNormal.Let (
        ("y", Type.Int),
        KNormal.Int 1,
        KNormal.App ("double", ["y"])
      )
    ) in
  let closured = f knormed in
  ()

let case3 () =
  (* let rec f x = (let rec g y = x + y in g) in
    let t1 = 1 in
    let t2 = 2 in
    let z = f t1 in
    (z t2) *)
  let knormed =
    KNormal.LetRec (
      {
        KNormal.name = ("f", Type.Fun ([Type.Int], Type.Fun ([Type.Int], Type.Int))); (* fun int -> (fun int -> int) *)
        KNormal.args = [("x", Type.Int)];
        KNormal.body = KNormal.LetRec (
          {
            KNormal.name = ("g", Type.Fun ([Type.Int], Type.Int)); (* fun int -> int *)
            KNormal.args = [("y", Type.Int)];
            KNormal.body = KNormal.Add ("x", "y")
          },
          KNormal.Var ( "g" )
        )
      },
      KNormal.Let (
        ("t1", Type.Int),
        KNormal.Int 1,
        KNormal.Let (
          ("t2", Type.Int),
          KNormal.Int 2,
          KNormal.Let (
            ("z", Type.Fun ([Type.Int], Type.Int)),
            KNormal.App ( "f", ["t1"] ),
            KNormal.App ( "z", ["t2"] )
          )
        )
      )
    ) in
    let closured = f knormed in
    ()

let case4 () =
  (* let z = 1 in print_int z *)
  let knormed =
    KNormal.Let (
      ("z", Type.Int),
      KNormal.Int 1,
      KNormal.AppExt ("print_int", ["z"])
    ) in
  let closured = f knormed in ()

let case5 () =
  (* let x = 1 in
    let rec f y z =
      let v = x + z in
      v - y in
    let t1 = 2 in
    let t2 = 3 in
    f t1 t2 *)
  let knormed =
    KNormal.Let (
      ("x", Type.Int),
      KNormal.Int 1,
      KNormal.LetRec (
        {
          KNormal.name = ("f", Type.Fun ([Type.Int], Type.Int));
          KNormal.args = [("y", Type.Int); ("z", Type.Int)];
          KNormal.body = KNormal.Let (
            ("v", Type.Int),
            KNormal.Add ("x", "z"),
            KNormal.Sub ("v", "y")
          )
        },
        KNormal.Let (
          ("t1", Type.Int),
          KNormal.Int 2,
          KNormal.Let (
            ("t2", Type.Int),
            KNormal.Int 3,
            KNormal.App ("f", ["t1"; "t2"])
          )
        )
      )
    ) in
  let closured = f knormed in ()

let () =
  print_string "Closure conversion tests... ";
  case1 ();
  case2 ();
  case3 ();
  case4 ();
  case5 ();
  print_endline "assertions required"
