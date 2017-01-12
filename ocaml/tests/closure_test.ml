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
  print_endline ">> case 1";
  print_endline (to_string closured);
  ()

let case2 () =
  (* let rec double x = x + x in (let y = 1 in double y) *)
  let knormed =
    KNormal.LetRec (
      {
        KNormal.name = ("double", Type.Int);
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
  print_endline ">> case 2";
  print_endline (to_string closured);
  ()

let case3 () =
  (* let rec f x = (let rec g y = x + y in g) in
    let t = 1 in
    let z = f t in
    (z t) *)
  let knormed =
    KNormal.LetRec (
      {
        KNormal.name = ("f", Type.Fun ([Type.Int], Type.Fun ([Type.Int], Type.Int) )); (* fun int -> (fun int -> int) *)
        KNormal.args = [("x", Type.Int)];
        KNormal.body = KNormal.LetRec (
          {
            KNormal.name = ("g", Type.Fun ([Type.Int], Type.Int) ); (* fun int -> int *)
            KNormal.args = [("y", Type.Int)];
            KNormal.body = KNormal.Add ("x", "y")
          },
          KNormal.App ( "g", [] )
        )
      },
      KNormal.Let (
        ("t", Type.Int),
        KNormal.Int 1,
        KNormal.Let (
          ("z", Type.Fun ([Type.Int], Type.Int)),
          KNormal.App ( "f", ["t"] ),
          KNormal.App ( "z", ["t"] )
        )
      )
    ) in
    let closured = f knormed in
    print_endline ">> case 3";
    print_endline (KNormal.to_string knormed);
    print_endline "-- after --";
    print_endline (to_string closured);
    ()

let case4 () =
  (* let z = 1 in print_int z *)
  let knormed =
    KNormal.Let (
      ("z", Type.Int),
      KNormal.Int 1,
      KNormal.AppExt ("print_int", ["z"])
    ) in
  let closured = f knormed in
  print_endline ">> case 4";
  print_endline (to_string closured)

let () =
  print_endline "\n*****\nClosure conversion tests";
  case1 ();
  case2 ();
  case3 ();
  case4 ()
