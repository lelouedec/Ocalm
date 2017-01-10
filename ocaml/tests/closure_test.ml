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
        name = ("double", Type.Int);
        args = [("x", Type.Int)];
        body = KNormal.Add ("x", "x")
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
  (* let rec make_adder x = (let rec adder y = x + y in adder) in
    let t = 1 in
    let z = make_adder t in
    (z t) *)
  let knormed =
    KNormal.LetRec (
      {
        name = ("make_adder", Type.Fun ([Type.Int], Type.Fun ([Type.Int], Type.Int) )); (* fun int -> (fun int -> int) *)
        args = [("x", Type.Int)];
        body = KNormal.LetRec (
          {
            name = ("adder", Type.Fun ([Type.Int], Type.Int) ); (* fun int -> int *)
            args = [("y", Type.Int)];
            body = KNormal.Add ("x", "y")
          },
          KNormal.App ( "adder", [] )
        )
      },
      KNormal.Let (
        ("t", Type.Int),
        KNormal.Int 1,
        KNormal.Let (
          ("z", Type.Fun ([Type.Int], Type.Int)),
          KNormal.App ( "make_adder", ["t"] ),
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

let () =
  print_endline "\n*****\nClosure conversion tests";
  case1 ();
  case2 ();
  case3 ();
