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
        Int 1,
        KNormal.App ("double", ["y"])
      )
    ) in
  let closured = f knormed in
  print_endline ">> case 2";
  print_endline (to_string closured);
  ()

let () =
  print_endline "\n*****\nClosure conversion tests";
  case1 ();
  case2 ();
