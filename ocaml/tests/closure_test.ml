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

let () =
  print_endline "Closure conversion tests";
  case1 ();
