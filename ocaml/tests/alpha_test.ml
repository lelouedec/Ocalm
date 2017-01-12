open KNormal

(* variable replacement expected if declared in let *)
let case1 () =
  print_endline ">> case 1";
  (* let x = 1 in (let _ = (let x = 2. in x + y) in x) *)
  let knormed =
    Let (
      ( "x", Type.Var (ref (Some Type.Int)) ),
      Int 1,
      Let (
        ( "_", Type.Var (ref (Some Type.Unit)) ),
        Let (
          ( "x", Type.Var (ref (Some Type.Float)) ),
          Float 2.,
          Add ( "x", "y" )
        ),
        Var "x"
      )
    ) in
  let alpha_ed = Alpha.f knormed in
  print_endline (KNormal.to_string alpha_ed);
  assert (not (alpha_ed = knormed))

(* replacement for let-rec *)
let case2 () =
  print_endline ">> case 2";
  (* let rec f x = (let y = 1 in x + y) in (let x = 2 in f x)*)
  let knormed =
    LetRec (
      {
        name = ( "f", Type.Var (ref (Some Type.Int)) );
        args = [("x", Type.Int)];
        body = Let (
          ( "y", Type.Var (ref (Some Type.Int)) ),
          Int 1,
          Add ( "x", "y" )
        )
      },
      Let (
        ( "x", Type.Var (ref (Some Type.Int)) ),
        Int 2,
        App( "f", ["x"])
      )
    ) in
  let alpha_ed = Alpha.f knormed in
  print_endline (KNormal.to_string alpha_ed)

(* leave external function (name) unchanged *)
let case3 () =
  print_endline ">> case 3";
  (* let x = 1 in print_int x *)
  let knormed =
    Let (
      ( "x", Type.Var (ref (Some Type.Int)) ),
      Int 1,
      AppExt ("print_int", ["x"])
    ) in
  let alpha_ed = Alpha.f knormed in
  print_endline (KNormal.to_string alpha_ed)

let () =
  print_endline "Alpha-conversion tests";
  case1 ();
  case2 ();
  case3 ()
