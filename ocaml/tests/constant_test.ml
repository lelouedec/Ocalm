open KNormal

(* let v1 = 1 in ( let v2 = 2 in v1 + v2 ) *)
let case1 () =
  print_endline ">> case 1";
  let knormed =
    Let (
      (
        "v1",
        Type.Var (ref (Some Type.Int))
      ),
      Int 1,
      Let (
        (
        "v2",
          Type.Var (ref (Some Type.Int))
        ),
        Int 2,
        Add ( "v1", "v2" )
      )
    ) in
  let constant_ed = Constant.f knormed in
  print_endline (KNormal.to_string constant_ed)

(* let v1 = 1. in ( let v2 = 2. in v1 /. v2 ) *)
let case2 () =
  print_endline ">> case 2";
  let knormed =
    Let (
      (
        "v1",
        Type.Var (ref (Some Type.Float))
      ),
      Float 1.,
      Let (
        (
        "v2",
          Type.Var (ref (Some Type.Float))
        ),
        Float 2.,
        FDiv ( "v1", "v2" )
      )
    ) in
  let constant_ed = Constant.f knormed in
  print_endline (KNormal.to_string constant_ed)

(* let rec f x = (let y = ( let v1 = 1 in ( let v2 = 2 in v1 + v2 ) ) in x + y) in (let x = 2 in f x)*)
let case3 () =
  print_endline ">> case 3";
  let knormed =
    LetRec (
      {
        name = ( "f", Type.Var (ref (Some Type.Int)) );
        args = [("x", Type.Int)];
        body = Let (
          ( "y", Type.Var (ref (Some Type.Int)) ),
          Let (
            (
              "v1",
              Type.Var (ref (Some Type.Int))
            ),
            Int 1,
            Let (
              (
              "v2",
                Type.Var (ref (Some Type.Int))
              ),
              Int 2,
              Add ( "v1", "v2" )
            )
          ),
          Add ( "x", "y" )
        )
      },
      Let (
        ( "x", Type.Var (ref (Some Type.Int)) ),
        Int 2,
        App( "f", ["x"])
      )
    ) in
  let constant_ed = Constant.f knormed in
  print_endline (KNormal.to_string constant_ed)

let () =
  print_endline "Constant folding tests";
  case1 ();
  case2 ();
  case3 ()
