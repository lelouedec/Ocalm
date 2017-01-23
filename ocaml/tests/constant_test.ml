open KNormal

(* let v1 = 1 in ( let v2 = 2 in v1 + v2 ) *)
let case1 () =
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
  assert(constant_ed = 
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
        Int 3))) (* variables in addition folded w/ result *)

(* let rec f x = (let y = ( let v1 = 1 in ( let v2 = 2 in v1 + v2 ) ) in x + y) in (let x = 2 in f x)*)
let case2 () =
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
          ), Unit
        )
      },
      Let (
        ( "x", Type.Var (ref (Some Type.Int)) ),
        Int 2,
        App( "f", ["x"])
      )
    ) in
  let constant_ed = Constant.f knormed in
  assert(constant_ed = 
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
              Int 3 (* same thing happen in body of functions *)
            )
          ), Unit
        )
      },
      Let (
        ( "x", Type.Var (ref (Some Type.Int)) ),
        Int 2,
        App( "f", ["x"])
      )
    ))

let () =
  print_string "Constant folding tests... ";
  case1 ();
  case2 ();
  print_endline "passed"
