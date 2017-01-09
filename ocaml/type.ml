type t =
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t
  | Tuple of t list
  | Array of t
  | Var of t option ref

let gentyp () = Var(ref None)

let rec to_string = function
  | Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Float -> "float"
  | Fun (l, t) -> "()"
  | Tuple l -> "<tuple>"
  | Array t -> "<array>"
  | Var r -> match !r with
    | Some t -> to_string t
    | _ -> "<undef>"
