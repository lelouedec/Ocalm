type t =
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t
  | Tuple of t list
  | Array of t
  | Var of t option ref

let typenum = ref 0
let gentyp () = Var(ref None)

let rec to_string = function
  | Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Float -> "float"
  | Fun (l, t) ->
    Printf.sprintf "(%s -> %s)"
      (
        match l with
        | [] -> "unit"
        | _ -> String.concat " -> " (List.map (fun t -> to_string t) l)
      )
      (to_string t)
  | Tuple l -> 
    Printf.sprintf "(%s) tuple"
      (String.concat "," (List.map (fun t -> (to_string t)) l))
  | Array t -> (to_string t) ^ " array"
  | Var r -> match !r with
    | Some t -> to_string t
    | _ -> "<undef>"
