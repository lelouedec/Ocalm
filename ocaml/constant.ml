open KNormal

(* check identifier type for int, float or tuple  *)
let is_int id vars =
  try (
    let type_int = St.find id vars
    in ( 
      match type_int with 
      | Int i -> true 
      | _ -> false
    )
  )
  with e -> false
  
let is_float id vars =
  try (
    let type_float = St.find id vars
    in ( 
      match type_float with 
      | Float f -> true 
      | _ -> false
    )
  )
  with e -> false

let is_tuple id vars =
  try (
    let type_tuple = St.find id vars
    in ( 
      match type_tuple with 
      | Tuple t  -> true 
      | _ -> false
    )
  )
  with e -> false

(* Find variable value in the mapping *)
let find_int id vars = 
  let val_int = St.find id vars 
  in (
    match val_int with 
    | Int i -> i 
    | _ -> failwith "Value not found"
  )
  
let find_float id vars = 
  let val_float = St.find id vars 
  in (
    match val_float with 
    | Float f -> f 
    | _ -> failwith "Value not found"
  )

let find_tuple id vars = 
  let val_tuple = St.find id vars
  in (
    match val_tuple with 
    | Tuple l -> l 
    | _ -> failwith "Value not found"
  )

let rec g exp vars = 
  match exp with 
  | Add (id1, id2) when is_int id1 vars && is_int id2 vars -> Int (find_int id1 vars + find_int id2 vars)
  | Sub (id1, id2) when is_int id1 vars && is_int id2 vars -> Int (find_int id1 vars - find_int id2 vars)
  | Neg id when is_int id vars -> Int (-(find_int id vars))
  | FAdd (id1, id2) when is_float id1 vars && is_float id2 vars -> Float (find_float id1 vars +. find_float id2 vars)
  | FSub (id1, id2) when is_float id1 vars && is_float id2 vars -> Float (find_float id1 vars -. find_float id2 vars)
  | FNeg x when is_float x vars -> Float (-.(find_float x vars))
  | FMul (id1, id2) when is_float id1 vars && is_float id2 vars -> Float (find_float id1 vars *. find_float id2 vars)
  | FDiv (id1, id2) when is_float id1 vars && is_float id2 vars -> Float (find_float id1 vars /. find_float id2 vars)
  | Var id when is_int id vars -> Int (find_int id vars)
  | Var id when is_float id vars -> Float (find_float id vars)
  | Var id when is_tuple id vars -> Tuple (find_tuple id vars)
  | IfEq (id1, id2, e1, e2) when is_int id1 vars && is_int id2 vars -> if find_int id1 vars = find_int id2 vars then g e1 vars else g e2 vars
  | IfEq (id1, id2, e1, e2) when is_float id1 vars && is_float id2 vars -> if find_float id1 vars = find_float id2 vars then g e1 vars else g e2 vars
  | IfLE (id1, id2, e1, e2) when is_int id1 vars && is_int id2 vars -> if find_int id1 vars <= find_int id2 vars then g e1 vars else g e2 vars
  | IfLE (id1, id2, e1, e2) when is_float id1 vars && is_float id2 vars -> if find_float id1 vars <= find_float id2 vars then g e1 vars else g e2 vars
  | Let ((id, t), e1, e2) -> 
    let new_e1 = g e1 vars in
    let new_e2 = g e2 (St.add id new_e1 vars) in
    Let ((id, t), new_e1, new_e2)
  | LetRec ({ name = (label, t); args = args; body = body }, e) ->
      LetRec ({ name = (label, t); args = args; body = g body vars }, g e vars)
  (* | LetTuple (l, e1, e2) when is_tuple e1 vars ->
      List.fold_right2
        (fun xt z e' -> Let (xt, Var (z), e'))
        l
        (find_tuple e1 vars)
        (g e2 vars)
  | LetTuple (l, e1, e2) -> LetTuple (l, e1, g e2 vars) *)
  | _ -> exp

let rec f exp = g exp St.empty
