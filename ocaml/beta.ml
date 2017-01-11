open KNormal

let lookup id vars =
  try St.find id vars
  with e -> id

let rec g (exp : t) (vars : Id.t St.t) : t =
  match exp with
  | Unit | Int _ | Float _ -> exp
  | Not id -> Not (lookup id vars)
  | Neg id -> Neg (lookup id vars)
  | Add (id1, id2) -> Add (lookup id1 vars, lookup id2 vars)
  | Sub (id1, id2) -> Sub (lookup id1 vars, lookup id2 vars)
  | FNeg id -> FNeg (lookup id vars)
  | FAdd (id1, id2) -> FAdd (lookup id1 vars, lookup id2 vars)
  | FSub (id1, id2) -> FSub (lookup id1 vars, lookup id2 vars)
  | FMul (id1, id2) -> FMul (lookup id1 vars, lookup id2 vars)
  | FDiv (id1, id2) -> FDiv (lookup id1 vars, lookup id2 vars)
  | Let ((id, t), e1, e2) ->
    let e1' = g e1 vars
    in (
      match e1' with
      (* e1 is a single variable *)
      | Var (id') -> g e2 (St.add id id' vars)
      (* e1 is a complex expression *)
      | _ -> Let ((id, t), e1', g e2 vars)
    )
  | Var id -> Var (lookup id vars)
  | _ -> failwith "dunno"

let rec f (exp : t) : t = g exp St.empty