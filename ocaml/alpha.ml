open KNormal

let lookup id vars =
  try St.find id vars
  with e -> id

let rec g (exp : t) (vars : Id.t St.t) : t =
  match exp with
  | Unit -> exp
  | Int i -> exp
  | Float f -> exp
  | Not id -> 
    let newid = lookup id vars in
    Not (newid)
  | Neg id -> 
    let newid = lookup id vars in
    Neg (newid)
  | Add (id1, id2) ->
    let newid1 = lookup id1 vars in
    let newid2 = lookup id2 vars in
    Add (newid1, newid2)
  | Sub (id1, id2) ->
    let newid1 = lookup id1 vars in
    let newid2 = lookup id2 vars in
    Sub (newid1, newid2)
  | Let ((id, t), e1, e2) ->
    let newid = Id.genid () in
    let new_vars = St.add id newid vars in
    Let (
      (newid, t),
      g e1 vars,
      g e2 new_vars
    )
  | Var id ->
    let newid = lookup id vars
    in Var (newid)
  | _ -> failwith "undef"

let rec f (exp : t) : t = g exp St.empty
