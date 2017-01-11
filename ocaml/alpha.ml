open KNormal

let lookup id vars =
  try St.find id vars
  with e -> id

let rec get_fresh2 id1 id2 vars =
  let newid1 = lookup id1 vars in 
  let newid2 = lookup id2 vars in
  (newid1, newid2)

let rec g (exp : t) (vars : Id.t St.t) : t =
  match exp with
  | Unit -> exp
  | Int i -> exp
  | Float f -> exp
  | Not id -> 
    let newid = lookup id vars in Not (newid)
  | Neg id -> 
    let newid = lookup id vars in Neg (newid)
  | Add (id1, id2) -> let (n1, n2) = get_fresh2 id1 id2 vars in Add(n1, n2)
  | Sub (id1, id2) -> let (n1, n2) = get_fresh2 id1 id2 vars in Sub(n1, n2)
  | FNeg id -> let newid = lookup id vars in FNeg (newid)
  | FAdd (id1, id2) -> let (n1, n2) = get_fresh2 id1 id2 vars in FAdd(n1, n2)
  | FSub (id1, id2) -> let (n1, n2) = get_fresh2 id1 id2 vars in FSub(n1, n2)
  | FMul (id1, id2) -> let (n1, n2) = get_fresh2 id1 id2 vars in FMul(n1, n2)
  | FDiv (id1, id2) -> let (n1, n2) = get_fresh2 id1 id2 vars in FDiv(n1, n2)
  | Eq (id1, id2) -> let (n1, n2) = get_fresh2 id1 id2 vars in Eq(n1, n2)
  | LE (id1, id2) -> let (n1, n2) = get_fresh2 id1 id2 vars in LE(n1, n2)
  | IfEq (id1, id2, e1, e2) -> 
    let (n1, n2) = get_fresh2 id1 id2 vars in IfEq(n1, n2, g e1 vars, g e2 vars)
  | IfLE (id1, id2, e1, e2) -> 
    let (n1, n2) = get_fresh2 id1 id2 vars in IfLE(n1, n2, g e1 vars, g e2 vars)
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
