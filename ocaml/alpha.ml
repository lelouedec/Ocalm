open KNormal

module ToReplaceVars = Map.Make(String)

let lookup id repl_vars =
  try ToReplaceVars.find id repl_vars
  with e -> id

let rec g (exp : t) (vars : string list) (repl_vars : string ToReplaceVars.t) : t =
  match exp with
  | Unit -> exp
  | Bool b -> exp
  | Int i -> exp
  | Float f -> exp
  | Add (id1, id2) ->
    let newid1 = lookup id1 repl_vars in
    let newid2 = lookup id2 repl_vars in
    Add (newid1, newid2)
  | Sub (id1, id2) ->
    let newid1 = lookup id1 repl_vars in
    let newid2 = lookup id2 repl_vars in
    Sub (newid1, newid2)
  | Let ((id, t), e1, e2) ->
    if List.mem id vars then
      let newid = Id.genid () in
      let vars = vars @ [newid] in
      let repl_vars = ToReplaceVars.add id newid repl_vars in
      Let (
        (newid, t),
        g e1 vars repl_vars,
        g e2 vars repl_vars
      )
    else
      let vars = vars @ [id] in
      Let (
        (id, t),
        g e1 vars repl_vars,
        g e2 vars repl_vars
      )
  | Var id ->
    let newid = lookup id repl_vars
    in Var (newid)
  | _ -> failwith "undef"

let rec f (exp : t) : t = g exp [] ToReplaceVars.empty
