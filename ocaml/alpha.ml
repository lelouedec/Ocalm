open KNormal

module ToReplaceVars = Map.Make(String)


let rec g (exp : t) (vars : string list) (repl_vars : string ToReplaceVars.t) : t =
  match exp with
  | Unit -> exp
  | Bool b -> exp
  | Int i -> exp
  | Float f -> exp
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
    let newid = if ToReplaceVars.mem id repl_vars then
      ToReplaceVars.find id repl_vars
    else
      id
    in Var (newid)
  | _ -> failwith "undef"

let rec f (exp : t) : t = g exp [] ToReplaceVars.empty
