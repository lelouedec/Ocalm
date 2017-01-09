open KNormal

module Vars = Map.Make(String)

let lookup id vars =
  try Vars.find id vars
  with e -> id

let rec g (exp : t) (vars : string Vars.t) : t =
  print_endline ("match : " ^ to_s exp);
  match exp with
  | Unit | Bool _ | Int _ | Float _ -> exp
  | Add (e1, e2) -> Add (lookup e1 vars, lookup e2 vars)
  | Let ((id, t), e1, e2) ->
    let e1' = g e1 vars
    in (
      match e1' with
      (* e1 is a single variable *)
      | Var (id') -> g e2 (Vars.add id id' vars)
      | _ -> failwith "nyi"
    )
  | Var id -> Var (lookup id vars)
  | _ -> failwith "dunno"

let rec f (exp : t) : t = g exp Vars.empty
