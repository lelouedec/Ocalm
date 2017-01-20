open KNormal

(* Threshold is set by user *)
let threshold = ref 7

(* Helper function: Inline.size to compute the size of expression *)
let rec size exp = 
  match exp with
  | IfEq (id1, id2, e1, e2) -> 1 + size e1 + size e2
  | IfLE (id1, id2, e1, e2) -> 1 + size e1 + size e2
  | Let ((id, t), e1, e2) -> 1 + size e1 + size e2
  | LetRec ({ body = e1 }, e2) -> 1 + size e1 + size e2
  | LetTuple (l, e1, e2) -> 1 + size e2
  | _ -> 1

let rec g exp vars = 
  match exp with 
  | IfEq (id1, id2, e1, e2) -> IfEq (id1, id2, g e1 vars , g e2 vars)
  | IfLE (id1, id2, e1, e2) -> IfLE (id1, id2, g e1 vars, g e2 vars)
  | Let ((id, t), e1, e2) -> Let ((id, t), g e1 vars, g e2 vars)
  | LetRec ({ name = (label, t); args = args; body = body }, e) -> 
    let new_vars = 
    (
      match size body - !threshold with
      | z when (z <= 0) -> St.add label (args, body) vars
      | _ -> vars
    ) in
    LetRec ({ name = (label, t); args = args; body = g body new_vars}, g e new_vars)
  | App (e1, le2) when St.mem e1 vars -> 
    let (formal_args, e) = St.find e1 vars in
    let vars' =
      List.fold_right2
        (fun (z, t) y vars' -> St.add z y vars')
        formal_args
        le2
        St.empty in
    Alpha.g e vars'
  (* | LetTuple (l, e1, e2) -> LetTuple (l, e1, g e2 vars) *)
  | e -> e

let rec f exp = g exp St.empty