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
      let vars = if size body > !threshold then vars else St.add label (args, body) vars in
      LetRec ({ name = (label, t); args = args; body = g body vars}, g e vars)
  | App (x, ys) when St.mem x vars -> 
      let (zs, e) = St.find x vars in
      (* Format.eprintf "inlining %s@." x; *)
      let vars' =
        List.fold_left2
          (fun vars' (z, t) y -> St.add z y vars')
          St.empty
          zs
          ys in
      Alpha.g e vars'
  (* | LetTuple (l, e1, e2) -> LetTuple (l, e1, g e2 vars) *)
  | e -> e

let rec f exp = g exp St.empty