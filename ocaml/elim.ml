open KNormal

let rec side_effect exp =
  match exp with
  | IfEq (id1, id2, e1, e2) -> side_effect e1 || side_effect e2
  | IfLE (id1, id2, e1, e2) -> side_effect e1 || side_effect e2
  | Let ((id, t), e1, e2) -> side_effect e1 || side_effect e2
  | LetRec ({name = (label, t); args = args; body = body}, e) -> side_effect e
  | AppExt (id, args) -> true
  | App (id, args) -> true (* cannot know if body of the function cause side effects... *)
  | _ -> false

let rec present exp i = 
  match exp with
  | Not id | Neg id | FNeg id -> i = id
  | Add (id1, id2) | Sub (id1, id2) | FAdd (id1, id2) | FSub (id1, id2) | FMul (id1, id2) | FDiv (id1, id2) | Eq (id1, id2) | LE (id1, id2) | Get (id1, id2) -> i = id1 || i = id2
  | IfEq (id1, id2, e1, e2) -> present e1 i || present e2 i
  | IfLE (id1, id2, e1, e2) -> present e1 i || present e2 i
  | Let ((id, t), e1, e2) -> present e1 i || present e2 i
  | LetRec ({name = (label, t); args = args; body = body}, e) -> present body i || present e i || List.mem i (List.map (fun (x, y) -> x) args)
  | App (id, args) | AppExt (id, args) -> i = id || (List.mem i args)
  | Var (id) -> i = id
  | _ -> false

let rec f exp =
  match exp with
  | IfEq (id1, id2, e1, e2) -> IfEq(id1, id2, f e1, f e2)
  | IfLE (id1, id2, e1, e2) -> IfLE(id1, id2, f e1, f e2)
  | Let ((id, t), e1, e2) ->
    let el1 = f e1 in
    let el2 = f e2 in
    if (side_effect el1 || present el2 id) then 
      Let ((id, t), el1, el2)
    else (
      el2 )
  | LetRec ({name = (label, t); args = args; body = body}, e) -> 
    let el = f e in
      if (present el label) then
      LetRec ({name = (label, t); args = args; body = f body}, el)
    else (
      el )
  (*| LetTuple (l, e1, e2)-> *)
  | _ -> exp
