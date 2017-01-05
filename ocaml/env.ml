open Syntax

module E = 
  Set.Make(
    struct
      type t = Id.t
      let compare = compare
    end)
include E

let rec env_list exp = 
  match exp with
  | Not e -> env_list e
  | Neg e -> env_list e
  | Add (e1, e2) -> env_list e1 @ env_list e2
  | Sub (e1, e2) -> env_list e1 @ env_list e2
  | FNeg e -> env_list e
  | FAdd (e1, e2) -> env_list e1 @ env_list e2
  | FSub (e1, e2) -> env_list e1 @ env_list e2
  | FMul (e1, e2) -> env_list e1 @ env_list e2
  | FDiv (e1, e2) -> env_list e1 @ env_list e2
  | Eq (e1, e2) -> env_list e1 @ env_list e2
  | LE (e1, e2) -> env_list e1 @ env_list e2
  | If (e1, e2, e3) -> env_list e1 @ env_list e2 @ env_list e3
  | Let ((id,t), e1, e2) -> [id] @ env_list e1 @ env_list e2
  | Var id -> [id]
  | App (e1, e2) -> env_list e1
  | LetRec (fd, e) -> 
    let (x, _) = fd.name in [x] @ 
    List.fold_left (fun l2 e -> l2 @ let (x, _) = e in [x]) [] fd.args @ env_list fd.body @ env_list e
  | LetTuple (l, e1, e2)-> List.fold_left (fun l2 e -> l2 @ let (x, _) = e in [x]) [] l @ env_list e1 @ env_list e2
  | Get (e1, e2) -> env_list e1 @ env_list e2
  | Put (e1, e2, e3) -> env_list e1 @ env_list e2 @ env_list e3
  | Tuple (l) -> List.fold_left (fun l2 e -> l2 @ env_list e) [] l
  | Array (e1, e2) -> env_list e1 @ env_list e2
  | _ -> []

let get_env exp = List.fold_left (fun e l -> add l e) empty (env_list exp)
