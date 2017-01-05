open Printf

type t =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec infix_to_string (to_s : 'a -> string) (l : 'a list) (op : string) : string =
    match l with
    | [] -> ""
    | [x] -> to_s x
    | hd :: tl -> (to_s hd) ^ op ^ (infix_to_string to_s tl op)

let rec to_string exp =
    match exp with
  | Unit -> "()"
  | Bool b -> if b then "true" else "false"
  | Int i -> string_of_int i
  | Float f -> sprintf "%.2f" f
  | Not e -> sprintf "(not %s)" (to_string e)
  | Neg e -> sprintf "(- %s)" (to_string e)
  | Add (e1, e2) -> sprintf "(%s + %s)" (to_string e1) (to_string e2)
  | Sub (e1, e2) -> sprintf "(%s - %s)" (to_string e1) (to_string e2)
  | FNeg e -> sprintf "(-. %s)" (to_string e)
  | FAdd (e1, e2) -> sprintf "(%s +. %s)" (to_string e1) (to_string e2)
  | FSub (e1, e2) -> sprintf "(%s -. %s)" (to_string e1) (to_string e2)
  | FMul (e1, e2) -> sprintf "(%s *. %s)" (to_string e1) (to_string e2)
  | FDiv (e1, e2) -> sprintf "(%s /. %s)" (to_string e1) (to_string e2)
  | Eq (e1, e2) -> sprintf "(%s = %s)" (to_string e1) (to_string e2)
  | LE (e1, e2) -> sprintf "(%s <= %s)" (to_string e1) (to_string e2)
  | If (e1, e2, e3) ->
          sprintf "(if %s then %s else %s)" (to_string e1) (to_string e2) (to_string e3)
  | Let ((id,t), e1, e2) ->
          sprintf "(let %s = %s in %s)" (Id.to_string id) (to_string e1) (to_string e2)
  | Var id -> Id.to_string id
  | App (e1, le2) -> sprintf "(%s %s)" (to_string e1) (infix_to_string to_string le2 " ")
  | LetRec (fd, e) ->
          sprintf "(let rec %s %s = %s in %s)"
          (let (x, _) = fd.name in (Id.to_string x))
          (infix_to_string (fun (x,_) -> (Id.to_string x)) fd.args " ")
          (to_string fd.body)
          (to_string e)
  | LetTuple (l, e1, e2)->
          sprintf "(let (%s) = %s in %s)"
          (infix_to_string (fun (x, _) -> Id.to_string x) l ", ")
          (to_string e1)
          (to_string e2)
  | Get(e1, e2) -> sprintf "%s.(%s)" (to_string e1) (to_string e2)
  | Put(e1, e2, e3) -> sprintf "(%s.(%s) <- %s)"
                 (to_string e1) (to_string e2) (to_string e3)
  | Tuple(l) -> sprintf "(%s)" (infix_to_string to_string l ", ")
  | Array(e1,e2) -> sprintf "(Array.create %s %s)"
       (to_string e1) (to_string e2)

let rec height = function
  | Unit -> 0
  | Bool b -> 0
  | Int i -> 0
  | Float f -> 0
  | Not e -> height e + 1
  | Neg e -> height e + 1
  | Add (e1, e2) -> max (height e1) (height e2) + 1
  | Sub (e1, e2) -> max (height e1) (height e2) + 1
  | FNeg e -> height e + 1
  | FAdd (e1, e2) -> max (height e1) (height e2) + 1
  | FSub (e1, e2) -> max (height e1) (height e2) + 1
  | FMul (e1, e2) -> max (height e1) (height e2) + 1
  | FDiv (e1, e2) -> max (height e1) (height e2) + 1
  | Eq (e1, e2) -> max (height e1) (height e2) + 1
  | LE (e1, e2) -> max (height e1) (height e2) + 1
  | If (e1, e2, e3) ->
          max (height e1) (max (height e2) (height e3)) + 1
  | Let ((id,t), e1, e2) ->
          max (height e1) (height e2) + 1
  | Var id -> 0
  (*
  | App (e1, le2) -> sprintf "(%s %s)" (to_string e1) (infix_to_string to_string le2 " ")
  | LetRec (fd, e) ->
          sprintf "(let rec %s %s = %s in %s)"
          (let (x, _) = fd.name in (Id.to_string x))
          (infix_to_string (fun (x,_) -> (Id.to_string x)) fd.args " ")
          (to_string fd.body)
          (to_string e)
  | LetTuple (l, e1, e2)->
          sprintf "(let (%s) = %s in %s)"
          (infix_to_string (fun (x, _) -> Id.to_string x) l ", ")
          (to_string e1)
          (to_string e2)
  | Get(e1, e2) -> sprintf "%s.(%s)" (to_string e1) (to_string e2)
  | Put(e1, e2, e3) -> sprintf "(%s.(%s) <- %s)"
                 (to_string e1) (to_string e2) (to_string e3)
  | Tuple(l) -> sprintf "(%s)" (infix_to_string to_string l ", ")
  | Array(e1,e2) -> sprintf "(Array.create %s %s)"
       (to_string e1) (to_string e2)  *)
  | _ -> 0

(* Sort only when the recursive call ends *)
let rec get_vars = function
  | Not e -> get_vars e
  | Neg e -> get_vars e
  | Add (e1, e2) -> get_vars e1 @ get_vars e2
  | Sub (e1, e2) -> get_vars e1 @ get_vars e2
  | FNeg e -> get_vars e
  | FAdd (e1, e2) -> get_vars e1 @ get_vars e2
  | FSub (e1, e2) -> get_vars e1 @ get_vars e2
  | FMul (e1, e2) -> get_vars e1 @ get_vars e2
  | FDiv (e1, e2) -> get_vars e1 @ get_vars e2
  | Eq (e1, e2) -> get_vars e1 @ get_vars e2
  | LE (e1, e2) -> get_vars e1 @ get_vars e2
  | If (e1, e2, e3) -> get_vars e1 @ get_vars e2 @ get_vars e3
  | Let ((id,t), e1, e2) ->
      [Id.to_string id] @ get_vars e1 @ get_vars e2
  | Var id -> [Id.to_string id]
  | _ -> []
