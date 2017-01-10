open Printf

type t =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of Id.t
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | Eq of Id.t * Id.t
  | LE of Id.t * Id.t
  | If of Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Array of Id.t * Id.t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let insert_let (e, t) k =
  match e with
  | Var x -> k x
  | _ ->
      let x = Id.genid () in let e', t' = k x in
      Typing.st := St.add x t !Typing.st;
      Let ((x, t), e, e'), t'

let rec temporaries exp =
  match exp with
  | Syntax.Unit -> (Unit, Type.Unit)
  | Syntax.Bool b -> (Bool (b), Type.Bool)
  | Syntax.Int i -> (Int (i), Type.Int)
  | Syntax.Float f -> (Float (f), Type.Float)
  (*| Syntax.Not e -> 
  | Syntax.Neg e -> *)
  | Syntax.Add (e1, e2) ->
      insert_let (temporaries e1)
        (fun x -> insert_let (temporaries e2)
          (fun y -> Add (x, y), Type.Int))
  | Syntax.Sub (e1, e2) -> 
      insert_let (temporaries e1)
        (fun x -> insert_let (temporaries e2)
          (fun y -> Sub (x, y), Type.Int))
  | Syntax.FNeg e -> 
      insert_let (temporaries e)
        (fun x -> FNeg (x), Type.Float)
  | Syntax.FAdd (e1, e2) -> 
      insert_let (temporaries e1)
        (fun x -> insert_let (temporaries e2)
          (fun y -> FAdd (x, y), Type.Float))
  | Syntax.FSub (e1, e2) -> 
      insert_let (temporaries e1)
        (fun x -> insert_let (temporaries e2)
          (fun y -> FSub (x, y), Type.Float))
  | Syntax.FMul (e1, e2) -> 
      insert_let (temporaries e1)
        (fun x -> insert_let (temporaries e2)
          (fun y -> FMul (x, y), Type.Float))
  | Syntax.FDiv (e1, e2) -> 
      insert_let (temporaries e1)
        (fun x -> insert_let (temporaries e2)
          (fun y -> FDiv (x, y), Type.Float))
  (*| Syntax.Eq (e1, e2) -> 
  | Syntax.LE (e1, e2) ->   
  | Syntax.If (e1, e2, e3) -> *)
  | Syntax.Let ((id,t), e1, e2) ->
      let e1', t1 = temporaries e1 in
      let e2', t2 = temporaries e2 in
      Let ((id, t), e1', e2'), t2
  | Syntax.Var (id) when St.mem id !Typing.st -> Var (id), St.find id !Typing.st
  (*| Syntax.App (e1, le2) -> 
  | Syntax.LetRec (fd, e) -> 
  | Syntax.LetTuple (l, e1, e2)-> 
  | Syntax.Get (e1, e2) -> 
  | Syntax.Put (e1, e2, e3) -> 
  | Syntax.Tuple (l) -> 
  | Syntax.Array (e1, e2) -> *)
  | _ -> (Unit, Type.Int)

let rec to_string exp =
    match exp with
  | Unit -> "()\n"
  | Bool b -> if b then "true\n" else "false\n"
  | Int i -> string_of_int i
  | Float f -> sprintf "%.2f\n" f
  | Not id -> sprintf "(not %s)" (Id.to_string id)
  | Neg id -> sprintf "(- %s)" (Id.to_string id)
  | Add (id1, id2) -> sprintf "(%s + %s)" (Id.to_string id1) (Id.to_string id2)
  | Sub (id1, id2) -> sprintf "(%s - %s)" (Id.to_string id1) (Id.to_string id2)
  | FNeg id -> sprintf "(-. %s)" (Id.to_string id)
  | FAdd (id1, id2) -> sprintf "(%s +. %s)" (Id.to_string id1) (Id.to_string id2)
  | FSub (id1, id2) -> sprintf "(%s -. %s)" (Id.to_string id1) (Id.to_string id2)
  | FMul (id1, id2) -> sprintf "(%s *. %s)" (Id.to_string id1) (Id.to_string id2)
  | FDiv (id1, id2) -> sprintf "(%s /. %s)" (Id.to_string id1) (Id.to_string id2)
  | Eq (id1, id2) -> sprintf "(%s = %s)" (Id.to_string id1) (Id.to_string id2)
  | Let ((id, t), e1, e2) ->
          sprintf "(let (%s : %s) = %s in %s)\n" (Id.to_string id) (Type.to_string t) (to_string e1) (to_string e2)
  | Var id -> Id.to_string id
  | _ -> "to be defined"

let f exp =
  let n, _ = temporaries exp in
  n
