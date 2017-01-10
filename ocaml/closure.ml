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
  | AppCls of Id.t * Id.t list
  | AppDir of Id.t * Id.t list
  (* to be added *)

type let_fn = (Id.t * Type.t) * (Id.t * Type.t) list * t
type let_main = t
  
type prog = let_fn list * let_main

let rec to_string (prog : prog) = "to be impl for testing"

let functions : let_fn list ref = ref []

let rec extract_main (exp : KNormal.t) : t =
  match exp with
  | KNormal.Unit -> Unit
  | KNormal.Bool b -> Bool b
  | KNormal.Int i -> Int i
  | KNormal.Float f -> Float f
  | KNormal.Neg id -> Neg id
  | KNormal.Add (id1, id2) -> Add (id1, id2)
  | KNormal.Let ((id, t), e1, e2) -> Let ((id, t), extract_main e1, extract_main e2)
  | KNormal.LetRec (fn, e) ->
    let (fname, fargs, fbody) = KNormal.denormalize fn in
    let split_fn = (fname, fargs, extract_main fbody) in
    functions := [split_fn] @ !functions;
    extract_main e
  | _ -> failwith "nyi" 

let rec f (exp : KNormal.t) : prog =
  functions := [];
  let main_body = extract_main exp in
  (!functions, main_body)
