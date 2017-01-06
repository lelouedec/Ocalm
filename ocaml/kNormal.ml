open Printf

(* type var_or_int =
  | Var of Id.t
  | Int of int

type var_or_bool =
  | Var of Id.t
  | Bool of bool

type var_or_float =
  | Var of Id.t
  | Float of float

type var_or_const =
  | Var of Id.t
  | Float of float
  | Int of int
  | Bool of bool
  | Unit *)

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
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of Id.t * Id.t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec to_s (exp : t) =
    match exp with
  | Unit -> "()"
  | Bool b -> if b then "true" else "false"
  | Int i -> string_of_int i
  | Float f -> sprintf "%.2f" f
  | Not id -> sprintf "(not %s)" (Id.to_string id)
  | Add (id1, id2) -> sprintf "(%s + %s)" (Id.to_string id1) (Id.to_string id2)
  | Eq (id1, id2) -> sprintf "(%s = %s)" (Id.to_string id1) (Id.to_string id2)
  | Let ((id, t), e1, e2) ->
          sprintf "(let (%s : %s) = %s in %s)" (Id.to_string id) (Type.to_s t) (to_s e1) (to_s e2)
  | Var id -> Id.to_string id
  | _ -> "to be defined"

let f (exp : Syntax.t) : t =
  Let (
    (
      "x",
      Type.Var (ref (Some Type.Int))
    ),
    Int(1),
    Var("x")
  )
