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

type fn_args =
  | Ident of Id.t * fn_args
  | Nil
  
type fundef = 
  | LetFn of Id.t * fn_args * t * fundef
  | LetMain of t

let rec f (exp : KNormal.t) : fundef =
  LetMain ( Unit )
