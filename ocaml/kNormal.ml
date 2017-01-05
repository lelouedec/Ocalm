type var_or_int =
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
  | Unit

type t =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of var_or_bool
  | Neg of var_or_int
  | Add of var_or_int * var_or_int
  | Sub of var_or_int * var_or_int
  | FNeg of var_or_float
  | FAdd of var_or_float * var_or_float
  | FSub of var_or_float * var_or_float
  | FMul of var_or_float * var_or_float
  | FDiv of var_or_float * var_or_float
  | Eq of var_or_const * var_or_const
  | LE of var_or_const * var_or_const
  | If of var_or_bool * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * var_or_const list
  | Tuple of var_or_const list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of var_or_int * var_or_const
  | Get of Id.t * var_or_int
  | Put of Id.t * var_or_int * var_or_const
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }
