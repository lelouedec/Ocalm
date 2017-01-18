open Closure

let rec make_exp cls =
  match cls with
  | Unit -> Asml.Nop
  | Int i -> Asml.Int(i)
  (*| Float f -> *)
  | Neg id -> Asml.Neg(id)
  | Add (id1, id2) -> Asml.Add(id1, Asml.Ident(id2))
  | Sub (id1, id2) -> Asml.Sub(id1, Asml.Ident(id2))
  | FNeg id -> Asml.FNeg(id)
  | FAdd (id1, id2) -> Asml.FAdd(id1, id2)
  | FSub (id1, id2) -> Asml.FSub(id1, id2)
  | FMul (id1, id2) -> Asml.FMul(id1, id2)
  | FDiv (id1, id2) -> Asml.FDiv(id1, id2)
  | IfEq (id1, id2, e1, e2) -> Asml.IfEq(id1, Asml.Ident(id2), make_exp(e1), make_exp(e2))
  | IfLE (id1, id2, e1, e2) -> Asml.IfLEq(id1, Asml.Ident(id2), make_exp(e1), make_exp(e2))
  | Let ((id, t), e1, e2) -> make_exp(e1)
  | Var id -> Asml.Ident(id)
  (*| AppCls (id, args) -> *)
  | AppDir (id, args) -> Asml.CallLabel(id, args)
  | _ -> Asml.Nop

let rec tr cls = 
  match cls with
  | Unit -> Asml.Exp (Asml.Nop)
  | Int i -> Asml.Exp (make_exp cls)
  (*| Float f -> *)
  | Neg id -> Asml.Exp (make_exp cls)
  | Add (id1, id2) -> Asml.Exp (make_exp cls)
  | Sub (id1, id2) -> Asml.Exp (make_exp cls)
  | FNeg id -> Asml.Exp (make_exp cls)
  | FAdd (id1, id2) -> Asml.Exp (make_exp cls)
  | FSub (id1, id2) -> Asml.Exp (make_exp cls)
  | FMul (id1, id2) -> Asml.Exp (make_exp cls)
  | FDiv (id1, id2) -> Asml.Exp (make_exp cls)
  | IfEq (id1, id2, e1, e2) -> Asml.Exp (make_exp cls)
  | IfLE (id1, id2, e1, e2) -> Asml.Exp (make_exp cls)
  | Let ((id, t), e1, e2) -> Asml.LetIdentEq (id, make_exp e1, tr e2)
  | Var id -> Asml.Exp (make_exp cls)
  (*| AppCls (id, args) -> *)
  | AppDir (id, args) -> Asml.Exp (make_exp cls)
  | _ -> Asml.Exp (Asml.Nop)

let f cls =
  Asml.LetUnderscEQ ( tr (cls) )
