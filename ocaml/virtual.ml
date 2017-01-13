open Closure

let rec f cls = 
  match cls with
  | Unit -> Asml.LPexpRp (Asml.Nop)
  | Int i -> Asml.LPexpRp (Asml.Int(i))
  (*| Float f -> 
  | Not id -> *)
  | Neg id -> Asml.LPexpRp (Asml.Neg(id))
  | Add (id1, id2) -> Asml.LPexpRp (Asml.Add(id1, Asml.Ident(id2)))
  | Sub (id1, id2) -> Asml.LPexpRp (Asml.Sub(id1, Asml.Ident(id2)))
  | FNeg id -> Asml.LPexpRp (Asml.FNeg(id))
  | FAdd (id1, id2) -> Asml.LPexpRp (Asml.FAdd(id1, id2))
  | FSub (id1, id2) -> Asml.LPexpRp (Asml.FSub(id1, id2))
  | FMul (id1, id2) -> Asml.LPexpRp (Asml.FMul(id1, id2))
  | FDiv (id1, id2) -> Asml.LPexpRp (Asml.FDiv(id1, id2))
  (*| Eq (id1, id2) -> 
  | LE (id1, id2) -> 
  | IfEq (id1, id2, e1, e2) -> 
  | IfLE (id1, id2, e1, e2) -> 
  | Let of ((id, t), e1, e2)
  | Var of id
  | AppCls (id, args) -> 
  | AppDir (id, args) -> *)
  | _ -> Asml.LPexpRp (Asml.Nop)

