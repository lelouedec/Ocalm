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
  | AppCls (id, args) -> Asml.CallClo(id, args)
  | AppDir (id, args) -> Asml.CallLabel(id, args)
  | _ -> Asml.Nop

let rec tr cls = 
  match cls with
  | Unit -> Asml.Exp (Asml.Nop)
  (*| Float f -> *)
  | Let ((id, t), e1, e2) -> Asml.LetIdentEq (id, make_exp e1, tr e2)
  | Var id -> Asml.Exp (make_exp cls)
  | _ -> Asml.Exp (make_exp cls)

let f cls =
  let cls_fns, cls_main = cls in
  let rec transform functions =
    match functions with
    | [] -> Asml.LetUnderscEQ (tr cls_main)
    | hd :: tl ->
      (* assume no float function *)
      let ((fname, ftype), args, fargs, body) = hd in
      let asml_args =
        (List.map fst args)
        @ (List.map fst fargs) in
      let asml_body = tr body in
      Asml.LetLabelEq (fname, asml_args, asml_body, transform tl) in
  transform cls_fns
