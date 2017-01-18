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
  | AppCls (id, args) -> Asml.Exp (make_exp cls)
  | AppDir (id, args) -> Asml.Exp (make_exp cls)
  | _ -> Asml.Exp (make_exp cls)

let rec generate_fun_body fargs ?(counter = 1) fbody =
  let word_size = 4 in
  match fargs with
  | [] -> tr fbody
  | hd :: tl ->
    (* function with free vars -- closure,
      have to load env values from heap to assign free vars *)
    let x = fst hd in
    Asml.LetIdentEq (
      x,
      Asml.Ld ("%self", Asml.Int (counter * word_size)),
      generate_fun_body tl ~counter:(counter + 1) fbody
    )


let f cls =
  let cls_fns, cls_main = cls in
  let rec transform functions =
    match functions with
    | [] -> Asml.LetUnderscEQ (tr cls_main)
    | hd :: tl ->
      (* assume no float function *)
      let ((fname, ftype), args, fargs, fbody) = hd in
      let asml_args =
        (List.map fst args) in
      let asml_body = generate_fun_body fargs fbody in
      Asml.LetLabelEq (fname, asml_args, asml_body, transform tl) in
  transform cls_fns
