open Closure

let word_size = 4

let rec tr cls =
  match cls with
  | Unit -> Asml.Exp (Asml.Nop)
  | Int i -> Asml.Exp (Asml.Int i)
  (*| Float f -> *)
  | Neg id -> Asml.Exp (Asml.Neg id)
  | Add (id1, id2) -> Asml.Exp (Asml.Add(id1, Asml.Ident(id2)))
  | Sub (id1, id2) -> Asml.Exp (Asml.Sub(id1, Asml.Ident(id2)))
  | FNeg id -> Asml.Exp (Asml.FNeg id)
  | FAdd (id1, id2) -> Asml.Exp (Asml.FAdd(id1, id2))
  | FSub (id1, id2) -> Asml.Exp (Asml.FSub(id1, id2))
  | FMul (id1, id2) -> Asml.Exp (Asml.FMul(id1, id2))
  | FDiv (id1, id2) -> Asml.Exp (Asml.FDiv(id1, id2))
  | IfEq (id1, id2, e1, e2) -> Asml.Exp (Asml.IfEq(id1, Asml.Ident(id2), tr e1, tr e2))
  | IfLE (id1, id2, e1, e2) -> Asml.Exp (Asml.IfLEq(id1, Asml.Ident(id2), tr e1, tr e2))
  | Let ((id, t), e1, e2) ->
    let e1' = tr e1 in
    let e1' = (
      match e1' with
      | Asml.Exp e -> e
      | _ -> Asml.Nop (* not sure *)
    ) in
    Asml.LetIdentEq (id, e1', tr e2)
  | Var id | Array id -> Asml.Exp (Asml.Ident(id))
  | AppCls (id, args) -> Asml.Exp (Asml.CallClo(id, args))
  | AppDir (id, args) -> Asml.Exp (Asml.CallLabel(id, args))
  | Get (id1, id2) -> Asml.Exp (Asml.Ld (id1, Asml.Ident(id2)))
  | Put (id1, id2, id3) -> Asml.Exp (Asml.St (id1, Asml.Ident(id2), id3))
  | MakeCls ((id, t), label, free_vars, e) ->
    let size = List.length free_vars in
    let addr_id = "addr_" ^ label in
    let rec alloc_mem_for_free_vars free_vars ?(counter = 1) fbody =
      match free_vars with
      | [] -> tr fbody
      | hd :: tl ->
        Asml.LetIdentEq (
          Id.gen_asml_id (),
          Asml.St (id, Asml.Int (counter * word_size), hd),
          alloc_mem_for_free_vars tl ~counter:(counter + 1) fbody
        ) in

    Asml.LetIdentEq (
      id,
      Asml.New (Asml.Int ((size + 1) * word_size)),
      Asml.LetIdentEq (
        addr_id,
        Asml.Label (label), (* or Ident, not sure for now *)
        Asml.LetIdentEq (
          Id.gen_asml_id (),
          Asml.St (id, Asml.Int (0), addr_id),
          alloc_mem_for_free_vars free_vars e
        )
      )
    )
  | _ -> Asml.Exp (Asml.Nop)

let rec generate_fun_body fargs ?(counter = 1) fbody =
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
