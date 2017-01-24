open Closure

let word_size = 4

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
  | IfEq (id1, id2, e1, e2) -> Asml.IfEq(id1, Asml.Ident(id2), Asml.Exp(make_exp e1), Asml.Exp(make_exp e2))
  | IfLE (id1, id2, e1, e2) -> Asml.IfLEq(id1, Asml.Ident(id2), Asml.Exp(make_exp e1), Asml.Exp(make_exp e2))
  | Let ((id, t), e1, e2) -> make_exp(e1)
  | Var id | Array id -> Asml.Ident(id)
  | AppCls (id, args) -> Asml.CallClo(id, args)
  | AppDir (id, args) -> Asml.CallLabel(id, args)
  | _ -> Asml.Nop

let rec tr cls current_fn =
  match cls with
  | Unit -> Asml.Exp (Asml.Nop)
  (*| Float f -> *)
  | IfEq (id1, id2, e1, e2) -> Asml.Exp (Asml.IfEq(id1, Asml.Ident(id2), tr e1 current_fn, tr e2 current_fn))
  | IfLE (id1, id2, e1, e2) -> Asml.Exp (Asml.IfLEq(id1, Asml.Ident(id2), tr e1 current_fn, tr e2 current_fn))
  | Let ((id, t), e1, e2) -> Asml.LetIdentEq (id, make_exp e1, tr e2 current_fn)
  | MakeCls ((id, t), label, free_vars, e) ->
    let size = List.length free_vars in
    let addr_id = "addr_" ^ label in
    let rec alloc_mem_for_free_vars free_vars ?(counter = 1) fbody =
      match free_vars with
      | [] -> tr fbody current_fn
      | hd :: tl ->
        (* TODO a more general case: any function label as argument for others *)
        if hd = "%self" then (
          let newid = Id.gen_asml_id () in
          let newaddr = "addr_" ^ newid in
          Asml.LetIdentEq (
            newid,
            Asml.New (Asml.Int word_size),
            Asml.LetIdentEq (
              newaddr,
              Asml.Label (current_fn),
              Asml.LetIdentEq (
                Id.gen_asml_id (),
                Asml.St (newid, Asml.Int 0, newaddr),
                Asml.LetIdentEq (
                  Id.gen_asml_id (),
                  Asml.St (id, Asml.Int (counter * word_size), newid),
                  alloc_mem_for_free_vars tl ~counter:(counter + 1) fbody
                )
              )
            )
          )  
        ) else
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
  | _ -> Asml.Exp (make_exp cls)

let rec generate_fun_body fname fargs ?(counter = 1) fbody =
  match fargs with
  | [] -> tr fbody fname
  | hd :: tl ->
    (* function with free vars -- closure,
      have to load env values from heap to assign free vars *)
    let x = fst hd in
    Asml.LetIdentEq (
      x,
      Asml.Ld ("%self", Asml.Int (counter * word_size)),
      generate_fun_body fname tl ~counter:(counter + 1) fbody
    )


let f cls =
  let cls_fns, cls_main = cls in
  let rec transform functions =
    match functions with
    | [] -> Asml.LetUnderscEQ (tr cls_main "")
    | hd :: tl ->
      (* assume no float function *)
      let ((fname, ftype), args, fargs, fbody) = hd in
      let asml_args =
        (List.map fst args) in
      let asml_body = generate_fun_body fname fargs fbody in
      Asml.LetLabelEq (fname, asml_args, asml_body, transform tl) in
  transform cls_fns
