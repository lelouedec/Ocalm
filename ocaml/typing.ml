open Syntax

let st = ref St.empty
let st_ext = ref St.empty

let rec generate exp t =
  match exp with
  | Unit -> [(Type.Unit, t)], Type.Unit
  | Bool (_) -> [(Type.Bool, t)], Type.Bool
  | Int (_) -> [(Type.Int, t)], Type.Int
  | Float (_) -> [(Type.Float, t)], Type.Float
  | Not e ->
    let eqs, _ = generate e Type.Bool in
    eqs @ [(Type.Bool, t)], Type.Bool
  | Neg e ->
    let eqs, _ = generate e Type.Int in
    eqs @ [(Type.Int, t)], Type.Int
  | Add (e1, e2) | Sub (e1, e2) ->
    let eqs1, _ = generate e1 Type.Int in
    let eqs2, _ = generate e2 Type.Int in
    eqs1 @ eqs2 @ [(Type.Int, t)], Type.Int
  | FNeg e ->
    let eqs, _ = generate e Type.Float in
    eqs @ [(Type.Float, t)], Type.Float
  | FAdd (e1, e2) | FSub (e1, e2) | FMul (e1, e2) | FDiv (e1, e2) ->
    let eqs1, _ = generate e1 Type.Float in
    let eqs2, _ = generate e2 Type.Float in
    eqs1 @ eqs2 @ [(Type.Float, t)], Type.Float
  | Eq (e1, e2) | LE (e1, e2) ->
    let eqs1, _ = generate e1 Type.Int in
    let eqs2, _ = generate e2 Type.Int in
    eqs1 @ eqs2 @ [(Type.Bool, t)], Type.Bool
  | If (e1, e2, e3) ->
    let eqs1, _ = generate e1 Type.Bool in
    let eqs2, _ = generate e2 t in
    let eqs3, _ = generate e3 t in
    eqs1 @ eqs2 @ eqs3, t
  | Let ((id,tv), e1, e2) ->
    let eqs1, t1 = generate e1 tv in
    st := St.add id t1 !st;
    let eqs2, t2 = generate e2 t in
    eqs1 @ eqs2 @ [(tv, t1)], t2
  | Var (id) when St.mem id !st -> let t1 = St.find id !st in [(t1, t)], t1
  | Var (id) when St.mem id !st_ext -> let t1 = St.find id !st_ext in [(t1, t)], t1
  | Var (id) ->
      let t1 = Type.gentyp () in st_ext := St.add id t1 !st_ext;
      [(t1, t)], t
  | App (e1, le2) ->
      (match e1 with
        (* known function label *)
        | Var id when St.mem id !st ->
          let fn = St.find id !st in (
            match fn with
            | Type.Fun (args, rt) | Type.Var ({ contents = Some (Type.Fun (args, rt)) })->
              let nb_args = List.length args in
              let nb_args_given = List.length le2 in
              if nb_args = nb_args_given then
                let mp = List.map2 
                    (fun x y -> fst (generate x y))
                    le2
                    args
                in
                List.concat mp @ [(rt, t)], rt
              else
                raise (failwith (Printf.sprintf "The function expects %d argument(s) while %d are supplied" nb_args nb_args_given))

            (* function type not resolved yet *)
            | Type.Var ({ contents = None }) ->
              let list_of_list_eqs =
                List.map
                  (fun arg -> fst (generate arg (Type.gentyp ())))
                  le2 in
              let eqs = List.fold_left
                (fun res leqs -> res @ leqs)
                []
                list_of_list_eqs in
              eqs, Type.gentyp ()

            | _ -> raise (failwith (
              Printf.sprintf "invalid function type %s for label %s"
                (Type.to_string fn)
                (Id.to_string id)
            ))
          )
        (* unknown function label -- treated as external *)
        | Var id ->
          let list_of_list_eqs =
            List.map
              (fun arg -> fst (generate arg (Type.gentyp ())))
              le2 in
          let eqs = List.fold_left
            (fun res leqs -> res @ leqs)
            []
            list_of_list_eqs in
          eqs, Type.Unit
        | _ ->
          let label_eqs, fun_t = generate e1 (Type.gentyp ()) in
          let (args, rt) = (match fun_t with
            | Type.Fun (args1, rt1) -> (args1, rt1)
            | _ ->
              raise (failwith (
                Printf.sprintf "invalid function type %s for exp %s"
                  (Type.to_string fun_t)
                  (Syntax.to_string e1)
              ))) in
          let nb_args = List.length args in
          let nb_args_given = List.length le2 in
          if nb_args = nb_args_given then
            let mp = List.map2 
                (fun x y -> fst (generate x y))
                le2
                args
            in (List.concat mp) @ label_eqs @ [(rt, t)], rt
          else
            raise (failwith (Printf.sprintf "The function expects %d argument(s) while %d are supplied" nb_args nb_args_given))
      )
  | LetRec ({ name = (id, tv); args = largs; body = e }, e2) -> 
      List.iter (fun (idi, tvi) -> 
        st := St.add idi tvi !st) largs;
      let eqs_body, t_body = generate e (Type.gentyp ()) in
      let fn = Type.Fun (List.map (fun x -> snd x) largs, t_body) in
      st := St.add id fn !st;
      let eqs2, t2 = generate e2 t in
      [(fn, tv)] @ eqs_body @ eqs2, t2
  (*| LetTuple (l, e1, e2)-> 
  | Put (e1, e2, e3) -> 
  | Tuple (l) -> *)
  | Array (e1, e2) -> 
    let eqs1, _ = generate e1 Type.Int in
    let eqs2, ta = generate e2 Type.Int in (* assume array type is int *)
    eqs1 @ eqs2 @ [(Type.Array(ta), t)], Type.Array(ta)
  | Get (e1, e2) -> 
    let eqs1, _ = generate e1 (Type.Array(Type.Int)) in
    let eqs2, _ = generate e2 Type.Int in
    eqs1 @ eqs2 @ [(Type.Int, t)], Type.Int
  | _ -> [(Type.Unit, Type.Unit)], Type.Unit

let rec unify eq = 
  match eq with
  | Type.Unit, Type.Unit | Type.Bool, Type.Bool | Type.Int, Type.Int | Type.Float, Type.Float -> ()
  | Type.Array (t1), Type.Array (t2) when t1 == t2 -> ()
  | Type.Var (t1), Type.Var (t2) when t1 == t2 -> ()
  | Type.Var ({ contents = Some(t1') }), _ -> let (_, t2) = eq in unify (t1', t2)
  | _, Type.Var ({ contents = Some(t2') }) -> let (t1, _) = eq in unify (t1, t2')
  | Type.Var ({ contents = None } as t1'), _ ->
      let (t1, t2) = eq in
      t1' := Some(t2)
  | _, Type.Var ({ contents = None } as t2') ->
      let (t1, t2) = eq in
      t2' := Some(t1)
  | Type.Fun(t1s, t1'), Type.Fun(t2s, t2') ->
      List.iter2 (fun x y -> unify (x, y)) t1s t2s;
      unify (t1', t2')
  | _ ->
      let t1, t2 = eq in
      raise (failwith (Printf.sprintf "mismatch types during unification: %s vs %s" (Type.to_string t1) (Type.to_string t2)))

let print_equations eq = 
  List.iter (fun (x, y) -> print_endline ("[left : " ^ Type.to_string x ^ ", right : " ^ Type.to_string y ^ "]")) eq 

let f exp = 
  st := St.empty; 
  st_ext := St.empty;

  (* TODO might need to perform multiple passes *)
  let eqs, _ = generate exp Type.Unit in
  List.iter (fun (eq1, eq2) -> unify (eq1, eq2)) eqs;
  exp
