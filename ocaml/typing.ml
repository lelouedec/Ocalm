open Syntax

let st = ref St.empty
let st_ext = ref St.empty

let rec generate exp t =
  match exp with
  | Unit -> [(Type.Unit, t)]
  | Bool (_) -> [(Type.Bool, t)]
  | Int (_) -> [(Type.Int, t)]
  | Float (_) -> [(Type.Float, t)]
  | Not e -> generate e Type.Bool @ [(Type.Bool, t)]
  | Neg e -> generate e Type.Int @ [(Type.Int, t)]
  | Add (e1, e2) -> generate e1 Type.Int @ generate e2 Type.Int @ [(Type.Int, t)]
  | Sub (e1, e2) -> generate e1 Type.Int @ generate e2 Type.Int @ [(Type.Int, t)]
  | FNeg e -> generate e Type.Float @ [(Type.Float, t)]
  | FAdd (e1, e2) -> generate e1 Type.Float @ generate e2 Type.Float @ [(Type.Float, t)]
  | FSub (e1, e2) -> generate e1 Type.Float @ generate e2 Type.Float @ [(Type.Float, t)]
  | FMul (e1, e2) -> generate e1 Type.Float @ generate e2 Type.Float @ [(Type.Float, t)]
  | FDiv (e1, e2) -> generate e1 Type.Float @ generate e2 Type.Float @ [(Type.Float, t)]
  | Eq (e1, e2) -> generate e1 Type.Int @ generate e2 Type.Int @ [(Type.Bool, t)]
  | LE (e1, e2) -> generate e1 Type.Int @ generate e2 Type.Int @ [(Type.Bool, t)]
  | If (e1, e2, e3) -> generate e1 Type.Bool @ generate e2 t @ generate e3 t
  | Let ((id,tv), e1, e2) -> st := St.add id tv !st; generate e1 tv @ generate e2 t
  | Var (id) when St.mem id !st -> let t1 = St.find id !st in [(t1, t)]
  | Var (id) when St.mem id !st_ext -> let t1 = St.find id !st_ext in [(t1, t)]
  | Var (id) ->
      let t1 = Type.gentyp () in st_ext := St.add id t1 !st_ext;
      [(t1, t)]
  | App (e1, le2) -> print_endline (Syntax.to_string e1);
      let t1 = Type.gentyp() in 
      (match e1 with
        (* known function label *)
        | Var id when St.mem id !st ->
          let fn = St.find id !st in print_endline ("fn : " ^ (Type.to_string fn));
          let (args, rt) = (match fn with 
            | Type.Fun (args1, rt1) -> (args1, rt1)
            | _ -> raise (failwith "f")) in
          let mp = List.map2 
              (fun x y -> generate x y)
              le2
              args
          in List.concat mp @ [(t1, fn)] @ [(rt, t)]
        (* unknown function label -- treated as external *)
        | Var id ->
          let list_of_list_eqs =
            List.map
              (fun arg -> generate arg (Type.gentyp ()))
              le2 in
          List.fold_left
            (fun res leqs -> res @ leqs)
            []
            list_of_list_eqs
        | _ -> print_endline "app typing not supported in this case"; []);
      (*let fn = St.find e1 !st in print_endline ("fn : " ^ (Type.to_string fn));*)
      (*let ls = List.map (fun x -> generate x (Type.Var (ref (None)))) le2 in List.concat ls *)
  | LetRec ({ name = (id, tv); args = largs; body = e }, e2) -> 
      let fn = Type.Fun(List.map (fun x -> snd x) largs, tv) in
      List.iter (fun (idi, tvi) -> 
        st := St.add idi tvi !st) largs;
        st := St.add id (fn) !st;

      generate e tv @ generate e2 t
  (*| LetTuple (l, e1, e2)-> 
  | Get (e1, e2) -> 
  | Put (e1, e2, e3) -> 
  | Tuple (l) -> 
  | Array (e1, e2) -> *)
  | _ -> [(Type.Unit, Type.Unit)]

let rec unify eq = 
  match eq with
  | Type.Unit, Type.Unit | Type.Bool, Type.Bool | Type.Int, Type.Int | Type.Float, Type.Float -> ()
  | Type.Var (t1), Type.Var (t2) when t1 == t2 -> ()
  | Type.Var (t1), Type.Var (t2) -> (*t1 := Some(Type.Int); t2 := Some(Type.Int);*) () (* unknown types : int *)
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
  | _ -> raise (failwith "mismatch types during unification")

let print_equations eq = 
  List.iter (fun (x, y) -> print_endline ("[left : " ^ Type.to_string x ^ ", right : " ^ Type.to_string y ^ "]")) eq 

let f exp = 
  st := St.empty; 
  st_ext := St.empty;

  let eqs = generate exp Type.Unit in (print_equations eqs); List.iter unify (eqs); print_equations eqs;
  exp
