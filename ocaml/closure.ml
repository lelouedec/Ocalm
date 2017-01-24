open Printf

type t =
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.t * Id.t list
  | MakeCls of (Id.t * Type.t) * Id.t * Id.t list * t
  | Tuple of Id.t list
  | Array of Id.t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  (* to be added *)

type let_fn = (Id.t * Type.t) * (Id.t * Type.t) list * (Id.t * Type.t) list * t
type let_main = t
  
type prog = let_fn list * let_main

let rec exp_to_string = function
  | Unit -> "()"
  | Int i -> string_of_int i
  | Float f -> sprintf "%.2f" f
  | Neg id -> sprintf "(- %s)" (Id.to_string id)
  | Add (id1, id2) -> sprintf "(%s + %s)" (Id.to_string id1) (Id.to_string id2)
  | Sub (id1, id2) -> sprintf "(%s - %s)" (Id.to_string id1) (Id.to_string id2)
  | IfEq (id1, id2, e1, e2) ->
          sprintf "if %s = %s then\n  %s\nelse\n  %s\n"
          (Id.to_string id1)
          (Id.to_string id2)
          (exp_to_string e1)
          (exp_to_string e2)
  | IfLE (id1, id2, e1, e2) ->
          sprintf "if %s <= %s then\n  %s\nelse\n  %s\n"
          (Id.to_string id1)
          (Id.to_string id2)
          (exp_to_string e1)
          (exp_to_string e2)
  | Let ((id, t), e1, e2) ->
          sprintf "(let (%s : %s) = %s in \n %s)" (Id.to_string id) (Type.to_string t) (exp_to_string e1) (exp_to_string e2)
  | Var id -> Id.to_string id
  | AppDir (label, args) ->
    sprintf "apply_direct(%s)"
      (String.concat ", " 
        ((label) :: args)
      )
  | AppCls (id, args) ->
    sprintf "apply_closure(%s)"
      (String.concat ", "
        (id :: args)
      )
  | MakeCls ((id, t), label, free_vars, e) ->
    sprintf "let %s = make_closure(%s, %s) in %s"
      (Id.to_string id)
      (Id.to_string label)
      (String.concat ", " (List.map Id.to_string free_vars))
      (exp_to_string e)
  | Tuple ids -> sprintf "<tuple, (%s)>" 
      (String.concat "," 
        (List.map (fun id -> (Id.to_string id)) ids))
  | Array id -> sprintf "<array, %s>" (Id.to_string id)
  | Get (id1, id2) -> sprintf "%s.(%s)" (Id.to_string id1) (Id.to_string id2)
  | Put (id1, id2, id3) -> sprintf "%s.(%s) <- %s" (Id.to_string id1) (Id.to_string id2) (Id.to_string id3)
  | _ -> failwith "nyi to_s"

let fn_to_string fn =
  let ((id, _), args, free_args, exp) = fn in
  Printf.sprintf "let %s %s %s = \n %s\n"
    id
    (String.concat " " (List.map fst args))
    (String.concat " " (List.map fst free_args))
    (exp_to_string exp)
  
let main_to_string main =
  Printf.sprintf "let () =  \n %s" (exp_to_string main)

let to_string (prog : prog) =
  let (functions, main) = prog in
  (String.concat
    "\n"
    (List.map fn_to_string functions)
  ) ^ main_to_string main

let rec free_vars exp =
  Env.remove "%self" (
    match exp with
    | Unit | Int _ | Float _ -> Env.empty
    | Neg id | FNeg id | Var id -> Env.singleton id
    | Add (id1, id2) | Sub (id1, id2) -> Env.of_list [id1; id2]
    | IfEq (id1, id2, e1, e2) | IfLE (id1, id2, e1, e2) ->
      let free1 = free_vars e1 in
      let free2 = free_vars e2 in
      Env.add id1 (Env.add id2 (Env.union free1 free2))
    | Let ((id, t), e1, e2) ->
      let set1 = free_vars e1 in
      let set2 = free_vars e2 in
      Env.union set1 (Env.remove id set2)
    | AppDir (id, args) -> Env.of_list args (* function labels are always known *)
    | AppCls (id, args) -> Env.add id (Env.of_list args)
    | MakeCls ((id, t), label, free_args, e) ->
      let free = Env.union (Env.of_list free_args) (free_vars e) in
      Env.remove id free
    | _ -> Env.empty
  )

let functions : let_fn list ref = ref []
let current_fn = ref ""

let fname_to_cls f cls_names =
  try
    St.find f cls_names
  with e -> f

let rec extract_main (exp : KNormal.t) (known : Env.t) (cls_names : Id.t St.t) : t =
  match exp with
  | KNormal.Unit -> Unit
  | KNormal.Int i -> Int i
  | KNormal.Float f -> Float f
  | KNormal.Neg id -> Neg id
  | KNormal.Add (id1, id2) -> Add (id1, id2)
  | KNormal.Sub (id1, id2) -> Sub (id1, id2)
  | KNormal.IfEq (id1, id2, e1, e2) ->
    IfEq (id1, id2, extract_main e1 known cls_names, extract_main e2 known cls_names)
  | KNormal.IfLE (id1, id2, e1, e2) ->
    IfLE (id1, id2, extract_main e1 known cls_names, extract_main e2 known cls_names)
  | KNormal.Let ((id, t), e1, e2) -> Let ((id, t), extract_main e1 known cls_names, extract_main e2 known cls_names)
  | KNormal.Var id ->
    Var (fname_to_cls id cls_names)
  | KNormal.LetRec (fn, e) ->
    let functions_backup = !functions in
    let ({ KNormal.name = (fname, ftype); KNormal.args = fargs; KNormal.body = fbody }) = fn in
    (* assume fname is a known function -- no free variables *)
    let known' = Env.add fname known in
    let current_fn_backup = !current_fn in
    current_fn := fname;
    let fbody' = extract_main fbody known' cls_names in
    current_fn := current_fn_backup;
    let list_args = List.map (fun (id, _) -> id) fargs in
    let free_vars = Env.diff (free_vars fbody') (Env.of_list (fname :: list_args)) in

    if Env.is_empty free_vars then (
      let split_fn = ((fname, ftype), fargs, [], fbody') in
      functions := [split_fn] @ !functions;
      extract_main e known' cls_names
    ) else (
      functions := functions_backup;
      current_fn := fname;
      let fbody' = extract_main fbody known cls_names in
      current_fn := current_fn_backup;
      (* TODO lookup type of free variable somewhere instead of assuming as int *)
      let free_args = List.map (fun x -> (x, Type.Int)) (Env.elements free_vars) in
      let split_fn = ((fname, ftype), fargs, free_args, fbody') in
      functions := [split_fn] @ !functions;
      let newid = Id.gen_asml_id () in
      (* add mapping between function label and variable that stores the closure *)
      let cls_names' = St.add fname newid cls_names in
      let e' = extract_main e known cls_names' in

      let rec extract_label_to_arg args cls_names =
        match args with
        | [] ->
          MakeCls ((newid, ftype), fname, List.map (fun arg -> fname_to_cls arg cls_names) (Env.elements free_vars), e')
        | hd :: tl ->
          if Env.mem hd known then
            let id = Id.gen_asml_id () in
            (* FIXME function type should be different *)
            MakeCls ((id, ftype), hd, [], extract_label_to_arg tl (St.add hd id cls_names))
          else
            let cls_names = if hd = !current_fn then (St.add hd "%self" cls_names) else cls_names in
            extract_label_to_arg tl cls_names
      in  
      extract_label_to_arg (Env.elements free_vars) cls_names
    )
  | KNormal.App (label, args) when Env.mem label known ->
    let args' = List.map (fun arg -> fname_to_cls arg cls_names) args in
    AppDir (label, args')
  | KNormal.App (id, args) ->
    let id' = fname_to_cls id (St.add !current_fn "%self" cls_names) in
    let args' = List.map (fun arg -> fname_to_cls arg cls_names) args in
    AppCls (id', args')
  | KNormal.AppExt (label, args) ->
    AppDir ("min_caml_" ^ label, args)
  | KNormal.Tuple (ids) -> Tuple (ids)
  | KNormal.Array (id) -> Array (id)
  | KNormal.Get (id1, id2) -> Get(id1, id2)
  | KNormal.Put (id1, id2, id3) -> Put (id1, id2, id3)
  | _ -> failwith ("nyi extract\nexp: " ^ KNormal.to_string exp)

let rec f (exp : KNormal.t) : prog =
  current_fn := "";
  functions := [];
  let main_body = extract_main exp Env.empty St.empty in
  (!functions, main_body)
