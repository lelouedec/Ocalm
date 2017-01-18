open Printf

type t =
  | Unit
  | Int of int
  | Float of float
  | Not of Id.t
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | Eq of Id.t * Id.t
  | LE of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.t * Id.t list
  | MakeCls of (Id.t * Type.t) * Id.t * Id.t list * t
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
  | Let ((id, t), e1, e2) ->
          sprintf "(let (%s : %s) = %s in \n %s)" (Id.to_string id) (Type.to_string t) (exp_to_string e1) (exp_to_string e2)
  | Var id -> Id.to_string id
  | AppDir (label, args) ->
    sprintf "apply_direct(%s)"
      (String.concat ", " 
        (("_" ^ label) :: args)
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
  | _ -> failwith "nyi to_s"

let fn_to_string fn =
  let ((id, _), args, free_args, exp) = fn in
  Printf.sprintf "let _%s %s %s = \n %s\n"
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

let rec free_vars = function
  | Unit | Int _ | Float _ -> Env.empty
  | Not id | Neg id | FNeg id | Var id -> Env.singleton id
  | Add (id1, id2) | Sub (id1, id2) -> Env.of_list [id1; id2]
  | Let ((id, t), e1, e2) ->
    let set1 = free_vars e1 in
    let set2 = free_vars e2 in
    Env.union set1 (Env.remove id set2)
  | AppCls (id, args) | AppDir (id, args) -> Env.add id (Env.of_list args)
  | MakeCls ((id, t), label, free_args, e) ->
    let free = Env.union (Env.of_list free_args) (free_vars e) in
    Env.remove id free
  | _ -> Env.empty

let functions : let_fn list ref = ref []

let rec extract_main (exp : KNormal.t) (known : Env.t) (cls_names : Id.t St.t) : t =
  match exp with
  | KNormal.Unit -> Unit
  | KNormal.Int i -> Int i
  | KNormal.Float f -> Float f
  | KNormal.Neg id -> Neg id
  | KNormal.Add (id1, id2) -> Add (id1, id2)
  | KNormal.Sub (id1, id2) -> Sub (id1, id2)
  | KNormal.Let ((id, t), e1, e2) -> Let ((id, t), extract_main e1 known cls_names, extract_main e2 known cls_names)
  | KNormal.Var id ->
    if St.mem id cls_names then
      Var (St.find id cls_names)
    else
      Var id
  | KNormal.LetRec (fn, e) ->
    let ({ KNormal.name = (fname, ftype); KNormal.args = fargs; KNormal.body = fbody }) = fn in
    (* assume fname is a known function -- no free variables *)
    let known' = Env.add fname known in
    let fbody' = extract_main fbody known' cls_names in
    let list_args = List.map (fun (id, _) -> id) fargs in
    let free_vars = Env.diff (free_vars fbody') (Env.of_list list_args) in

    if Env.is_empty free_vars then
      let split_fn = ((fname, ftype), fargs, [], fbody') in
      functions := [split_fn] @ !functions;
      extract_main e known' cls_names
    else (
      let fbody' = extract_main fbody known cls_names in
      (* TODO lookup type of free variable somewhere instead of assuming as int *)
      let free_args = List.map (fun x -> (x, Type.Int)) (Env.elements free_vars) in
      let split_fn = ((fname, ftype), fargs, free_args, fbody') in
      functions := [split_fn] @ !functions;
      let newid = Id.genid () in
      (* add mapping between function label and variable that stores the closure *)
      let cls_names' = St.add fname newid cls_names in
      let e' = extract_main e known cls_names' in
      MakeCls ((newid, ftype), fname, Env.elements free_vars, e')
    )
  | KNormal.App (label, args) when Env.mem label known ->
    AppDir (label, args)
  | KNormal.App (id, args) ->
    let id' = if St.mem id cls_names then
      St.find id cls_names
    else id in
    AppCls (id', args)
  | KNormal.AppExt (label, args) ->
    AppDir ("min_caml_" ^ label, args)
  | _ -> failwith ("nyi extract" ^ KNormal.to_string exp)

let rec f (exp : KNormal.t) : prog =
  functions := [];
  let main_body = extract_main exp Env.empty St.empty in
  (!functions, main_body)
