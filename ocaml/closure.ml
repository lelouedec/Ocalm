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
  (* to be added *)

type let_fn = (Id.t * Type.t) * (Id.t * Type.t) list * t
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
        (["_" ^ label] @ args)
      )
  | _ -> failwith "nyi to_s"

let fn_to_string fn =
  let ((id, _), args, exp) = fn in
  Printf.sprintf "let _%s %s = \n %s\n"
    id
    (String.concat " " (List.map (fun arg -> let (id, _) = arg in id) args))
    (exp_to_string exp)
  
let main_to_string main =
  Printf.sprintf "let () =  \n %s" (exp_to_string main)

let to_string (prog : prog) =
  let (functions, main) = prog in
  (String.concat
    "\n"
    (List.map fn_to_string functions)
  ) ^ main_to_string main


let functions : let_fn list ref = ref []

let rec extract_main (exp : KNormal.t) : t =
  match exp with
  | KNormal.Unit -> Unit
  | KNormal.Int i -> Int i
  | KNormal.Float f -> Float f
  | KNormal.Neg id -> Neg id
  | KNormal.Add (id1, id2) -> Add (id1, id2)
  | KNormal.Sub (id1, id2) -> Sub (id1, id2)
  | KNormal.Let ((id, t), e1, e2) -> Let ((id, t), extract_main e1, extract_main e2)
  | KNormal.Var id -> Var id
  | KNormal.LetRec (fn, e) ->
    let ({ KNormal.name = fname; KNormal.args = fargs; KNormal.body = fbody }) = fn in
    let list_args = List.map (fun (id, _) -> id) fargs in
    let free_vars = Env.diff (KNormal.free_vars fbody) (Env.of_list list_args) in
    (* TODO use free vars list to determine direct or closure call *)
    let split_fn = (fname, fargs, extract_main fbody) in
    functions := [split_fn] @ !functions;
    extract_main e
  | KNormal.App (label, args) ->
    (* assume no app closure for now *)
    AppDir (label, args)
  | KNormal.AppExt (label, args) ->
    AppDir ("min_caml_" ^ label, args)
  | _ -> failwith "nyi extract"

let rec f (exp : KNormal.t) : prog =
  functions := [];
  let main_body = extract_main exp in
  (!functions, main_body)
