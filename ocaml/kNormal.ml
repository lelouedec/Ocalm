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
  | IfEq of Id.t * Id.t * t * t (* x == y , branch if , branch else *)
  | IfLE of Id.t * Id.t * t * t (* x <= y , branch if , branch else *)
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
  | AppExt of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Array of Id.t * Id.t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let insert_let (e, t) k =
  match e with
  | Var x -> k x
  | _ ->
      let x = Id.genid () in let e', t' = k x in Typing.st := St.add x t !Typing.st;
      Let ((x, t), e, e'), t'

let rec temporaries exp =
  match exp with
  | Syntax.Unit -> (Unit, Type.Unit)
  | Syntax.Bool b -> if b = true then (Int (1), Type.Bool) else (Int (0), Type.Bool)
  | Syntax.Int i -> (Int (i), Type.Int)
  | Syntax.Float f -> (Float (f), Type.Float)
  | Syntax.Not e -> insert_let (temporaries e) (fun x -> Not (x), Type.Bool)
  | Syntax.Neg e -> insert_let (temporaries e) (fun x -> Neg (x), Type.Int)
  | Syntax.Add (e1, e2) ->
      insert_let (temporaries e1)
        (fun x -> insert_let (temporaries e2)
          (fun y -> Add (x, y), Type.Int))
  | Syntax.Sub (e1, e2) -> 
      insert_let (temporaries e1)
        (fun x -> insert_let (temporaries e2)
          (fun y -> Sub (x, y), Type.Int))
  | Syntax.FNeg e -> insert_let (temporaries e) (fun x -> FNeg (x), Type.Float)
  | Syntax.FAdd (e1, e2) -> 
      insert_let (temporaries e1)
        (fun x -> insert_let (temporaries e2)
          (fun y -> FAdd (x, y), Type.Float))
  | Syntax.FSub (e1, e2) -> 
      insert_let (temporaries e1)
        (fun x -> insert_let (temporaries e2)
          (fun y -> FSub (x, y), Type.Float))
  | Syntax.FMul (e1, e2) -> 
      insert_let (temporaries e1)
        (fun x -> insert_let (temporaries e2)
          (fun y -> FMul (x, y), Type.Float))
  | Syntax.FDiv (e1, e2) -> 
      insert_let (temporaries e1)
        (fun x -> insert_let (temporaries e2)
          (fun y -> FDiv (x, y), Type.Float))
  | Syntax.Eq (e1, e2) -> 
      insert_let (temporaries e1)
        (fun x -> insert_let (temporaries e2)
          (fun y -> Eq (x, y), Type.Bool))
  | Syntax.LE (e1, e2) ->  
      insert_let (temporaries e1)
        (fun x -> insert_let (temporaries e2)
          (fun y -> LE (x, y), Type.Bool)) 
  | Syntax.If (e1, e2, e3) ->
      let e2', t1 = temporaries e2 in
      let e3', t2 = temporaries e3 in
      (match e1 with
      | Syntax.Eq (e4, e5) -> 
        insert_let (temporaries e4)
          (fun x -> insert_let (temporaries e5)
            (fun y -> IfEq (x, y, e2', e3'), Type.Unit)) 
      | Syntax.LE (e4, e5) -> 
        insert_let (temporaries e4)
          (fun x -> insert_let (temporaries e5)
            (fun y -> IfLE (x, y, e2', e3'), Type.Unit))
      | Syntax.Not (e) -> temporaries (Syntax.If(e, e3, e2))
      | _ -> raise (failwith "incorrect if test expression"))
  | Syntax.Let ((id,t), e1, e2) ->
      let e1', t1 = temporaries e1 in
      let e2', t2 = temporaries e2 in
      Let ((id, t), e1', e2'), t2
  | Syntax.Var (id) when St.mem id !Typing.st -> Var (id), St.find id !Typing.st
  | Syntax.App (Syntax.Var (id), le2) when not (St.mem id !Typing.st) ->
    let rec convert_args (f : Id.t) (le : Syntax.t list) (ids : Id.t list) : t * Type.t = (
      match le with
      | [] -> AppExt (f, ids), Type.Unit (* assume that external functions return unit *)
      | hd :: tl ->
        insert_let (temporaries hd)
          (fun x -> convert_args f tl (ids @ [x]))
    ) in
    insert_let (Var id, Type.Unit)
      (fun x -> convert_args x le2 [])
  | Syntax.App (e1, le2) ->
      let label, t = ( match e1 with
        | Syntax.Var (id) when St.mem id !Typing.st -> Var id, St.find id !Typing.st
        | _ -> temporaries e1
      ) in

      let rt = (match t with
        | Type.Fun (_, rt') | Type.Var ({ contents = Some (Type.Fun (_, rt'))}) -> rt'
        | _ -> raise (failwith ("not a function -- " ^ (Syntax.to_string e1)))
      ) in
      let rec convert_args (f : Id.t) (le : Syntax.t list) (ids : Id.t list) : t * Type.t = (
        match le with
        | [] -> App (f, ids), rt
        | hd::tl ->
          insert_let (temporaries hd)
            (fun x -> convert_args f tl (ids @ [x]))
      ) in
      insert_let (label, t)
        (fun f -> convert_args f le2 [])
  | Syntax.LetRec ({Syntax.name = (label, ft) ; Syntax.args = args ; Syntax.body = body}, e) -> 
      List.iter 
        (fun (x, t) -> Typing.st := St.add x t !Typing.st; ())
        args;
      let args_type = List.map (fun (_, t) -> t) args in
      let body', ft' = temporaries body in
      Typing.st := St.add
        label
        (Type.Fun (args_type, ft'))
        !Typing.st;
      let e', t = temporaries e in
      LetRec ({name = (label, ft') ; args = args ; body = body'}, e'), t
  (*
  | Syntax.LetTuple (l, e1, e2)-> 
  | Syntax.Get (e1, e2) -> 
  | Syntax.Put (e1, e2, e3) -> 
  | Syntax.Tuple (l) -> 
  | Syntax.Array (e1, e2) -> *)
  | _ -> (Unit, Type.Int)

let rec to_string exp =
    match exp with
  | Unit -> "()"
  | Int i -> string_of_int i ^ ""
  | Float f -> sprintf "%.2f" f
  | Not id -> sprintf "(not %s)" (Id.to_string id)
  | Neg id -> sprintf "(neg %s)" (Id.to_string id)
  | Add (id1, id2) -> sprintf "(%s + %s)" (Id.to_string id1) (Id.to_string id2)
  | Sub (id1, id2) -> sprintf "(%s - %s)" (Id.to_string id1) (Id.to_string id2)
  | FNeg id -> sprintf "(-. %s)" (Id.to_string id)
  | FAdd (id1, id2) -> sprintf "(%s +. %s)" (Id.to_string id1) (Id.to_string id2)
  | FSub (id1, id2) -> sprintf "(%s -. %s)" (Id.to_string id1) (Id.to_string id2)
  | FMul (id1, id2) -> sprintf "(%s *. %s)" (Id.to_string id1) (Id.to_string id2)
  | FDiv (id1, id2) -> sprintf "(%s /. %s)" (Id.to_string id1) (Id.to_string id2)
  | Eq (id1, id2) -> sprintf "(%s = %s)" (Id.to_string id1) (Id.to_string id2)
  | LE (id1, id2) -> sprintf "(%s <= %s)" (Id.to_string id1) (Id.to_string id2)
  | IfEq (id1, id2, e1, e2) -> sprintf "(if %s = %s) then %s else %s" (Id.to_string id1) (Id.to_string id2) (to_string e1) (to_string e2)
  | IfLE (id1, id2, e1, e2) -> sprintf "(if %s <= %s) then %s else %s" (Id.to_string id1) (Id.to_string id2) (to_string e1) (to_string e2)
  | Let ((id, t), e1, e2) ->
          sprintf "(let (%s : %s) = %s in \n%s)" (Id.to_string id) (Type.to_string t) (to_string e1) (to_string e2)
  | Var id -> Id.to_string id
  | App (id, args) ->
          sprintf "(%s %s)"
          (Id.to_string id)
          (
            match args with
            | [] -> "()"
            | _ -> (String.concat " " (List.map (fun arg -> Id.to_string arg) args))
          )
  | AppExt (id, args) ->
          sprintf "(min_caml_%s %s)"
          (Id.to_string id)
          (
            match args with
            | [] -> "()"
            | _ -> (String.concat " " (List.map (fun arg -> Id.to_string arg) args))
          )
  | LetRec (fd, e) ->
          let (x, fun_t) = fd.name in
          sprintf "(let rec %s %s : %s = %s in \n%s)"
            (Id.to_string x)
            (
              match fd.args with
              | [] -> "()"
              | _ -> (String.concat
                " "
                (List.map
                  (fun (arg, t) -> sprintf "(%s : %s)" (Id.to_string arg) (Type.to_string t))
                  fd.args
                )
              )
            )
            (Type.to_string fun_t)
            (to_string fd.body)
            (to_string e)
(*
  | LetTuple (l, e1, e2)->
  | Get (e1, e2) ->
  | Put (e1, e2, e3) ->
  | Tuple (l) ->
  | Array (e1, e2) ->
 *)
  | _ -> "unsupported knormal expression"

let f exp =
  let n, _ = temporaries exp in
  n
