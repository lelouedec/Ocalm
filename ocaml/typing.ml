open Syntax

let rec genenerate exp st extst t =
  match exp with
  | Unit -> [(Type.Unit, t)]
  | Bool (_) -> [(Type.Bool, t)]
  | Int (_) -> [(Type.Int, t)]
  | Float (_) -> [(Type.Float, t)]
  | Not e -> genenerate e st extst Type.Bool @ [(Type.Bool, t)]
  | Neg e -> genenerate e st extst Type.Int @ [(Type.Int, t)]
  | Add (e1, e2) -> genenerate e1 st extst Type.Int @ genenerate e2 st extst Type.Int @ [(Type.Int, t)]
  | Sub (e1, e2) -> genenerate e1 st extst Type.Int @ genenerate e2 st extst Type.Int @ [(Type.Int, t)]
  | FNeg e -> genenerate e st extst Type.Float @ [(Type.Float, t)]
  | FAdd (e1, e2) -> genenerate e1 st extst Type.Float @ genenerate e2 st extst Type.Float @ [(Type.Float, t)]
  | FSub (e1, e2) -> genenerate e1 st extst Type.Float @ genenerate e2 st extst Type.Float @ [(Type.Float, t)]
  | FMul (e1, e2) -> genenerate e1 st extst Type.Float @ genenerate e2 st extst Type.Float @ [(Type.Float, t)]
  | FDiv (e1, e2) -> genenerate e1 st extst Type.Float @ genenerate e2 st extst Type.Float @ [(Type.Float, t)]
  (*| Eq (e1, e2) -> 
  | LE (e1, e2) ->   
  | If (e1, e2, e3) -> *)
  | Let ((id,tv), e1, e2) -> genenerate e1 st extst tv @ genenerate e2 (St.add id tv st) extst t
	| Var(id) when St.mem id st -> let t1 = St.find id st in [(t1, t)]
  | Var(id) when St.mem id extst -> let t1 = St.find id extst in [(t1, t)]
	| Var(id) ->
				let t1 = Type.gentyp () in St.add id t1 extst;
				[t1, t]
  (*| App (e1, le2) -> 
  | LetRec (fd, e) -> 
  | LetTuple (l, e1, e2)-> 
  | Get (e1, e2) -> 
  | Put (e1, e2, e3) -> 
  | Tuple (l) -> 
  | Array (e1, e2) -> *)
  | _ -> [(Type.Unit, Type.Unit)]

let rec unify eq = 
  match eq with
  | Type.Unit, Type.Unit | Type.Bool, Type.Bool | Type.Int, Type.Int | Type.Float, Type.Float -> ()
  | Type.Var(t1), Type.Var(t2) when t1 == t2 -> ()
  | Type.Var({ contents = Some(t1') }), _ -> let (_, t2) = eq in unify (t1', t2)
  | _, Type.Var({ contents = Some(t2') }) -> let (t1, _) = eq in unify (t1, t2')
  | Type.Var({ contents = None } as t1'), _ ->
      let (t1, t2) = eq in
      t1' := Some(t2)
  | _, Type.Var({ contents = None } as t2') ->
      let (t1, t2) = eq in
      t2' := Some(t1)
  | _ -> raise (failwith "mismatch types during unification")

let rec to_string t =
  match t with
  | (t1, t2) -> print_endline ("lhs -> " ^ Type.to_string(t1) ^ " rhs -> " ^ Type.to_string(t2))
