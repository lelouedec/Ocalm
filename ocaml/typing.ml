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
  (*| Eq (e1, e2) -> 
  | LE (e1, e2) ->   
  | If (e1, e2, e3) -> *)
  | Let ((id,tv), e1, e2) -> st := St.add id tv !st; generate e1 tv @ generate e2 t
  | Var(id) when St.mem id !st -> let t1 = St.find id !st in [(t1, t)]
  | Var(id) when St.mem id !st_ext -> let t1 = St.find id !st_ext in [(t1, t)]
  | Var(id) ->
        let t1 = Type.gentyp () in st_ext := St.add id t1 !st_ext;
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

let f exp = 
  st := St.empty; 
  st_ext := St.empty;

  let eqs = generate exp Type.Unit in List.iter unify (eqs);
  exp

