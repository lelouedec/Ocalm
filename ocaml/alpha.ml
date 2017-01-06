open KNormal

let rec f (exp : t) : t =
  match exp with
  | Unit -> exp
  | Bool b -> exp
  | Int i -> exp
  | Float f -> exp
  | Let ((id, t), e1, e2) ->
    Let (
      (Id.genid (), t),
      f e1,
      f e2)
  | Var id -> Var (Id.genid ())
  | _ -> failwith "undef"

let g () : t = Var("x")
