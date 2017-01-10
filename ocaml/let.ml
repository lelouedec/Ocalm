open KNormal

let rec f exp =
  match exp with
  | Let(xt, e1, e2) ->
      let rec reduce e = match e with
	    | Let(yt, e3, e4) -> Let(yt, e3, reduce e4)
	    | _ -> Let(xt, e, f e2) in reduce e1
| _ -> exp

