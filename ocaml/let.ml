open KNormal

let rec f exp = 
	match exp with
	| Let ((id, t), e1, e2) ->
		let rec insert exp1 = 
			match exp1 with
			| Let (id, e3, e4) -> Let (id, e3, insert e4)
			| LetRec (fd, e) -> LetRec (fd, insert e)
			(* | LetTuple (l, e1, e2) -> LetTuple (l, e1, insert e2) *)
			| e -> Let ((id, t), e, f e2) in
		insert (f e1)
	| LetRec ({ name = (label, t); args = args; body = body }, e) -> 
		LetRec ({ name = (label, t); args = args; body = f body }, f e)
	(* | LetTuple (l, e1, e2) -> LetTuple (l, e1, f e2) *)
	| IfEq (id1, id2, e1, e2) -> IfEq (id1, id2, f e1, f e2) 
	| IfLE (id1, id2, e1, e2) -> IfLE (id1, id2, f e1, f e2) 
	| e -> e
