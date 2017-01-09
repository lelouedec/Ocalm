open Printf



type ident_or_imm = 
	| Ident of Id.t 
	| Int of int 

let rec ident_or_imm_to_string l =
match l  with 
	| Ident i-> sprintf "ident : %s " i
	| Int i -> string_of_int i

type exp = 
	| Let of (Id.t * Type.t) * exp * exp
	| Nop 
	| LPexpRp of ( exp ) 
	| Int of int 
	| Ident of Id.t
	| Label of string 
	| Neg of Id.t
	| FNeg of Id.t
	| Add of Id.t * ident_or_imm
	| Sub of Id.t * ident_or_imm
	| Ld of Id.t * ident_or_imm
	| St of Id.t * ident_or_imm * Id.t	
	| FAdd of Id.t * Id.t
	| FSub of Id.t * Id.t
	| FMul of Id.t * Id.t 
	| FDiv of Id.t * Id.t 
	| New of ident_or_imm
	| IfEq of Id.t * ident_or_imm * exp* exp
	| IfLEq of Id.t * ident_or_imm * exp * exp
	| IfGEq of Id.t * ident_or_imm * exp * exp
	| CallLabel of exp
	| CallClo of Id.t * exp

type fundef = { name : Id.t; args : Id.t list; fargs : Id.t list; body : exp; ret : Type.t }



let rec to_string exp =
 match exp with 
   	| Let ((id,t), e1, e2) -> 
          sprintf "(let %s = %s in %s)" (id) (to_string e1) (to_string e2)
 	| Nop ->" "
	| LPexpRp e -> sprintf " ( %s )" ( to_string e )
	| Int i -> string_of_int i 
	| Ident i -> i 
	| Label s -> s  
	| Neg i -> i
	| FNeg i -> i 
	| Add (i,id) -> sprintf "%s + %s "  (i)  (ident_or_imm_to_string  id)
	| Sub (i,id) -> sprintf "%s + %s "  (i)  (ident_or_imm_to_string id)
	| Ld (i,id) -> sprintf "%s + %s "  (i)  (ident_or_imm_to_string id)
	| St (i1,id,i2) -> sprintf "mem(%s + %s ) <- %s "  (i1)  (ident_or_imm_to_string id) (i2) 	
	| FAdd (i,id) -> sprintf "%s + %s "  (i)  (id)
	| FSub (i,id) -> sprintf "%s + %s "  (i)  (id)
	| FMul (i,id) -> sprintf "%s + %s "  (i)  (id)
	| FDiv (i,id) -> sprintf "%s + %s "  (i)  (id)
	| New i -> sprintf "new %s" (ident_or_imm_to_string	i)
	| IfEq (i, id , t1, t2 ) -> sprintf ("if %s = %s  then %S else %s ") (i) (ident_or_imm_to_string id) ( to_string t1) (to_string t1 )
	| IfLEq (i, id , t1, t2 ) -> sprintf ("if %s <= %s then %S else %s ") (i) (ident_or_imm_to_string id) ( to_string t1) (to_string t1 )
	| IfGEq (i, id , t1, t2 ) -> sprintf ("if %s >= %s then %S else %s ") (i) (ident_or_imm_to_string id) ( to_string t1) (to_string t1 )
	| CallLabel t -> sprintf " Call %s" (to_string t)
	| CallClo  (id,t) -> sprintf " Call  %s %s" (id) (to_string t)



let test () =
	print_endline "Test";
	let exp = 
		Let (
				("x", Type.Int ),
				Int 0,
				Ident "x"
			) in 
	print_endline (to_string (exp) ); 




