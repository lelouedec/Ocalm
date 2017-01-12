open Printf

type ident_or_imm = 
	| Ident of Id.t 
	| Int of int 

let rec ident_or_imm_to_string l =
match l  with 
	| Ident i-> sprintf " %s " i
	| Int i -> string_of_int i

type exp = 
	| Nop 
	| LPexpRp of ( exp ) 
	| Int 
	| Ident of Id.t
	| Label of Id.t 
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
	| CallLabel of Id.t
	| CallClo of Id.t * exp

let rec to_string exp =
 match exp with 
 	| Nop ->" "
	| LPexpRp e -> sprintf " ( %s )" ( to_string e )
	| Int -> sprintf " :int "
	| Ident i ->  i
	| Label s -> sprintf "_ %s" s  
	| Neg i -> i
	| FNeg i -> i 
	| Add (i,id) -> sprintf " %s + %s "  (i)  (ident_or_imm_to_string  id)
	| Sub (i,id) -> sprintf "%s - %s "  (i)  (ident_or_imm_to_string id)
	| Ld (i,id) -> sprintf "%s + %s "  (i)  (ident_or_imm_to_string id)
	| St (i1,id,i2) -> sprintf "mem(%s + %s ) <- %s "  (i1)  (ident_or_imm_to_string id) (i2) 	
	| FAdd (i,id) -> sprintf "%s + %s "  (i)  (id)
	| FSub (i,id) -> sprintf "%s + %s "  (i)  (id)
	| FMul (i,id) -> sprintf "%s + %s "  (i)  (id)
	| FDiv (i,id) -> sprintf "%s + %s "  (i)  (id)
	| New i -> sprintf "new %s in " (ident_or_imm_to_string	i)
	| IfEq (i, id , t1, t2 ) -> sprintf ("if %s = %s  then %s else %s ") (i) (ident_or_imm_to_string id) ( to_string t1) (to_string t1 )
	| IfLEq (i, id , t1, t2 ) -> sprintf ("if %s <= %s then %s else %s ") (i) (ident_or_imm_to_string id) ( to_string t1) (to_string t1 )
	| IfGEq (i, id , t1, t2 ) -> sprintf ("if %s >= %s then %s else %s ") (i) (ident_or_imm_to_string id) ( to_string t1) (to_string t1 )
	| CallLabel e -> sprintf " Call %s" ( e)
	| CallClo  (id,t) -> sprintf " Call  %s %s" (id) (to_string t)


type asmt = 
	| LpasmtRPAREN of asmt 
	| LetIdentEq  of  Id.t * exp * asmt
	| Exp of exp 

let rec asmt_to_string asmt =
	match asmt with 
	| LpasmtRPAREN a -> sprintf " ( %s )" (asmt_to_string a)
	| LetIdentEq (i,e2,a) -> sprintf "Let %s = %s in %s" (i) ( to_string e2) (asmt_to_string a)
	| Exp e -> (to_string e)

type forma_args = Id.t list
	


let rec form_to_string (l : string list) : string =
  match l with
    [] -> ""
  | x :: xs -> x ^" " ^ (form_to_string xs)

	

type fundefs =
	| LetUnderscEQ of asmt (* Let _ =  *)
	| LetLabeleqFloat of Id.t * float * fundefs (*Let _label = 0.2 in *)
	| LetLabelEq of Id.t * forma_args * asmt * fundefs (* Let _label = something in ...*)

let rec fundefs_to_string fu =
	match fu with 
	| LetUnderscEQ a -> sprintf "Let _ = %s" (asmt_to_string a)
	| LetLabeleqFloat (l,fl, fu) -> sprintf " Let _%s = %.2f \n %s " (l) (fl) (fundefs_to_string fu)
	| LetLabelEq (l,fo, a, fu) -> sprintf "Let _%s %s =  %s \n \n%s" (l) (form_to_string fo) (asmt_to_string a) (fundefs_to_string fu)


let test exp =
	print_endline "******************************************************************";	
	print_endline (fundefs_to_string (exp) );

	(*Register_alloc.allocate exp2;*)



