open Asml

let rec ident_or_imm_to_asm l =
match l  with 
	| Ident i-> sprintf "ident : %s " i
	| Int i -> string_of_int i

let rec exp_to_asm exp f =
 match exp with 
 	| Nop -> ()
	| LPexpRp e -> ()
	| Int -> ()
	| Ident i -> let fu = function_has#look_for f in fu#add i 
	| Label s -> ()
	| Neg i -> ()
	| FNeg i -> () 
	| Add (i,id) -> let fu = function_has#look_for f in fu#add i
	| Sub (i,id) -> ()
	| Ld (i,id) -> ()
	| St (i1,id,i2) -> ()
	| FAdd (i,id) -> ()
	| FSub (i,id) -> ()
	| FMul (i,id) -> ()
	| FDiv (i,id) -> ()
	| New i -> ()
	| IfEq (i, id , t1, t2 ) -> ()
	| IfLEq (i, id , t1, t2 ) -> ()
	| IfGEq (i, id , t1, t2 ) -> ()
	| CallLabel e -> ()
	| CallClo  (id,t) -> ()


let rec asmt_to_asm a f =
	match a with
	| LpasmtRPAREN a -> assign_asmt a f
	| LetIdentEq (i,e2,a) -> let fu = function_has#look_for f in fu#add i; assign_exp e2 f ; assign_asmt a f
	| Exp e -> assign_exp e 

let rec form_to_asm (l : string list) f : unit =
  match l with
    [] -> ()
  | x :: xs -> let fu = function_has#look_for f in fu#add x  ;  assign_form xs f 

let rec function_to_asm exp  =
	match exp with
	| LetUnderscEQ a -> function_has#add "_" ;assign_asmt a "_"(* Let _ =  *)
	| LetLabeleqFloat (i,fl,fu) -> () ; () ; assign_function fu (*Let _label = 0.2 in *)
	| LetLabelEq (i,f,a,fu)-> function_has#add i ; assign_form f i;  assign_asmt a i; assign_function fu  (* Let _label = something in ...*)

let generate exp = 

	let asm_code = function_to_asm


