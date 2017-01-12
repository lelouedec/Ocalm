open Asml
let Label_counter = 0 
let rec ident_or_imm_to_asm (l : ident_or_imm)  reg  =
match l  with 
	| Ident i-> sprintf "%s " (reg#look_for i)
	| Int i ->  sprintf"#%s " (i)

let rec exp_to_asm exp reg =
 match exp with 
 	| Nop -> 
	| LPexpRp e -> 
	| Int -> 
	| Ident i ->  
	| Label s -> 
	| Neg i -> "NEG %s" (reg#look_for i)
	| FNeg i ->  
	| Add (i,id) -> sprintf "ADD %s , %s " (reg#look_for i) (ident_or_imm_to_asm id)
	| Sub (i,id) -> sprintf "SUB %s , %s " (reg#look_for i) (ident_or_imm_to_asm id)
	| Ld (i,id) -> sprintf "LDR %s , [%s] " (reg#look_for i) (ident_or_imm_to_asm id)
	| St (i1,id,i2) -> sprintf "STR %s, [%s, %s]" (reg#look_for i1) (ident_or_imm_to_asm id) (reg#look_for i2)
	| FAdd (i,id) -> 
	| FSub (i,id) -> 
	| FMul (i,id) -> 
	| FDiv (i,id) -> 
	| New i -> 
	| IfEq (i, id , t1, t2 ) -> sprintf "CMP %s , %s \n 
										BEQ labeltrue%d \n 
										%s \n
										BNE label_out%d  \n 
										labeltrue%d %s  \n
										label_out%s " (reg#look_for i) (ident_or_imm_to_asm id) (Label_counter) (exp_to_asm t2) (Label_counter) (i) (exp_to_asm t1) (i);Label_counter <- Label_counter + 1
	| IfLEq (i, id , t1, t2 ) -> sprintf "CMP %s , %s \n 
										BLEQ labeltrue%d \n 
										%s \n
										BNE label_out%d  \n 
										labeltrue%s %s  \n
										label_out%s " (reg#look_for i) (ident_or_imm_to_asm id) (Label_counter) (exp_to_asm t2) (Label_counter) (i) (exp_to_asm t1) (i)
	| IfGEq (i, id , t1, t2 ) -> sprintf "CMP %s , %s \n 
										BGEQ labeltrue%s \n 
										%s \n
										BNE label_out%s  \n 
										labeltrue%s %s  \n
										label_out%s " (reg#look_for i) (ident_or_imm_to_asm id) (i) (exp_to_asm t2) (i) (i) (exp_to_asm t1) (i)
	| CallLabel i -> sprintf " BNE %s" i
	| CallClo  (id,t) -> sprintf "	"


let rec asmt_to_asm a reg =
	match a with
	| LpasmtRPAREN a -> sprintf "%s" (asmt_to_asm a)
	| LetIdentEq (i,e2,a) -> sprintf "%s \n ADD %s, %s , %s \n %s" (exp_to_asm e2) (reg#look_for i) () 
	| Exp e -> 

let rec form_to_asm (l : string list)  reg : unit =
  match l with
    [] -> ()
  | x :: 

let rec function_to_asm exp reg =
	match exp with
	| LetUnderscEQ a -> 
	| LetLabeleqFloat (i,fl,fu) -> 
	| LetLabelEq (i,f,a,fu)-> 

let generate exp reg = 

	let asm_code = function_to_asm exp reg in asm_code


