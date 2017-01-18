

(*Not working yet DONT USE PLEASE *)


open Asml
open Printf 

let label_counter = 0 
let rec ident_or_imm_to_asm (l : ident_or_imm)  regf  =
match l  with 
	| Ident i-> sprintf "%s " (regf#look_for i)
	| Int i ->  "#"^ string_of_int (i)

let prologue = sprintf " stmfd  sp!, {fp, lr} \n add fp, sp, #4 \n sub sp, #n \n"

let epilogue = "sub sp, fp, #4 \n ldmfd  sp!, {fp, lr} \n bx lr \n"

let rec exp_to_asm exp regf =
 match exp with 
 	| Nop -> sprintf " "
	| LPexpRp e -> sprintf "(%s)" exp_to_asm exp regf
	| Int i -> sprintf "MOV R1 , #%d" i 
	| Ident i ->  sprintf "%s" (regf#look_for i)
	| Label s -> sprintf " %s" label
	| Neg i -> sprintf " "
	| FNeg i ->  sprintf " "
	| Add (i,id) -> sprintf "ADD %s , %s , %s " (regf#look_for i) (regf#look_for i) (ident_or_imm_to_asm id regf)
	| Sub (i,id) -> sprintf "SUB %s , %s " (regf#look_for i) (ident_or_imm_to_asm id regf )
	| Ld (i,id) -> sprintf "LDR %s , [%s] " (regf#look_for i) (ident_or_imm_to_asm id regf )
	| St (i1,id,i2) -> sprintf "STR %s, [%s, %s]" (regf#look_for i1) (ident_or_imm_to_asm id regf ) (regf#look_for i2)
	| FAdd (i,id) -> sprintf " "
	| FSub (i,id) -> sprintf " "
	| FMul (i,id) -> sprintf " "
	| FDiv (i,id) -> sprintf " "
	| New i -> sprintf " "
	| IfEq (i, id , t1, t2 ) -> sprintf "CMP %s , %s \n 
										BEQ labeltrue%d \n 
										%s \n
										BNE label_out%d  \n 
										labeltrue%d %s  \n
										label_out%s " (regf#look_for i) (ident_or_imm_to_asm id regf ) (label_counter) (exp_to_asm t2 regf) (label_counter) (label_counter)  (exp_to_asm t1 regf ) (i)
	| IfLEq (i, id , t1, t2 ) -> sprintf "CMP %s , %s \n 
										BLEQ labeltrue%d \n 
										%s \n
										BNE label_out%d  \n 
										labeltrue%d %s  \n
										label_out%s " (regf#look_for i) (ident_or_imm_to_asm id regf) (label_counter) (exp_to_asm t2 regf ) (label_counter) (label_counter)  (exp_to_asm t1 regf ) (i)
	| IfGEq (i, id , t1, t2 ) -> sprintf "CMP %s , %s \n 
										BGEQ labeltrue%d \n 
										%s \n
										BNE label_out%d  \n 
										labeltrue%d %s  \n
										label_out%s " (regf#look_for i) (ident_or_imm_to_asm id regf ) (label_counter) (exp_to_asm t2 regf) (label_counter) (label_counter)  (exp_to_asm t1 regf ) (i)
	| CallLabel (i,l)-> sprintf " BNE %s" i
	| CallClo  (id,t) -> sprintf "	"


let rec asmt_to_asm a reg =
	match a with
	| LpasmtRPAREN a -> sprintf "%s" (asmt_to_asm a reg)
	| LetIdentEq (i,e2,a) -> sprintf " %s \n ADD %s , %s , %s \n %s " (exp_to_asm e2 reg ) (reg#look_for i) ("R0") ("R1") (asmt_to_asm a reg)
	| Exp e -> exp_to_asm e reg 

let rec form_to_asm (l : string list)  reg : string =
  match l with
    [] -> ""
	  | x :: xs -> form_to_asm (reg#look_for x ) reg ^ " " ^(form_to_asm xs reg)

let rec function_to_asm exp reg =
	match exp with
	| LetUnderscEQ a -> sprintf " _main : %s " ( asmt_to_asm a (reg#look_for "_") ); 
	| LetLabeleqFloat (i,fl,fu) -> sprintf " "
	| LetLabelEq (i,f,a,fu)-> sprintf " "

let generate exp reg = 
	print_endline "***************ASSEMBLY ******************";
	let asm_code = function_to_asm exp reg in  
	print_endline asm_code;
	print_endline "******************************************"

