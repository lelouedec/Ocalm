

(*Not working yet DONT USE PLEASE *)


open Asml
open Printf 

let label_counter = 0 
let rec ident_or_imm_to_asm (l : ident_or_imm)  regf  =
match l  with 
	| Ident i-> sprintf "%s " (regf#look_for i)
	| Int i ->  "#"^ string_of_int (i)

let prologue i = sprintf " stmfd  R13!, {R11,R14} \n add R11, R13, #4 \n sub R13, #4 \n" 

let epilogue = sprintf "sub R13, R11, #4 \n ldmfd  R13!, {R11, R14} \n bx R14 \n"

let rec exp_to_asm exp regf =
 match exp with 
 	| Nop -> sprintf "MOV R0, R0"
	| LPexpRp e -> sprintf "(%s)" (exp_to_asm exp regf)
	| Int i -> sprintf "  MOV R1 , #%d" (i) 
	| Ident i ->  sprintf "%s" (regf#look_for i)
	| Label s -> sprintf "%s" (s)
	| Neg i -> sprintf "  MOV R1, #%s \n  NEG R1, R1 " i 
	| FNeg i ->  sprintf " "
	| Add (i,id) -> sprintf "  ADD %s , %s , %s " (regf#look_for i) (regf#look_for i) (ident_or_imm_to_asm id regf)
	| Sub (i,id) -> sprintf "  SUB %s , %s " (regf#look_for i) (ident_or_imm_to_asm id regf )
	| Ld (i,id) -> sprintf "  LDR %s , [%s] " (regf#look_for i) (ident_or_imm_to_asm id regf )
	| St (i1,id,i2) -> sprintf "  STR %s, [%s, %s]" (regf#look_for i1) (ident_or_imm_to_asm id regf ) (regf#look_for i2)
	| FAdd (i,id) -> sprintf " "
	| FSub (i,id) -> sprintf " "
	| FMul (i,id) -> sprintf " "
	| FDiv (i,id) -> sprintf " "
	| New i -> sprintf " "
	| IfEq (i, id , t1, t2 ) -> sprintf "CMP %s , %s \n BEQ labeltrue%d \n %s \n  BNE label_out%d  \n  labeltrue%d %s  \n  label_out%d \n  " (regf#look_for i) (ident_or_imm_to_asm id regf ) (label_counter) (asmt_to_asm t2 regf) (label_counter) (label_counter)  (asmt_to_asm t1 regf ) (label_counter)
	| IfLEq (i, id , t1, t2 ) -> sprintf "CMP %s , %s \n 
										BLEQ labeltrue%d \n 
										%s \n
										BNE label_out%d  \n 
										labeltrue%d %s  \n
										label_out%d " (regf#look_for i) (ident_or_imm_to_asm id regf) (label_counter) (asmt_to_asm t2 regf ) (label_counter) (label_counter)  (asmt_to_asm t1 regf ) (label_counter)
	| IfGEq (i, id , t1, t2 ) -> sprintf "CMP %s , %s \n 
										BGEQ labeltrue%d \n 
										%s 
										\nBNE label_out%d  
										\nlabeltrue%d %s  \n
										label_out%s " (regf#look_for i) (ident_or_imm_to_asm id regf ) (label_counter) (asmt_to_asm t2 regf) (label_counter) (label_counter)  (asmt_to_asm t1 regf ) (i)
	| CallLabel (i,l)-> sprintf "Mov R0, #0\n  ADD R0, R0, %s \n BL %s" (regf#look_for (List.hd l) ) (i)
	| CallClo  (id,t) -> sprintf "  "
and asmt_to_asm a reg =
	match a with
	| LpasmtRPAREN a -> sprintf "%s" (asmt_to_asm a reg)
	| LetIdentEq (i,e2,a) -> sprintf "%s \n  ADD %s , %s , %s \n  %s" (exp_to_asm e2 reg ) (reg#look_for i) ("R0") ("R1") (asmt_to_asm a reg)
	| Exp e -> exp_to_asm e reg 

let rec form_to_asm (l : string list)  reg : string =
  match l with
    [] -> ""
	  | x :: xs -> form_to_asm (reg#look_for x ) reg ^ " " ^(form_to_asm xs reg)

let move_parameters (f: string list) reg : string =
	sprintf" MOV %s , #0 \n  ADD %s,%s,R0 " (reg#look_for (List.hd f)) (reg#look_for (List.hd f)) (reg#look_for (List.hd f))


let rec function_to_asm exp reg =
	match exp with
	| LetUnderscEQ a -> sprintf "  .text   \n  .global _start \n_start: \n%s\n" ( asmt_to_asm a (reg#look_for "_") ); 
	| LetLabeleqFloat (i,fl,fu) -> sprintf " "
	| LetLabelEq (i,f,a,fu)-> sprintf "%s:\n %s \n%s \n%s" (i) (move_parameters f (reg#look_for i)) (asmt_to_asm a (reg#look_for i)) (function_to_asm fu reg)

let generate exp reg = 
	function_to_asm exp reg

