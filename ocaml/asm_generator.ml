open Asml
open Printf 

let label_counter = ref(-1)

let rec ident_or_imm_to_asm (l : ident_or_imm)  regf  =
match l  with 
	| Ident i-> sprintf "%s " (regf#look_for i)
	| Int i ->  "#"^ string_of_int (i)

let prologue i = sprintf " stmfd  R13!, {R11,R14} \n add R11, R13, #4 \n sub R13, #4 \n" 

let epilogue = sprintf "sub R13, R11, #4 \n ldmfd  R13!, {R11, R14} \n bx R14 \n"

let rec exp_to_asm (exp : exp) (regf : Register_alloc.registers_function)  (resr : string) : string  = (*expression, variable->registers, register to put the result in*)
 match exp with 
 	| Nop -> sprintf "MOV R0, R0"
	| LPexpRp e -> sprintf "(%s)" (exp_to_asm exp regf resr)
	| Int i -> sprintf "MOV %s , #%d" (resr) (i) 
	| Ident i ->  sprintf "%s" (regf#look_for i)
	| Label s -> sprintf "%s" (s)
	| Neg i -> sprintf "  MOV R1, #%s \n  NEG R1, R1 " i 
	| FNeg i ->  sprintf " "
	| Add (i,id) -> sprintf "ADD %s , %s , %s \n  MOV %s, %s \n  " (regf#look_for i) (regf#look_for i) (ident_or_imm_to_asm id regf) (resr) (regf#look_for i)
	| Sub (i,id) -> sprintf "SUB %s , %s " (regf#look_for i) (ident_or_imm_to_asm id regf )
	| Ld (i,id) -> sprintf "  LDR %s , [%s] " (regf#look_for i) (ident_or_imm_to_asm id regf )
	| St (i1,id,i2) -> sprintf "  STR %s, [%s, %s]" (regf#look_for i1) (ident_or_imm_to_asm id regf ) (regf#look_for i2)
	| FAdd (i,id) -> sprintf " "
	| FSub (i,id) -> sprintf " "
	| FMul (i,id) -> sprintf " "
	| FDiv (i,id) -> sprintf " "
	| New i -> sprintf " "
	| IfEq (i, id , t1, t2 ) -> incr label_counter ; sprintf "CMP %s , %s\n  BEQ labeltrue%d \n %s\n  BNE label_out%d  \nlabeltrue%d: \n  %s  \nlabel_out%d: \n" (regf#look_for i) (ident_or_imm_to_asm id regf ) (!label_counter) (asmt_to_asm t2 regf) (!label_counter) (!label_counter)  (asmt_to_asm t1 regf ) (!label_counter)
	| IfLEq (i, id , t1, t2 ) ->incr label_counter; sprintf "CMP %s , %s\n  BLEQ labeltrue%d \n  %s\n  BNE label_out%d  \nlabeltrue%d: \n  %s  \nlabel_out%d: \n" (regf#look_for i) (ident_or_imm_to_asm id regf) (!label_counter) (asmt_to_asm t2 regf ) (!label_counter) (!label_counter)  (asmt_to_asm t1 regf ) (!label_counter)
	| IfGEq (i, id , t1, t2 ) -> incr label_counter; sprintf "CMP %s , %s\n  BGEQ labeltrue%d \n  %s\n  BNE label_out%d \nlabeltrue%d: \n  %s  \nlabel_out%s: \n" (regf#look_for i) (ident_or_imm_to_asm id regf ) (!label_counter) (asmt_to_asm t2 regf) (!label_counter) (!label_counter)  (asmt_to_asm t1 regf ) (i)
	| CallLabel (i,l)-> sprintf "MOV R0, %s \n  BL %s  \n  MOV %s, R0" (regf#look_for (List.hd l) ) (i) (resr)
	| CallClo  (id,t) -> sprintf "  "
and asmt_to_asm a reg =
	match a with
	| LpasmtRPAREN a -> sprintf "%s" (asmt_to_asm a reg)
	| LetIdentEq (i,e2,a) ->  sprintf "%s\n  %s " (exp_to_asm e2 reg (reg#look_for i)) (asmt_to_asm a reg)
	| Exp e -> exp_to_asm e reg "R1"

let rec form_to_asm (l : string list)  reg : string =
  match l with
    [] -> ""
	  | x :: xs -> form_to_asm (reg#look_for x ) reg ^ " " ^(form_to_asm xs reg)

let move_parameters (f: string list) reg : string =
	sprintf"MOV %s, R0 " (reg#look_for (List.hd f)) 


let rec function_to_asm exp reg =
	match exp with
	| LetUnderscEQ a -> sprintf "  .text   \n  .global _start \n_start: \n  %s\n" ( asmt_to_asm a (reg#look_for "_") ); 
	| LetLabeleqFloat (i,fl,fu) -> sprintf " "
	| LetLabelEq (i,f,a,fu)-> sprintf "%s:\n%s \n %s \n%s %s\n \n%s" (i) (prologue 2) (move_parameters f (reg#look_for i)) (asmt_to_asm a (reg#look_for i)) (epilogue) (function_to_asm fu reg)

let generate exp reg = 
	function_to_asm exp reg

