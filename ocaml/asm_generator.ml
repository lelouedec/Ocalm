open Asml
open Printf 

let label_counter = ref(-1)
let reg_counter = ref(-1)

let rec move_parameters (f: string list) reg : string =
	match f with
    [] -> sprintf "  "
	| x :: xs -> incr reg_counter ; sprintf "MOV %s, R%d \n  %s" (reg#look_for x) (!reg_counter)  (move_parameters xs reg)


let rec ident_or_imm_to_asm (l : ident_or_imm)  regf  =
match l  with 
	| Ident i-> sprintf "%s " (regf#look_for i)
	| Int i ->  "#"^ string_of_int (i)

let prologue i = sprintf "STMFD  R13!, {R11,R14} \n  ADD R11, R13, #4 \n  SUB R13, #4 \n\n" 

let epilogue = sprintf "SUB R13, R11, #4 \n  LDMFD  R13!, {R11, R14} \n  BX R14 \n\n  "

let rec exp_to_asm (exp : exp) (regf : Register_alloc.registers_function)  (resr : string) : string  = (*expression, variable->registers, register to put the result in*)
 match exp with 
 	| Nop -> sprintf ""
	| LPexpRp e -> sprintf "(%s)" (exp_to_asm exp regf resr)
	| Int i -> sprintf "MOV %s , #%d\n  " (resr) (i) 
	| Ident i ->  sprintf "MOV R0, %s\n  " (regf#look_for i)
	| Label s -> sprintf "LDR %s,=%s\n  " (resr) (s)
	| Neg i -> sprintf "MOV R1, #%s \n  NEG R1, R1\n  " i 
	| FNeg i ->  sprintf " "
	| Add (i,id) -> sprintf "ADD %s , %s , %s\n  " (resr) (ident_or_imm_to_asm id regf) (regf#look_for i)
	| Sub (i,id) -> sprintf "SUB %s , %s, %s\n  " (resr) (regf#look_for i) (ident_or_imm_to_asm id regf ) 
	| Ld (i,id) -> if i = "%self" then sprintf "LDR %s , [FP,%s]\n  " (resr)(ident_or_imm_to_asm id regf ) else sprintf "LDR %s , [%s,%s]\n  " (resr)(regf#look_for i) (ident_or_imm_to_asm id regf )
	| St (i1,id,i2) -> sprintf "STR %s, [%s, %s]\n  " (regf#look_for i2) (regf#look_for i1) (ident_or_imm_to_asm id regf) (*mem( i1 + ofset) <- i2*)
	| FAdd (i,id) -> sprintf " "
	| FSub (i,id) -> sprintf " "
	| FMul (i,id) -> sprintf " "
	| FDiv (i,id) -> sprintf " "
	| New i -> sprintf "LDR %s, addr_first_free\n  MOV R1, %s \n  ADD R1, R2, %s\n" (resr) (resr) (ident_or_imm_to_asm i regf) 
	| IfEq (i, id , t1, t2 ) -> incr label_counter ; sprintf "CMP %s , %s\n  BEQ labeltrue%d \n %s\n  B label_out%d  \nlabeltrue%d: \n  %s  \nlabel_out%d: \n  " 
	(regf#look_for i) (ident_or_imm_to_asm id regf ) (!label_counter) (asmt_to_asm t2 regf) (!label_counter) (!label_counter)  (asmt_to_asm t1 regf ) (!label_counter)
	| IfLEq (i, id , t1, t2 ) ->incr label_counter; sprintf "CMP %s , %s\n  BLEQ labeltrue%d \n  %s\n  B label_out%d  \nlabeltrue%d: \n  %s  \nlabel_out%d: \n  " 
	(regf#look_for i) (ident_or_imm_to_asm id regf) (!label_counter) (asmt_to_asm t2 regf ) (!label_counter) (!label_counter)  (asmt_to_asm t1 regf ) (!label_counter)
	| IfGEq (i, id , t1, t2 ) -> incr label_counter; sprintf "CMP %s , %s\n  BGEQ labeltrue%d \n  %s\n  B label_out%d \nlabeltrue%d: \n  %s  \nlabel_out%s: \n  " 
	(regf#look_for i) (ident_or_imm_to_asm id regf ) (!label_counter) (asmt_to_asm t2 regf) (!label_counter) (!label_counter)  (asmt_to_asm t1 regf ) (i)
	| CallLabel (i,l)-> sprintf "MOV R0, %s \n STMFD R13!, {R4-R8} \n  BL %s\n LDMFD R13!, {R4-R8}  \n  MOV %s, R0\n" (regf#look_for (List.hd l) ) (i) (resr)
	| CallClo  (id,t) -> sprintf "MOV R0, %s \n  LDR %s,[%s]\n  BLX %s \n  MOV %s, R0" (regf#look_for (List.hd t) ) (regf#look_for id) (regf#look_for id) (regf#look_for id) (resr)
and asmt_to_asm a reg =
	match a with
	| LpasmtRPAREN a -> sprintf "%s\n" (asmt_to_asm a reg)
	| LetIdentEq (i,e2,a) ->  sprintf "%s\n  %s\n " (exp_to_asm e2 reg (reg#look_for i)) (asmt_to_asm a reg)
	| Exp e -> exp_to_asm e reg "R0"

let rec form_to_asm (l : string list)  reg : string =
  match l with
    [] -> ""
	  | x :: xs -> form_to_asm (reg#look_for x ) reg ^ " " ^(form_to_asm xs reg)



let rec function_to_asm exp reg =
	match exp with
	| LetUnderscEQ a -> sprintf ".data\n  .balign 4\n  heap: .skip 20000\n .text   \n  .global _start \n_start: \n %s\n\n\n addr_of_heap : .word heap\n addr_first_free: .word heap\n" ( asmt_to_asm a (reg#look_for "_") ); 
	| LetLabeleqFloat (i,fl,fu) -> sprintf " "
	| LetLabelEq (i,f,a,fu)-> sprintf "%s:\n  %s  %s \n  %s  %s \n  %s\n" (i) (prologue 2) (move_parameters f (reg#look_for i)) (asmt_to_asm a (reg#look_for i)) (epilogue) (function_to_asm fu reg)

let generate exp reg = 
	function_to_asm exp reg

