open Asml
open Printf 

let rec print_list_reg (myList : string list )  (listorigin : string list ) : string  = 
match myList with
 | [] ->  ""
 | head::body ->  if (head = (List.nth  (listorigin) ((List.length listorigin)-1)) )  then (head) ^ (print_list_reg body listorigin) else (head) ^ "," ^ (print_list_reg body listorigin)



let label_counter = ref(-1)
let reg_counter_out = ref(-1)
let reg_counter_in = ref(-1)

let rec move_parameters_out (f: string list) (reg : Register_alloc.registers_function) (l : string list) : string =
	match f with
    [] -> sprintf ""
	| x :: xs -> reg_counter_out:= !reg_counter_out +1;
					if (!reg_counter_out > 3) 
					then (
						let d = reg#look_for x in(
								if (d#get_is_in_stack = 1)
									then  (
											let top =  ((List.length l) * 4 ) - 16 in sprintf "LDR R0, [FP,#%d] \n  STR R0, [FP,#-%d]\n  %s"
											(top - (d#get_par_offset))(d#get_par_offset+8)(move_parameters_out xs reg l)
										 )
									else  (
										    let top =  ((List.length l) * 4 ) - 16 in  sprintf "LDR %s,[FP,#%d] \n  %s"
										    (d#get_reg)(top - (d#get_par_offset)) (move_parameters_out xs reg l )
										  )
							)
					)
					else (
						let d = reg#look_for x in (
							let z = !reg_counter_out in(
								if(d#get_is_in_stack = 1) 
									then ( printf " %s , %s : %d\n" x d#get_reg d#get_is_in_stack;
										sprintf "STR R%d, [FP,#-%d]\n  %s" (z)(d#get_offset+8)(move_parameters_out xs reg l )
									)
									else(										 
										sprintf "MOV %s, R%d\n  %s"(d#get_reg)(z)(move_parameters_out xs reg l)
									)
							)
						)
					)
	
let rec move_parameters_in (f:string list) (reg : Register_alloc.registers_function) : string = 
	match f with 
	[] -> sprintf ""
	| x :: xs -> reg_counter_in:= !reg_counter_in +1;
				if (!reg_counter_in > 3)
				then (  
					let d= reg#look_for x in( 
							if(d#get_is_in_stack = 1)
							then (
								sprintf" push [FP,#-%d]\n %s" (d#get_par_offset+8) (move_parameters_in xs reg)
							)
							else(
								sprintf "push {%s}\n  %s" (d#get_reg) (move_parameters_in xs reg)
							)
					)
				)
				else (
					let d= reg#look_for x in( 
						if(d#get_is_in_stack = 1)
						then (
							sprintf" STR %s, [FP,#-%d]\n %s" (d#get_reg )(d#get_par_offset+8) (move_parameters_in xs reg)
						)
						else(
							let z = !reg_counter_in in sprintf "MOV R%d, %s\n  %s" (z) (let v = reg#look_for x in v#get_reg) (move_parameters_in xs reg)
						)
					)
				)

let rec ident_or_imm_to_asm (l : ident_or_imm)  regf  =
match l  with 	
	| Ident i-> sprintf "%s " (let v = regf#look_for i in v#get_reg)
	| Int i ->  "#"^ string_of_int (i)

let prologue i = sprintf "STMFD  SP!, {FP,LR} \n  ADD FP, SP, #4 \n  SUB SP, #4 \n\n" 

let epilogue = sprintf "SUB SP, FP, #4 \n  LDMFD  SP!, {FP, LR} \n  BX LR \n\n  "

let rec exp_to_asm (exp : exp) (regf : Register_alloc.registers_function)  (resr : string) : string  = (*expression, variable->registers, register to put the result in*)
 match exp with 
 	| Nop -> sprintf ""
	| LPexpRp e -> sprintf "(%s)" (exp_to_asm exp regf resr)
	| Int i -> sprintf "MOV %s , #%d\n" (resr) (i) 
	| Ident i ->  let v = (regf#look_for i) in if (v#get_is_in_stack = 0) 
											   then  sprintf "MOV R0, %s\n" ( v#get_reg ) 
											   else sprintf "LDR R0, [FP,#-%d]\n" (v#get_offset)
	| Label s -> sprintf "LDR %s,=%s\n" (resr) (s)
	| Neg i ->  sprintf "MOV R1, #%s \n  NEG R1, R1\n" i 
	| FNeg i ->  sprintf " "
	| Add (i,id) -> let v = (regf#look_for i) in if (v#get_is_in_stack =0) 
												then (sprintf "ADD %s , %s , %s\n"(resr) (ident_or_imm_to_asm id regf) ( v#get_reg ))
												else ( match id with 
													|Int i-> sprintf "LDR R2, [FP,#-%d]\n ADD %s, %s, R2 " (v#get_offset) (resr) (ident_or_imm_to_asm id regf)
													|Ident i -> let x= (regf#look_for i) in if (v#get_is_in_stack = 0) 
																						then (sprintf "LDR R2, [FP,#-%d]\n ADD %s, %s, R2 " (v#get_offset) (resr) ( v#get_reg) )
																						else  (sprintf "LDR R2, [FP,#-%d]\nLDR R3, [FP,#-%d] ADD %s, R3, R2 " (v#get_offset)  (x#get_offset) (resr)) )
	| Sub (i,id) -> let v = (regf#look_for i) in if (v#get_is_in_stack =0) 
												then (sprintf "ADD %s , %s , %s\n"(resr) (ident_or_imm_to_asm id regf) ( v#get_reg ))
												else ( match id with 
													|Int i-> sprintf "LDR R2, [FP,#-%d]\n SUB %s, %s, R2 " (v#get_offset) (resr) (ident_or_imm_to_asm id regf)
													|Ident i -> let x= (regf#look_for i) in if (v#get_is_in_stack = 0) 
																						then (sprintf "LDR R2, [FP,#-%d]\n SUB %s, %s, R2 " (v#get_offset) (resr) ( v#get_reg) )
																						else  (sprintf "LDR R2, [FP,#-%d]\nLDR R3, [FP,#-%d] SUB %s, R3, R2 " (v#get_offset)  (x#get_offset) (resr)) ) 
	| Ld (i,id) -> sprintf ""
	| St (i1,id,i2) -> sprintf " " (*mem( i1 + ofset) <- i2*)
	| FAdd (i,id) -> sprintf " "
	| FSub (i,id) -> sprintf " "
	| FMul (i,id) -> sprintf " "
	| FDiv (i,id) -> sprintf " "
	| New i -> sprintf "LDR %s, addr_first_free\n  MOV R1, %s \n  ADD R1, R2, %s\n" (resr) (resr) (ident_or_imm_to_asm i regf) 
	| IfEq (i, id , t1, t2 ) -> incr label_counter ; sprintf "CMP %s , %s\n  BEQ labeltrue%d \n %s\n  B label_out%d  \nlabeltrue%d: \n  %s  \nlabel_out%d: \n  " 
	(let v = (regf#look_for i) in v#get_reg ) (ident_or_imm_to_asm id regf ) (!label_counter) (asmt_to_asm t2 regf) (!label_counter) (!label_counter)  (asmt_to_asm t1 regf ) (!label_counter)
	| IfLEq (i, id , t1, t2 ) ->incr label_counter; sprintf "CMP %s , %s\n  BLEQ labeltrue%d \n  %s\n  B label_out%d  \nlabeltrue%d: \n  %s  \nlabel_out%d: \n  " 
	(let v = (regf#look_for i) in v#get_reg ) (ident_or_imm_to_asm id regf) (!label_counter) (asmt_to_asm t2 regf ) (!label_counter) (!label_counter)  (asmt_to_asm t1 regf ) (!label_counter)
	| IfGEq (i, id , t1, t2 ) -> incr label_counter; sprintf "CMP %s , %s\n  BGEQ labeltrue%d \n  %s\n  B label_out%d \nlabeltrue%d: \n  %s  \nlabel_out%s: \n  " 
	(let v = (regf#look_for i) in v#get_reg ) (ident_or_imm_to_asm id regf ) (!label_counter) (asmt_to_asm t2 regf) (!label_counter) (!label_counter)  (asmt_to_asm t1 regf ) (i)
	| CallLabel (i,l)-> reg_counter_in := -1;sprintf "STMFD R13!, {%s} \n  %s\n    BL %s\n  LDMFD R13!, {%s}  \n  MOV %s, R0\n"
	 (print_list_reg regf#get_reg_used regf#get_reg_used) (move_parameters_in l regf ) (i)  (print_list_reg regf#get_reg_used regf#get_reg_used) (resr)
	| CallClo  (id,t) -> sprintf "MOV R0, %s \n  LDR %s,[%s]\n  BLX %s \n  MOV %s, R0" (let v = (regf#look_for (List.hd t)) in v#get_reg ) (let v = (regf#look_for id) in v#get_reg ) (let v =  (regf#look_for id) in v#get_reg) (let v =  (regf#look_for id) in v#get_reg) (resr)
and asmt_to_asm a reg =
	match a with
	| LpasmtRPAREN a -> sprintf "%s\n" (asmt_to_asm a reg)
	| LetIdentEq (i,e2,a) ->  let v = reg#look_for i  in if (v#get_is_in_stack = 0) 
													  then sprintf "%s\n  %s\n " (exp_to_asm e2 reg ( v#get_reg))  (asmt_to_asm a reg)
													  else sprintf "LDR R1, [FP,#-%d]\n  %s\n  %s\n " (v#get_offset)(exp_to_asm e2 reg "R1")  (asmt_to_asm a reg)
	| Exp e -> exp_to_asm e reg "R0"

let rec form_to_asm (l : string list)  reg : string =
  match l with
    [] -> ""
	  | x :: xs -> form_to_asm (reg#look_for x ) reg ^ " " ^(form_to_asm xs reg)



let rec function_to_asm exp reg =
	match exp with
	| LetUnderscEQ a -> sprintf ".data\n  .balign 4\n  heap: .skip 20000\n .text   \n  .global _start \n_start: \n %s\n\n\n addr_of_heap : .word heap\n addr_first_free: .word heap\n" ( asmt_to_asm a (reg#look_for "_") ); 
	| LetLabeleqFloat (i,fl,fu) -> sprintf " "
	| LetLabelEq (i,f,a,fu)-> reg_counter_out := -1;sprintf "%s:\n  %s  %s \n  %s  %s \n  %s\n" 
	(i) (prologue 2) (move_parameters_out f (reg#look_for i) f) (asmt_to_asm a (reg#look_for i)) (epilogue) (function_to_asm fu reg)

let generate exp reg = 
	function_to_asm exp reg

