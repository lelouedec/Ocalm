open Asml
open Printf 

let rec print_list_reg (myList : string list )  (listorigin : string list ) : string  = 
match myList with
 | [] ->  ""
 | head::body ->  if (head = (List.nth  (listorigin) ((List.length listorigin)-1)) )  then (head) ^ (print_list_reg body listorigin) else (head) ^ "," ^ (print_list_reg body listorigin)

let self =ref("")

let label_counter = ref(-1)
let reg_counter_out = ref(-1)
let reg_counter_in = ref(-1)

let rec move_parameters_out (f: string list) (reg : Register_alloc.registers_function) (l : string list) : string =
	match f with
    [] -> reg_counter_out := -1;sprintf ""
	| x :: xs -> incr reg_counter_out; let z = !reg_counter_out in 
					if (z > 3) 
					then (
						let d = reg#look_for x in(
								if (d#get_is_in_stack = 1)
									then  (
											let top =  ((List.length l) * 4 ) - 16 in sprintf "LDR R0, [FP,#%d] \n  STR R0, [FP,#-%d]\n  %s"
											(top - (d#get_par_offset))(d#get_offset)(move_parameters_out xs reg l)
										 )
									else  (
										    let top =  ((List.length l) * 4 ) - 16 in  sprintf "LDR %s,[FP,#%d] \n  %s"
										    (d#get_reg)(top - (d#get_par_offset)) (move_parameters_out xs reg l )
										  )
							)
					)
					else (
						let d = reg#look_for x in (
								if(d#get_is_in_stack = 1) 
									then ( 
										sprintf "STR R%d, [FP,#-%d]\n  %s" (z)(d#get_offset)(move_parameters_out xs reg l )
									)
									else(										 
										sprintf "MOV %s, R%d\n  %s"(d#get_reg)(z)(move_parameters_out xs reg l)
									)
						)
					)
	
let rec move_parameters_in (f:string list) (reg : Register_alloc.registers_function) : string = 
	match f with 
	[] -> sprintf ""
	| x :: xs -> incr reg_counter_in; let z = !reg_counter_in in
				if (z > 3)
				then (  
					let d= reg#look_for x in( 
							if(d#get_is_in_stack = 1)
							then (
								sprintf" push {[FP,#-%d]}\n %s" (d#get_par_offset+4) (move_parameters_in xs reg)
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
							sprintf" STR %s, [FP,#-%d]\n %s" (d#get_reg )(d#get_offset) (move_parameters_in xs reg)
						)
						else(
							sprintf "MOV R%d, %s\n  %s" (z) (d#get_reg) (move_parameters_in xs reg)
						)
					)
				)

let rec ident_or_imm_to_asm (l : ident_or_imm)  regf  =
match l  with 	
	| Ident i-> sprintf "%s " (let v = regf#look_for i in v#get_reg)
	| Int i ->  "#"^ string_of_int (i)

let prologue i = sprintf "STMFD  SP!, {FP,LR} \n  ADD FP, SP, #4 \n  SUB SP, #%d \n" (i*4)

let epilogue = sprintf "SUB SP, FP, #4 \n  LDMFD  SP!, {FP, LR} \n  BX LR \n"



let rec exp_to_asm (exp : exp) (regf : Register_alloc.registers_function)  (resr : string) : string  = (*expression, variable->registers, register to put the result in*)
 match exp with 
 	| Nop -> sprintf ""
	| LPexpRp e -> sprintf "(%s)" (exp_to_asm exp regf resr)
	| Int i ->  if (i < 250 ) then sprintf "MOV %s , #%d\n" (resr) (i) else  sprintf "LDR %s , =#%d\n" (resr) (i)
	| Ident i ->  let v = (regf#look_for i) in if (v#get_is_in_stack = 0) 
											   then  sprintf "MOV R0, %s\n" ( v#get_reg ) 
											   else sprintf "LDR R0, [FP,#-%d]\n" (v#get_offset)
	| Label s -> sprintf "LDR %s,=%s\n" (resr) (s)
	| Neg i ->  let x = regf#look_for i in if(x#get_is_in_stack = 0) then (sprintf "NEG %s, %s\n" ) (resr)(x#get_reg)
										else (sprintf "LDR R1, [FP,#-%d]\n  NED R1, R1\n  STR R1, [FP,#-%d]\n"(x#get_offset) (x#get_offset))
	| FNeg i ->  sprintf " "
	| Add (i,id) -> let v = (regf#look_for i) in if (v#get_is_in_stack =0) 
												then (match id with 
													|Int i -> (sprintf "MOV R3, #%d\n  ADD %s , R3 , %s\n" (i) (resr) ( v#get_reg ))
													|Ident i -> let x= (regf#look_for i) in if (x#get_is_in_stack = 0) 
																						then (sprintf " ADD %s, %s, %s\n" (resr) (v#get_reg) ( x#get_reg) )
																						else  (sprintf "LDR R3, [FP,#-%d]\n  ADD %s, %s , R3" (x#get_offset) (resr) (v#get_reg)) )
												else ( match id with 
													|Int i-> sprintf "LDR R2, [FP,#-%d]\n ADD %s, %s, R2 " (v#get_offset) (resr) (ident_or_imm_to_asm id regf)
													|Ident i -> let x= (regf#look_for i) in if (x#get_is_in_stack = 0) 
																						then (sprintf "LDR R2, [FP,#-%d]\n  ADD %s, %s, R2 " (v#get_offset) (resr) ( x#get_reg) )
																						else  (sprintf "LDR R2, [FP,#-%d]\n  LDR R3, [FP,#-%d]\n  ADD %s, R3, R2 " (v#get_offset)  (x#get_offset) (resr)) )
	| Sub (i,id) -> let v = (regf#look_for i) in if (v#get_is_in_stack =0) 
												then (match id with 
													|Int i -> (sprintf "MOV R3, #%d\n  SUB %s , %s , R3\n" (i) (resr) ( v#get_reg ))
													|Ident i -> let x= (regf#look_for i) in if (x#get_is_in_stack = 0) 
																						then (sprintf " SUB %s, %s, %s\n" (resr)   (v#get_reg) (x#get_reg)  )
																						else  (sprintf "LDR R2, [FP,#-%d]\n  SUB %s, %s, R2 " (x#get_offset)  (resr) (v#get_reg) ) )
												else ( match id with 
													|Int i-> sprintf "LDR R2, [FP,#-%d]\n  SUB %s, %s, R2\n" (v#get_offset) (resr) (ident_or_imm_to_asm id regf)
													|Ident i -> let x= (regf#look_for i) in if (x#get_is_in_stack = 0) 
																						then (sprintf "LDR R2, [FP,#-%d]\n  SUB %s, R2, %s\n" (v#get_offset) (resr) ( x#get_reg) )
																						else  (sprintf "LDR R2, [FP,#-%d]\n  LDR R3, [FP,#-%d]\n  SUB %s, R2, R3 " (v#get_offset)  (x#get_offset) (resr)) )
	| Ld (i,id) -> if (i="%self") then sprintf "LDR %s, =self \n  LDR %s,[%s]\n  LDR %s, [%s,%s]" (resr) (resr)  (resr) (resr) (resr) (ident_or_imm_to_asm id regf) else 
					let x = regf#look_for i in
					if (x#get_is_in_stack = 0 )
					then ( sprintf "LDR %s, [%s,#%s]" (resr) (x#get_reg) (ident_or_imm_to_asm id regf)	)
					else (sprintf "LDR R3, [FP;#-%d] \n  LDR %s, [%s,#%s]" (x#get_offset) (resr) (x#get_reg) (ident_or_imm_to_asm id regf)		)
	| St (i1,id,i2) -> let x = regf#look_for i1 in 
					if (x#get_is_in_stack = 0 )
					then ( let y = regf#look_for i2	 in 
						if (y#get_is_in_stack = 0)
						then (sprintf "STR %s, [%s,%s]\n" (y#get_reg) (x#get_reg) (ident_or_imm_to_asm id regf)  )
						else ( sprintf "LDR R3, [FP,#-%d]\n STR R3, [%s,%s]") (y#get_offset) (x#get_reg) (ident_or_imm_to_asm id regf)) 
					else (let y = regf#look_for i2	 in 
						if (y#get_is_in_stack = 0)
						then (sprintf "LDR R3, [FP,#-%d]\n  STR R3, [%s,#%s\n" (x#get_offset) (y#get_reg) (ident_or_imm_to_asm id regf))
						else (sprintf "LDR R2, [FP,#-%d]\n LDR R3, [FP,#-%d]\n STR R2, [R3,#%s]\n") (y#get_offset) (x#get_offset) (ident_or_imm_to_asm id regf) ) (*mem( i1 + ofset) <- i2*)
	| FAdd (i,id) -> sprintf " "
	| FSub (i,id) -> sprintf " "
	| FMul (i,id) -> sprintf " "
	| FDiv (i,id) -> sprintf " "
	| New i -> sprintf "LDR %s, addr_first_free\n  MOV R1, %s \n  MOV R2,R1\n  ADD R1, R2, %s\n STR R1, [R2]" (resr) (resr) (ident_or_imm_to_asm i regf) 
	| IfEq (i, id , t1, t2 ) -> doIf i id t1 t2 "BEQ" regf resr
	| IfLEq (i, id , t1, t2 ) ->doIf i id t1 t2 "BLE" regf resr
	| IfGEq (i, id , t1, t2 ) -> doIf i id t1 t2 "BGE" regf  resr
	| CallLabel (i,l)-> reg_counter_in := -1;sprintf " STMFD SP!, {R1,R2,R3}\n %s\n  BL %s\n LDMFD SP!, {R1,R2,R3}\n  MOV %s, R0\n"
	  (move_parameters_in l regf ) (i)   (resr)
	| CallClo  (id,t) -> reg_counter_in := -1; let x = regf#look_for id in 
				if(x#get_is_in_stack = 0) 
				then (sprintf "STMFD SP!, {R1,R2,R3}\n  LDR R2, =self\n  STR %s,[R2]\n%s\n  LDR %s,[%s]\n  BLX %s \n  LDMFD SP!, {R1,R2,R3}\n  MOV %s, R0" 
				   (x#get_reg) (move_parameters_in t regf ) (x#get_reg ) (x#get_reg) (x#get_reg) (resr))
				else (sprintf "STMFD SP!, {R1,R2,R3}\n  LDR R2, =self\n  STR %s,[R2]\n %s \n LDR %s,[%s]\n  BLX %s \n  LDMFD SP!, {R1,R2,R3}\n  MOV %s, R0\n  " 
					(x#get_reg) (move_parameters_in t regf ) (x#get_reg ) (x#get_reg) (x#get_reg) (resr))
and asmt_to_asm a reg resr=
	match a with
	| LpasmtRPAREN a -> sprintf "%s\n" (asmt_to_asm a reg resr)
	| LetIdentEq (i,e2,a) ->  let v = reg#look_for i  in if (v#get_is_in_stack = 0) 
													  then sprintf "%s\n  %s\n " (exp_to_asm e2 reg (v#get_reg))  (asmt_to_asm a reg resr )
													  else sprintf "%s\n  STR R0, [FP,#-%d]\n  %s\n " (exp_to_asm e2 reg "R0")(v#get_offset)  (asmt_to_asm a reg "R0")
	| Exp e ->  exp_to_asm e reg resr
and doIf i (id : ident_or_imm) t1 t2 cmp regf resr: string = incr label_counter; let count = !label_counter in 
	let v = regf#look_for i in if (v#get_is_in_stack = 0)
		then ( incr label_counter ; match id with
			|Int i ->  sprintf "CMP %s , #%d\n  %s labeltrue%d \n %s\n  B label_out%d  \nlabeltrue%d: \n  %s label_out%d: \n" 	
			(v#get_reg ) (i) (cmp) (count) (asmt_to_asm t2 regf resr) (count) (count)  (asmt_to_asm t1 regf resr) (count)
			|Ident i -> let x = regf#look_for i in if (x#get_is_in_stack= 0 )
			then (sprintf "CMP %s , %s\n  %s labeltrue%d \n %s\n  B label_out%d  \nlabeltrue%d: \n  %s  label_out%d: \n" 	
			(v#get_reg ) (x#get_reg) (cmp) (count) (asmt_to_asm t2 regf resr) (count) (count)  (asmt_to_asm t1 regf resr) (count))
			else(sprintf "LDR R2, [FP,#-%d] \n  CMP %s , R2\n  %s labeltrue%d \n %s\n  B label_out%d  \nlabeltrue%d: \n  %s  label_out%d: \n" 
			(x#get_offset)(v#get_reg )(cmp)(count)  (asmt_to_asm t2 regf resr) (count) (count)  (asmt_to_asm t1 regf resr ) (count))
		)
		else ( match id with
		| Int i -> sprintf "LDR R3, [FP,#-%d]\n  CMP R3 , #%d\n  %s labeltrue%d \n %s\n  B label_out%d  \nlabeltrue%d: \n  %s  label_out%d: \n" 	
			(v#get_offset) (i) (cmp) (count) (asmt_to_asm t2 regf resr) (count) (count)  (asmt_to_asm t1 regf resr ) (count)
		| Ident i -> let x = regf#look_for i in if(x#get_is_in_stack = 0)
			then (sprintf " LDR R3, [FP,#-%d]\n  CMP R3 , %s\n  %s labeltrue%d \n %s\n  B label_out%d  \nlabeltrue%d: \n  %s  label_out%d: \n" 	
			(v#get_offset) (x#get_reg) (cmp) (count) (asmt_to_asm t2 regf resr) (count) (count)  (asmt_to_asm t1 regf resr) (count))
			else (sprintf " LDR R2, [FP,#-%d]\n  LDR R3, [FP,#-%d]\n  CMP R3 , R2\n  %s labeltrue%d \n %s\n  B label_out%d  \nlabeltrue%d: \n  %s  label_out%d: \n" 	
			(v#get_offset) (x#get_offset) (cmp) (count) (asmt_to_asm t2 regf resr) (count) (count)  (asmt_to_asm t1 regf resr) (count))
		) 

let rec form_to_asm (l : string list)  reg : string =
  match l with
    [] -> ""
	  | x :: xs -> form_to_asm (reg#look_for x ) reg ^ " " ^(form_to_asm xs reg)



let rec function_to_asm exp reg =
	match exp with
	| LetUnderscEQ a -> sprintf "_start: \n%s %s\n\n\n addr_of_heap : .word heap\n addr_first_free: .word heap\n"(prologue (let s = reg#look_for "_" in s#get_var_on_stack)) ( asmt_to_asm a (reg#look_for "_") "R0" ); 
	| LetLabeleqFloat (i,fl,fu) -> sprintf " "
	| LetLabelEq (i,f,a,fu)-> reg_counter_out := -1;sprintf "%s:\n  %s\nSTMFD R13!, {%s} \n  %s \n  %s  \n  LDMFD R13!, {%s}  \n\n  %s \n  %s" 
	(i) (prologue (let s = reg#look_for i in s#get_var_on_stack)) (print_list_reg (let x =reg#look_for i in x#get_reg_used) (let x =reg#look_for i in x#get_reg_used))  (move_parameters_out f (reg#look_for i) f) (asmt_to_asm a (reg#look_for i) "R0") (print_list_reg (let q = reg#look_for i in q#get_reg_used)(let q = reg#look_for i in q#get_reg_used)) (epilogue) (function_to_asm fu reg)

let generate exp reg = 
	let x = function_to_asm exp reg in 
	".data\n  .balign 4\n  heap: .skip 20000\n.balign 4\n  self : .word 0x0\n .text   \n  .global _start \n" ^ x

