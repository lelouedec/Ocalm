
open Printf
open Asml

class variable =
object(self)
val mutable register : string = "";
val mutable occurences : int = 0;
val mutable first_use = 0;
val mutable last_use = 0;
val mutable is_in_stack = 0;
val mutable offset = 0;
val mutable par_offset = 0;
method get_reg =
	register
method get_occ =
	occurences
method set_register x = 
	register <- x 
method incr_occ y = 
	occurences <- occurences + y
method set_first_u x = 
	first_use <- x
method set_last_u y =
	last_use <- y
method display_timeslice = 
	print_endline (string_of_int first_use ^"-> " ^ string_of_int last_use)
method get_first_use = 
	first_use
method get_last_use =
	last_use
method  set_in_stack =
	is_in_stack <- 1  
method get_is_in_stack = 
	is_in_stack
method get_offset = 
	offset
method set_offset x =
	offset <- x
method get_par_offset = 
	par_offset
method set_par_offset x =
	par_offset <- x
end ;; 

type t = (string, variable ) Hashtbl.t;;

class registers_function = 
	object (self)
	val mutable register_hash : t = Hashtbl.create 42;
	val mutable counter = 4;
	val mutable reg_to_save : string list = [];
	val mutable line = 0;
	val mutable c_offset = -16
	method get_reg_used = 
		reg_to_save
	method add_reg_to_save (x:string): unit =
    	let is_here = ref(0) in 
    	List.iter(fun r -> if ( x = r ) then incr is_here) reg_to_save;
    	if !is_here == 0 then reg_to_save <- reg_to_save @ [x] 

	method add x t =
	 if (Hashtbl.mem register_hash x) = true  
	 then let c = self#look_for x in c#incr_occ 1; c#set_last_u line; 
	 else (let y = new variable in  y#set_register ("R"^ string_of_int counter); 
	 							  y#incr_occ 1 ;
	 							  y#set_first_u line ; 
	 							  y#set_last_u line; 
	 							  Hashtbl.replace register_hash x  y;
	 							  self#add_reg_to_save (y#get_reg);
	 							  if counter = 10 then counter <- 12 else counter <- counter + 1;
	 							  if t == 1 then y#set_par_offset c_offset; c_offset <- c_offset + 4);
	 if counter > 12 then counter <- 4
	method look_for x = 			 
			 Hashtbl.find register_hash x 
	method get_hast =
				register_hash
	method clear =
		Hashtbl.clear register_hash;
		counter <- 4
    method incr_line = 
    	line <- line+1
    method display_nb_line =
    	print_endline (string_of_int line)

	end;;

type z = (string, registers_function) Hashtbl.t;;

class functions_register_hash = 
	object (self)
	val mutable functions_hash : z = Hashtbl.create 42;
	method add x =
	 if (Hashtbl.mem functions_hash x) == true then	() 
												else (let func = new registers_function in  Hashtbl.replace functions_hash (x)  (func) ); 
	method look_for x = 
			Hashtbl.find functions_hash x 
	method get_hast =
				functions_hash
	method clear =
		Hashtbl.clear functions_hash;
	end;;


let function_has = new functions_register_hash

let rec assign_ident_or_imm (l : ident_or_imm)  f =
 match l  with 
	| Ident i-> let fu = function_has#look_for f in fu#add i 0
	| Int i -> ()

let rec assign_exp exp f =
 match exp with 
 	| Nop -> ()
	| LPexpRp e -> ()
	| Int i -> ()
	| Ident i -> let fu = function_has#look_for f in fu#add i 0
	| Label s -> ()
	| Neg i -> ()
	| FNeg i -> () 
	| Add (i,id) ->  let fu = function_has#look_for f in fu#add i 0; fu#incr_line; assign_ident_or_imm id f 
	| Sub (i,id) ->  let fu = function_has#look_for f in fu#add i 0; fu#incr_line;assign_ident_or_imm id f 
	| Ld (i,id) -> if i ="%self" then () else let fu = function_has#look_for f in fu#add i 0;fu#incr_line; assign_ident_or_imm id f 
	| St (i1,id,i2) -> let fu = function_has#look_for f in fu#add i1 0 ;fu#incr_line; assign_ident_or_imm id f ;fu#add i2 0
	| FAdd (i,id) -> let fu = function_has#look_for f in fu#add i 0; fu#add id 0;fu#incr_line
	| FSub (i,id) -> let fu = function_has#look_for f in fu#add i 0; fu#add id 0;fu#incr_line
	| FMul (i,id) -> let fu = function_has#look_for f in fu#add i 0; fu#add id 0;fu#incr_line
	| FDiv (i,id) -> let fu = function_has#look_for f in fu#add i 0; fu#add id 0;fu#incr_line
	| New i -> ()
	| IfEq (i, id , t1, t2 ) -> let fu = function_has#look_for f in fu#add i 0;fu#incr_line; assign_ident_or_imm id f ; assign_asmt t1 f; assign_asmt t2 f 
	| IfLEq (i, id , t1, t2 ) -> let fu = function_has#look_for f in fu#add i 0; fu#incr_line;assign_ident_or_imm id f ; assign_asmt t1 f; assign_asmt t2 f 
	| IfGEq (i, id , t1, t2 ) -> let fu = function_has#look_for f in fu#add i 0; fu#incr_line; assign_ident_or_imm id f ; assign_asmt t1 f; assign_asmt t2 f 
	| CallLabel (e,a) -> ()
	| CallClo  (id,t) -> let fu = function_has#look_for f in fu#add id  0;fu#incr_line 
and assign_asmt a f =
	match a with
	| LpasmtRPAREN a -> assign_asmt a f
	| LetIdentEq (i,e2,a) -> let fu = function_has#look_for f in fu#add i 0;fu#incr_line; assign_exp e2 f ; assign_asmt a f
	| Exp e -> assign_exp e f

let rec assign_form (l : string list) f : unit =
  match l with
    [] -> ()
  | x :: xs -> let fu = function_has#look_for f in fu#add x 1 ;fu#incr_line;  assign_form xs f 

let rec assign_function exp  =
	match exp with
	| LetUnderscEQ a -> function_has#add "_" ;assign_asmt a "_"(* Let _ =  *)
	| LetLabeleqFloat (i,fl,fu) -> () ; () ; assign_function fu (*Let _label = 0.2 in *)
	| LetLabelEq (i,f,a,fu)-> function_has#add i ; assign_form f i;  assign_asmt a i; assign_function fu  (* Let _label = something in ...*)
let oset = ref(-4) 
let rec if_doubl_occcu  ke func valu =
	let ofset = !oset in 
	Hashtbl.iter (fun key value  -> 
		if (value#get_reg = valu#get_reg && key != ke && valu#get_is_in_stack = 0 &&  value#get_is_in_stack = 0 ) 
			then ((*if the register is used twice*)
			 (*printf "%s and %s use twice : %s,%s : %d-%d : %d-%d\n" (ke) (key) (valu#get_reg) (value#get_reg) (valu#get_first_use) (valu#get_last_use) (value#get_first_use) (value#get_last_use);*)
			if ( (value#get_first_use) > (valu#get_last_use) ) (*if there is no concurrency we do nothing*)
			then () 
			else ( if ( (value#get_last_use) < (valu#get_first_use) ) (*if there is no concurrency we do nothing*)
				then () 
				else (
					if ( (value#get_first_use) >= (valu#get_first_use)  && (value#get_last_use) >= (valu#get_last_use) ) 
					then (
							if(value#get_occ > valu#get_occ) 
							then (value#set_in_stack ; value#set_offset ofset) 
							else (valu#set_in_stack ; valu#set_offset ofset) )
					else ( 
						if ( (value#get_first_use) <= (valu#get_first_use) && (value#get_last_use) >= (valu#get_last_use) ) 
						then (
							if(value#get_occ > valu#get_occ) 
							then  (value#set_in_stack; value#set_offset ofset) 
							else (valu#set_in_stack ; valu#set_offset ofset) )  
						else (
							if ( (value#get_first_use) >= (valu#get_first_use) && (value#get_last_use) <= (valu#get_last_use) )
							then (
								if(value#get_occ > valu#get_occ) 
								then  (value#set_in_stack; value#set_offset ofset)
								else (valu#set_in_stack ; valu#set_offset ofset) )		
							)
						)
					)
				)
			)
	)func
let rec linear_alloc funcs = 
	Hashtbl.iter(fun key value -> oset := 0;
		Hashtbl.iter (fun key2 value2 ->  oset := !oset +4 ; if_doubl_occcu key2 value#get_hast value2 ;  ) value#get_hast 
	)funcs#get_hast

let allocate exp =
	function_has#clear;
	assign_function exp;
	linear_alloc function_has;
	(*Hashtbl.iter (fun key value -> printf"function : %s \n " key;Hashtbl.iter (fun key2 value2 -> printf"var : %s , register : %s \n" key2 (value2#get_reg)) value#get_hast)function_has#get_hast;*)

	function_has
