
open Printf
open Asml
open Array

class variable =
object(self)
val mutable register : string = "";
val mutable lifetime : int = 0;
method get_reg =
	register
method get_lifetime =
	lifetime
method set_register x = 
	register <- x 
method incr_lifetime y = 
	lifetime <- lifetime + y
end ;; 

type t = (string, variable ) Hashtbl.t;;

class registers_function = 
	object (self)
	val mutable register_hash : t = Hashtbl.create 42;
	val mutable counter = 4;
	val mutable reg_to_save : string list = [];
	method get_reg_used = 
		reg_to_save
	method add x =
	 if (Hashtbl.mem register_hash x)  then let c = self#look_for x in c#incr_lifetime 1 else  
	 	(let y = new variable in  y#set_register ("R"^ string_of_int counter); y#incr_lifetime 1  ; Hashtbl.replace register_hash x  y; reg_to_save <- reg_to_save @ [("R"^ string_of_int counter)] ;counter <- counter + 1);
	 if counter > 11 then counter <- 4
	method look_for x = 			 
			 Hashtbl.find register_hash x 
	method get_hast =
				register_hash
	method clear =
		Hashtbl.clear register_hash;
		counter <- 4
	end;;

type z = (string, registers_function) Hashtbl.t;;

class functions_register_hash = 
	object (self)
	val mutable functions_hash : z = Hashtbl.create 42;
	method add x =
	 if (Hashtbl.mem functions_hash x) == true then	() else (let func = new registers_function in  Hashtbl.replace functions_hash (x)  (func) ); 
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
	| Ident i-> let fu = function_has#look_for f in fu#add i 
	| Int i -> ()

let rec assign_exp exp f =
 match exp with 
 	| Nop -> ()
	| LPexpRp e -> ()
	| Int i -> ()
	| Ident i -> let fu = function_has#look_for f in fu#add i 
	| Label s -> ()
	| Neg i -> ()
	| FNeg i -> () 
	| Add (i,id) ->  let fu = function_has#look_for f in fu#add i ; assign_ident_or_imm id f 
	| Sub (i,id) ->  let fu = function_has#look_for f in fu#add i ; assign_ident_or_imm id f 
	| Ld (i,id) -> if i ="%self" then () else let fu = function_has#look_for f in fu#add i ; assign_ident_or_imm id f 
	| St (i1,id,i2) -> let fu = function_has#look_for f in fu#add i1 ; assign_ident_or_imm id f ;fu#add i2
	| FAdd (i,id) -> let fu = function_has#look_for f in fu#add i ; fu#add id 
	| FSub (i,id) -> let fu = function_has#look_for f in fu#add i ; fu#add id 
	| FMul (i,id) -> let fu = function_has#look_for f in fu#add i ; fu#add id 
	| FDiv (i,id) -> let fu = function_has#look_for f in fu#add i ; fu#add id 
	| New i -> ()
	| IfEq (i, id , t1, t2 ) -> let fu = function_has#look_for f in fu#add i ; assign_ident_or_imm id f ; assign_asmt t1 f; assign_asmt t2 f 
	| IfLEq (i, id , t1, t2 ) -> let fu = function_has#look_for f in fu#add i ; assign_ident_or_imm id f ; assign_asmt t1 f; assign_asmt t2 f 
	| IfGEq (i, id , t1, t2 ) -> let fu = function_has#look_for f in fu#add i ; assign_ident_or_imm id f ; assign_asmt t1 f; assign_asmt t2 f 
	| CallLabel (e,a) -> ()
	| CallClo  (id,t) -> let fu = function_has#look_for f in fu#add id ;
and assign_asmt a f =
	match a with
	| LpasmtRPAREN a -> assign_asmt a f
	| LetIdentEq (i,e2,a) -> let fu = function_has#look_for f in fu#add i; assign_exp e2 f ; assign_asmt a f
	| Exp e -> assign_exp e f

let rec assign_form (l : string list) f : unit =
  match l with
    [] -> ()
  | x :: xs -> let fu = function_has#look_for f in fu#add x  ;  assign_form xs f 

let rec assign_function exp  =
	match exp with
	| LetUnderscEQ a -> function_has#add "_" ;assign_asmt a "_"(* Let _ =  *)
	| LetLabeleqFloat (i,fl,fu) -> () ; () ; assign_function fu (*Let _label = 0.2 in *)
	| LetLabelEq (i,f,a,fu)-> function_has#add i ; assign_form f i;  assign_asmt a i; assign_function fu  (* Let _label = something in ...*)

let rec count_occu  ke func valu =
	let counter_oc = ref (0) in 
	Hashtbl.iter (fun key value  -> if (value#get_reg = valu && key != ke ) then incr counter_oc else ()) func;
	!counter_oc

let rec linear_alloc funcs = 
	Hashtbl.iter(fun key value -> 
		Hashtbl.iter (fun key2 value2 -> if (count_occu key2 value#get_hast value2#get_reg ) > 1 
		then print_endline "coucou" 
		else () ) value#get_hast )funcs#get_hast

let allocate exp =
	function_has#clear;
	assign_function exp;
	linear_alloc function_has;

	function_has
