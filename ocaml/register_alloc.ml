
open Printf
open Asml


type t = (string, string) Hashtbl.t;;

class register_function = 
	object (self)
	val mutable register_hash : t = Hashtbl.create 42;
	val mutable counter = 0;
	method add x =
	 if (Hashtbl.mem register_hash x) == false then	Hashtbl.replace register_hash x ("r"^ string_of_int counter) ;	counter <- counter + 1
	method look_for x = 
			Hashtbl.find_all register_hash x 
	end;;


let registerf = new register_function  

let rec assign_exp exp =
 match exp with 
 	| Nop -> ()
	| LPexpRp e -> ()
	| Int -> ()
	| Ident i -> registerf#add i 
	| Label s -> ()
	| Neg i -> ()
	| FNeg i -> () 
	| Add (i,id) -> registerf#add i
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


let rec assign_asmt a =
	match a with
	| LpasmtRPAREN a -> assign_asmt a 
	| LetIdentEq (i,e2,a) -> (); assign_exp e2  ; assign_asmt a 
	| Exp e -> assign_exp e 

let rec assign_form (l : string list) : unit =
  match l with
    [] -> ()
  | x :: xs -> registerf#add x ;  assign_form xs

let rec assign_function fo  =
	match fo with
	| LetUnderscEQ a -> assign_asmt a (* Let _ =  *)
	| LetLabeleqFloat (i,fl,fu) -> () ; () ; assign_function fu (*Let _label = 0.2 in *)
	| LetLabelEq (i,fo,a,fu)-> () ; assign_form fo ; assign_asmt a ; assign_function fu  (* Let _label = something in ...*)

let rec display_reg_var ( l : string list ): string = 
	match l with
    [] -> ""
  | x :: xs -> x ^ (display_reg_var xs)

let allocate exp =


	assign_function exp ;

	print_endline ( "x : " ^ display_reg_var( registerf#look_for "x" ))    ;
	print_endline ( "y : " ^ display_reg_var( registerf#look_for "y" ));
