type ident_or_imm = 
	| Ident of Id.t 
	| Int of int 

type exp = 
	| Nop 
	| LPexpRp of ( exp ) 
	| Int 
	| Label 
	| Neg of Id.t
	| FNeg of Id.t
	| Add of Id.t * ident_or_imm
	| Sub of Id.t * ident_or_imm
	(*| Ld of Mem
	| St of Mem2*)	
	| FAdd of Id.t * Id.t
	| FSub of Id.t * Id.t
	| FMul of Id.t * Id.t 
	| FDiv of Id.t * Id.t 
	| New of ident_or_imm
	| Mem of Id.t * ident_or_imm
	| Mem2 of  Id.t * ident_or_imm * Id.t 
	| IfEq of Id.t * ident_or_imm * asmt * asmt
	| IfLEq of Id.t * ident_or_imm * asmt * asmt
	| IfGEq of Id.t * ident_or_imm * asmt * asmt
	| CallLabel of formal_args
	| CallClo of Id.t * formal_args
and asmt =
	| LPasmRp of (asmt)
	| LetIn of Id.t * exp * asmt
and formal_args = 
	| Ident of formal_args
	| Nil
and fundefs = 
 	| Let_ of asmt
 	| LetLabeleqFloat of  fundefs
 	| LetLaelEq of formal_args * asmt * fundefs

