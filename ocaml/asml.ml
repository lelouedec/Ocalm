type id_or_imm = V of id.t | C of int 

type t = 
	| Let of (Id.t * Type.t)  * exp * t
	| Forget of Id.t * t 
and exp = 
	| Add of Id.t * id_or_imm
	| Ld of mem( x + y )
	| St of mem( x + y ) <- z
	| FAddD of Id.t * Id.t
	| IfEq of Id.t * id_or_imm * t * t
	| IfFEq of Id.t * Id.t * t * t
	| CallCls of Id.t * Id.t list * Id.t list
	| CallDir of Id.l * Id.t list * Id.t list
	| Save of Id.t * Id.t
	| Restore of Id.t