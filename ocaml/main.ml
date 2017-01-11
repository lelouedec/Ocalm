open Asml

let print_ast l =
  print_string (Syntax.to_string (Parser.exp Lexer.token l)); print_newline ()

let file f = 
  let inchan = open_in f in
  try
    (*let ast = Lexing.from_channel inchan in*)
    close_in inchan
  with e -> (close_in inchan; raise e)

let () = 
(*print_endline " coucou";
let files = ref [] in
  Arg.parse
    [ ]
    (fun s -> files := !files @ [s])
    (Printf.sprintf "usage: %s filenames" Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files)*)
let exp2 =
    LetLabelEq ("f",["x";"y"], Exp( Add( "x" , Int (0) ) ), LetUnderscEQ(Exp ( CallLabel ( "f") ) )) in 
Asml.test exp2 ;


let h = Register_alloc.allocate exp2 in ();

let exp3 =
    LetLabelEq ("f",["x"], LetIdentEq("z",Add( "x" , Int(1)),Exp( Sub( "z" , Int(2)) )),  (* x +t *)
     LetUnderscEQ(Exp ( CallLabel ( "f") )))   in  (*pb succ*)
Asml.test exp3 ;
let h2 = Register_alloc.allocate exp3 in () ;

let exp4 =
    LetLabelEq ("succ",["x"], LetIdentEq("t",Neg("1"),Exp( Add( "x" , Ident "t" ))),  (* x +t *)
     LetUnderscEQ(LetIdentEq("y",Neg("1"),Exp (CallLabel("succ y")) )))   in  (*pb succ*)
Asml.test exp4 ;
let h3 = Register_alloc.allocate exp4 in ()



