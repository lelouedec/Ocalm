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
Register_alloc.allocate exp2;


