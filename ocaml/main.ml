let print_ast l =
  print_string (Syntax.to_string (Parser.exp Lexer.token l)); print_newline ()

let file f = 
  let inchan = open_in f in
  try
    (*let ast = Lexing.from_channel inchan in*)
    Asml.test ();
    close_in inchan
  with e -> (close_in inchan; raise e)

let () = 
print_endline " coucou";
let files = ref [] in
  Arg.parse
    [ ]
    (fun s -> files := !files @ [s])
    (Printf.sprintf "usage: %s filenames" Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files


