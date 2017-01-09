let print_ast l =
  print_string (Asml.to_string (Parser_asml.exp Lexer_asml.token l)); print_newline ()

let file f = 
  let inchan = open_in f in
  try
    let ast = Lexing.from_channel inchan in
      print_ast ast ;
    close_in inchan
  with e -> (close_in inchan; raise e)

let () = 
  let files = ref [] in
  Arg.parse
    [ ]
    (fun s -> files := !files @ [s])
    (Printf.sprintf "usage: %s filenames" Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files
