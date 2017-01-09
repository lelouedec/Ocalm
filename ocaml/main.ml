let print_ast exp =
  print_string (Syntax.to_string (exp)); print_newline ()

let file f = 
  let inchan = open_in f in
  try
    let lex = Lexing.from_channel inchan in
    let exp = Parser.exp Lexer.token lex in
    let env = Env.get_env exp in
    print_ast exp;
    Env.iter
      print_endline (Id.to_string env);
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
