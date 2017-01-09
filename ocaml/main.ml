let print_ast exp =
  print_string (Syntax.to_string (exp)); print_newline ()

let file f = 
  let inchan = open_in f in
  try
    let lex = Lexing.from_channel inchan in
    let exp = Parser.exp Lexer.token lex in
    print_ast exp;

		let type_eq = Typing.genenerate exp St.empty St.empty Type.Unit in
		print_endline "type equations :";
    List.iter
      Typing.to_string (type_eq);
    List.iter
      Typing.unify (type_eq);
		print_endline "solved equations :";
    List.iter
      Typing.to_string (type_eq);

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
