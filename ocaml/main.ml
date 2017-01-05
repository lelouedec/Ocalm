let print_ast exp =
  print_string (Syntax.to_string exp); print_newline ()

let file f =
  let inchan = open_in f in
  try
    let exp = Parser.exp Lexer.token (Lexing.from_channel inchan) in
      let h = Syntax.height exp in
      print_string (Printf.sprintf "height: %d\n" h);
			List.iter print_endline (
				List.sort_uniq String.compare (Syntax.get_vars exp));
      print_ast exp;
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
