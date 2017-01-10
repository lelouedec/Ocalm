let print_ast exp =
  print_string (Syntax.to_string (exp)); print_newline ()

let file f = 
  let inchan = open_in f in
  try
    let _b = Lexing.from_channel inchan in
    let _r = 
      KNormal.f
        (Typing.f
          (Parser.exp Lexer.token _b)) in
    
    (* print_endline ("var y : " ^ Type.to_string(St.find "y" !Typing.st)); *)
    print_endline ("Typing:\n" ^
      St.to_string Type.to_string !Typing.st );

    print_endline (KNormal.to_string _r);

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
