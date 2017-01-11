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

let exp3 =
    LetLabelEq ("succ",["x"], LetIdentEq("t",Neg("1"),Exp( Add( "x" , Int(1)) )),  (* x +t *)
     LetUnderscEQ(LetIdentEq("y",Neg("1"),Exp (CallLabel("succ y")) )))   in  (*pb succ*)
Asml.test exp3 ;

let exp4 =
    LetLabelEq ("f",["x"], LetIdentEq("z",Neg("1"),Exp( Add( "x" , Int(1)) )),  (* x +t *)
     LetUnderscEQ(LetIdentEq("y",Neg("1"),Exp (CallLabel("succ y")) )))   in  (*pb succ*)
Asml.test exp4 ;

Register_alloc.allocate exp2;
Register_alloc.allocate exp3;




(*
let test () =
  print_endline "Test"; 
  let exp2 =
    LetLabelEq ("f",Ident "x", Exp( Add( "x" , Int (0) ) ), LetUnderscEQ(Exp ( CallLabel ( "f") ) )) in print_endline (fundefs_to_string (exp2) );
    let exp3 =
            (*LetLabelEq ("succ",Ident "x", Exp( Add( "x" , Int (0) ) ), LetUnderscEQ(Exp ( CallLabel ( "f") ) )) in print_endline (fundefs_to_string (exp3) );*)
         LetLabelEq("succ",Ident "x",LetIdentEq("t",Neg ("1"),Exp( Add( "x" ,Ident "t" ) )),
          LetUnderscEQ(Exp ( CallLabel("succ")))) in print_endline (fundefs_to_string (exp3));                                     
     let exp4 = 
      LetUnderscEQ(LetIdentEq ("x", Neg ("1"), Exp (Add( "x" , Int (0) ) ))) in print_endline (fundefs_to_string (exp4));
     let exp5 = 
      LetLabelEq(,,,);
*)