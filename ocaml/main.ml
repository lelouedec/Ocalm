open Printf

let version = "0.0.1"

let print_ast exp =
  print_string (Syntax.to_string (exp)); print_newline ()

let file f flags =
  let inchan = open_in f in
  try
    let _b = Lexing.from_channel inchan in
    let _p = Parser.exp Lexer.token _b in
    if List.mem "-p" flags then
      (* parse only *)
      ()
    else (
      let _t = Typing.f _p in
      if List.mem "-t" flags then
        (* type checking only *)
        ()
      else (
        let _r =
          (Elim.f
            (Constant.f
              (Inline.f
                (Let.f
                  (Beta.f
                    (Alpha.f
                      (KNormal.f _t))))))) in
        (* if List.mem "-d" flags then ( *)

          let cls = Closure.f _r in
          let vir = Virtual.f cls in

          if List.mem "-asml" flags then ( 
            let reg = Register_alloc.allocate vir in  print_endline (Asm_generator.generate vir reg)
          )
          else ();
          
        (* ) else () *)
      )
    );

    close_in inchan
  with e -> (close_in inchan; raise e)

let output f =
  print_endline ("Output: " ^ f)

let help () =
  print_endline (Printf.sprintf "usage: %s filenames" Sys.argv.(0));
  exit 0

let version () =
  print_endline (Printf.sprintf "%s v%s" Sys.argv.(0) version);
  exit 0

let () =
  let flags = ref [] in
  let options = [
    ("-o", Arg.String output, "<filename> set output file");
    ("-h", Arg.Unit help, "display help");
    ("-v", Arg.Unit version, "display version");
    ("-t", Arg.Unit (fun () -> flags := "-t" :: !flags), "type check only");
    ("-p", Arg.Unit (fun () -> flags := "-p" :: !flags), "parse only");
    ("-asml", Arg.Unit (fun () -> flags := "-asml" :: !flags), "output ASML");
    ("-wo", Arg.Unit (fun () -> flags := "-wo" :: !flags), "without optimizations");
    ("-d", Arg.Unit (fun () -> flags := "-d" :: !flags), "debug mode");
  ] in
  let files = ref [] in
    Arg.parse
      options
      (fun s -> files := !files @ [s])
      (Printf.sprintf "usage: %s filenames" Sys.argv.(0));
    List.iter
      (fun f -> ignore (file f !flags))
      !files
