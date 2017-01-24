open Printf
(*open Filename*)

let version = "0.0.1"

let print_ast exp =
  print_string (Syntax.to_string (exp)); print_newline ()

let file f flags : string =
  let result = ref "" in
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
          (Let.f
            (Beta.f
              (Alpha.f
                (KNormal.f _t)))) in
        let _r = if List.mem "-opt" flags then
          (Elim.f
            (Constant.f
              (Inline.f _r)
            )
          ) else _r in
        (* if List.mem "-d" flags then ( *)
          
          
          let cls = Closure.f _r in
          let vir = Virtual.f cls in

          if List.mem "-s" flags then (
            print_endline ((KNormal.to_string _r) ^ "\n\n");
            print_endline ((Closure.to_string cls) ^ "\n\n");
            print_endline ((Asml.fundefs_to_string vir) ^ "\n\n");
          )
          else ();

          if List.mem "-asml" flags then (
            result := Asml.fundefs_to_string vir;
            (* let reg = Register_alloc.allocate vir in  print_endline (Asm_generator.generate vir reg) *)
          )
          else (
            result := let reg = Register_alloc.allocate vir in (Asm_generator.generate vir reg);
          );
          
        (* ) else () *)
      )
    );

    close_in inchan;
    !result
  with e -> (close_in inchan; raise e)

let help () =
  print_endline (Printf.sprintf "usage: %s filenames" Sys.argv.(0));
  exit 0

let version () =
  print_endline (Printf.sprintf "%s v%s" Sys.argv.(0) version);
  exit 0

let () =
  let flags = ref [] in
  (* let output = ref "a.out" in *)
  (* let outputs = ref [] in *)
  let options = [
    (* ("-o", Arg.String (fun fname -> output := fname), "<filename> set output file"); *)
    ("-h", Arg.Unit help, "display help");
    ("-v", Arg.Unit version, "display version");
    ("-t", Arg.Unit (fun () -> flags := "-t" :: !flags), "type check only");
    ("-p", Arg.Unit (fun () -> flags := "-p" :: !flags), "parse only");
    ("-asml", Arg.Unit (fun () -> flags := "-asml" :: !flags), "output ASML");
    ("-opt", Arg.Unit (fun () -> flags := "-opt" :: !flags), "with optimizations");
    ("-d", Arg.Unit (fun () -> flags := "-d" :: !flags), "debug mode");
    ("-s", Arg.Unit (fun () -> flags := "-s" :: !flags), "print closure and asml")
  ] in
  let files = ref [] in
    Arg.parse
      options
      (fun s -> files := !files @ [s])
      (Printf.sprintf "usage: %s filenames" Sys.argv.(0));
    let results = List.map
      (fun f -> file f !flags)
      !files in
    let outputs = List.map 
      (fun f -> if List.mem "-asml" !flags then (Filename.chop_extension f) ^ ".asml" else (Filename.chop_extension f) ^ ".s") 
      !files in
    List.iter2
      (fun output res -> let ochan = open_out output in Printf.fprintf ochan "%s\n" res; close_out ochan)
      outputs
      results
    (* let ochan = open_out outputs in
    List.iter (fun res -> Printf.fprintf ochan "%s\n" res) results;
    close_out ochan *)
