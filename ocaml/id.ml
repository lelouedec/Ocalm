type t = string

let to_string x = x
let counter = ref (-1)

let genid =
  fun () ->
    incr counter;
    Printf.sprintf "?v%d" !counter

let knorm_counter = ref (-1)

let gen_asml_id =
  fun () ->
    incr knorm_counter;
    Printf.sprintf "v%d" !knorm_counter
