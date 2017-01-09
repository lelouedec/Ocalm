type t = string

let to_string x = x
let counter = ref (-1)

let genid =
  fun () ->
    incr counter;
    Printf.sprintf "?v%d" !counter

let gentmp = 
  incr counter;
  Printf.sprintf "tmp%d" !counter
