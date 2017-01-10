open Syntax

module S = 
  Map.Make(
    struct
      type t = Id.t
      let compare = compare
    end)
include S

let to_string to_s m =
  let pairs = S.bindings m in
  let strings = List.map (fun (k, v) -> Printf.sprintf "%s : %s" k (to_s v)) pairs in
  String.concat "\n" strings
