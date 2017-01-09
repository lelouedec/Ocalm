open Syntax

module S = 
  Map.Make(
    struct
      type t = Id.t
      let compare = compare
    end)
include S

