let rec f x =
  let rec g y = x + y in g
in (f 2) 3; ()
