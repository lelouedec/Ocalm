let rec f x =
  let rec g y = x + y in g
in 
let y = (f 2) 3 in ()
