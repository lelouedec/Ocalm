let rec double x = x + x in
let rec decr x = x - 1 in
let v = double (decr (decr (double 10))) in
()
