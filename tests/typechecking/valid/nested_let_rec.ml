let rec double x = x + x in
let rec incr x = x + 1 in
print_int (incr (double (incr 1)))
