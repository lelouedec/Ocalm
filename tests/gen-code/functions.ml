let rec add x = x + 1 in
let rec substract x = x-1 in
let rec double x = x + x in
print_int (double (add (substract 4)))
