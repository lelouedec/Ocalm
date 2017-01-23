let rec app f x = f x in
let rec g y = y + 1 in
let k = app g 3 in
print_int k
