let rec f x = x + 1 in
let rec g y = f in
print_int ((g 1) 2)
