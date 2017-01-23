let rec app f x = f x in
let rec f x = if x > 0 then x + (app f (x - 1)) else 0 in
let k = f 4 in
print_int k
