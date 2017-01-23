let rec app f x = f x in
let rec g x = if x > 0 then x + (app g (x - 1)) else 0 in
let k = g 4 in
print_int k
