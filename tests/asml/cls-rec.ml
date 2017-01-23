let t = 0 in
let rec f x = if x > 0 then x + f (x - 1) else t in
let k = f 3 in
print_int k
