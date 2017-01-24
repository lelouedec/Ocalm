let rec g f t = (f t) + (f (t + 1)) in
let rec f x = x + 1 in
let a = Array.create 3 f in
let k = g a.(1) 4 in
print_int k
