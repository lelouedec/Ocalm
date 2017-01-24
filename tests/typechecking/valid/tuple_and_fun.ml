let rec f x y = (x + y, x - y, 1.) in
let (a, b, c) = f 2 3 in
let m = a + b in
let l = c *. 1. in
print_int m
