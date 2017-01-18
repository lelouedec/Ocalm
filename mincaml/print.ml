(*print_int 123;
print_int (-456);
print_int (789+0);*)
(*let a = 1 in let c = a + (-a) in _*)

let bc = 1 in
let rec f x = x + 1 + 1 + 1 + 5 in 
let a = f 1 in print_int a
(*let rec fn x = x + x in
let a = 1 in let r = fn 2 in print_int r*)
(*let rec x x1 x2 = x1 + x2 in
let a = a + (x 3 4) in _*)
