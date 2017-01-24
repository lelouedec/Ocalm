let rec make_adder x =
let rec adder y = x + y in
adder in
let s=((make_adder 3) 7) in
print_int s
