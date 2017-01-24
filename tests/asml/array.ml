let x = 4 in
let ar = Array.create 3 (x - 2) in
let tu = ar.(0) <- ar.(0) - 1 in
let y = ar.(0) + ar.(1) in
print_int y
