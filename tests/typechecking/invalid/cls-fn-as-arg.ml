let x = 1 in
let rec f y = x + y in
let rec h z = z + 1. in
let rec g u = u 0 in
let k = g f in
let l = g h in
()
