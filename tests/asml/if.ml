let rec f x = x + 1 in
let a = 10 in
let b =
  if a > 0 then
    let c = a + 10 in (f c) + (f c)
  else
    let d = a - 10 in f d
in print_int b
