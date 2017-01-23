let rec app f x = f x in
let rec e x =
  let rec o x =
    if x > 0 then app e (x - 1) else 0 in
  if x > 0 then o (x - 1) else 1 in
print_int (e 5)
