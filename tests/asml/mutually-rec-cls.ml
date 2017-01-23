let x = 0 in
let rec e y =
  let rec o y =
    if y > 0 then e (y - 1) else x in
  if y > 0 then o (y - 1) else x + 1 in
let k = e 4 in
print_int k
