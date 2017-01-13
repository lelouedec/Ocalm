let rec even x =
  let rec odd x = (
    if x > 0 then even (x - 1) else
    if x < 0 then even (x + 1) else 0
  ) in
  if x > 0 then odd (x - 1) else
  if x < 0 then odd (x + 1) else 1
in print_int (even 10); print_int (even (-9))
