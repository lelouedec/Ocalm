let rec f x = if x = 0 then 0 else x + f (x-1) in 

let r = f 10 in print_int r