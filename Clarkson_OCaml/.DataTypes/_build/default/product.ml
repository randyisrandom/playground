let product ls = 
  let rec aux l acc = 
    match l with [] -> acc 
    |h::t -> aux t (h * acc) 
  in aux ls 1