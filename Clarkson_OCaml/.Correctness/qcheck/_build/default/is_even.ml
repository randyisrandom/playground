let is_even n = (n mod 2 = 0)

let rec is_sorted = function
  | [] -> true
  | [_] -> true
  | h1 :: (h2 :: _ as t') -> h1 <= h2 && is_sorted t'