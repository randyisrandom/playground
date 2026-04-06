(*twice no arguments*)
(*mystery operator 1*)
(*mystery operator 2*)
module Repeat = struct 
  let rec repeat f n x = match n with
   |0 -> x 
   |n -> f (repeat f (n - 1) x);;
end
(*product*)
let product = List.fold_left ( * ) 1 
let rec myprod = function [] -> 1 | h::t -> h * myprod t 
(*terse product*)
(*sum_cube*) 
let npr n = let rec aux acc = function 0 -> acc| n -> aux (n::acc) (n-1) in aux [] n
let cube ls = List.map (fun x -> x*x*x) ls 
let sum ls = List.fold_left (+) 0 ls 
let sumcube n = sum @@ cube @@ npr n 
(*sub_cube_odd*)
let isodd n = if n mod 2 <> 0 then true else false
let oddls = List.filter isodd 
let sumcubeodd n = sum @@ cube @@ oddls @@ npr n 
(*sum cube odd pipeline*)
let pipeline n = npr n |> oddls |> cube |> sum 
(*exists*)
(*account balance*)
let now account balance = List.fold_left (-.) balance account
(*library uncurried*)
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y
let id x = curry @@ uncurry x 
(*map composition*)
let (@@.) (f:'a->'b) (g:'c -> 'a) x = f @@ (g x)
let mapcomp f g = List.map (f @@. g) 
(*more list functions*)

(*association list keys*) 
let rec ismatrix m = 
  match m with 
  []-> false
  |[x]-> if List.length x = 0 then false else true 
  |x::y::t -> if List.length x = List.length y then ismatrix (y::t) else false

(*row vector add*)
let rowadd v1 v2 = List.map2 (+) v1 v2 
(*matrix add*)
let addmatrix m1 m2 = List.map2 (rowadd) m1 m2  
(*matrix multiply*)
let n_column n l = List.map (fun x -> List.nth x n) l 
let transpose m = 
  let rec aux_transpose m1 acc = 
    match m1 with 
    [] -> []
    |_ :: t -> n_column acc m1 :: aux_transpose t (acc+1) in aux_transpose m 0
let dotprod v1 v2 = List.fold_left (+) 0 (List.map2 ( * ) v1 v2) 


let rec left_vector v m = match transpose m with [] -> [] | h::t -> (dotprod v h) :: left_vector v t 

let rec matrixmultiply m1 m2 = 
  match m1 with 
  h::t -> left_vector h (transpose m2) :: matrixmultiply t m2 
  |[]->[]

  let n_column n l = Array.map (fun x -> x.(n)) l 