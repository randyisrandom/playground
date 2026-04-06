(**)
let a () = [1;2;3;4;5]

let b () = 1 :: 2 :: 3 :: 4 :: 5 :: []

let c () = [1] @ [2;3;4] @ [5] 
(**)
let product ls = 
  let rec product_aux acc = 
    function [] -> acc 
    | h :: t -> product_aux (acc * 1) t  
  in product_aux 1 ls
(**)
let concat ls = 
  let rec product_aux acc = 
    function [] -> acc 
    | h :: t -> product_aux (acc ^ "h") t  
  in product_aux "" (List.rev ls)
(*DO product test*)
(*a more practical example would be . file extensions*)
let is_bigred = 
  function [] -> false 
  | h :: _ -> if h = "bigred" then true else false 

let twofour = function [] -> false 
  |_::_:: [] -> true 
  |_::_::_::_::[]->true 
  |_::_ -> false 

let first_equal ls1 ls2 = 
  match ls1, ls2 with 
  | x1 :: y1 :: _, x2 :: y2 :: _ -> if (x1=x2 && y1=y2) then true else false 
  | _::_,_::_ -> false
  | [],_::_ -> false
  | _::_,[] -> false
  | [],[] -> false  
(*library*)
let fifth ls = if List.length ls < 5 then 0 else (List.nth ls 4) 

let sorted ls = List.rev @@ List.sort compare ls

(*library tests*)
(*library puzzle*)
let first ls = List.hd @@ List.rev ls
let f ls = let is_zero x = (x=0) in List.exists is_zero ls
(*take drop & take drop rec*)
   let rec take n l =
    if n=0 then []
    else match l with
      | [] -> []
      | h::t -> h :: take (n-1) t

  let rec drop n l = 
    if n=0 then l
    else match l with(*pattern matching is done on list if...*)
      | [] -> []
      | _ :: t -> drop (n-1) t

let take_tl n l = drop n (List.rev l)
(*create a pair of mutually recursive functions*)

(*recursive defintion of power set*)
let rec powerset = function
  | [] -> [[]]
  | x :: xs ->
      let ps = powerset xs in
      ps @ (List.map (fun y -> x :: y) ps)
(*print int list rec*)
(*print int list iter*)
let print_int_list lst =
  List.iter (fun x -> print_int x) lst
(*student*)
type student = {first_name : string; last_name : string; gpa : float}

let empty = {first_name = ""; last_name = ""; gpa = 4.0}

let name s = (s.first_name,s.last_name)

let makestudent first last grades = {first_name = first; last_name = last; gpa = grades}
(*pokerecord*)
type poketype = Normal | Fire | Water
type pokemon = {name:string; hp:int ;ptype:poketype}
(*safe hd and tl*)
let safe_hd ls = match ls with []->None|h :: _ -> Some h
let safe_tl ls = match ls with []->None|_ :: t -> Some t
(*pokefun*)
let pokemax x y = if (Int.max x.hp y.hp) = x.hp then x else  y
let pokemaxlist ls = List.fold_left (pokemax) ls
(*date before*)
let isdate (x,y,z) = (x>0)&&((y>0)&&(y<=12))&&((z>0)&&(z<=31))
let (<<<) (x1,y1,z1) (x2,y2,z2) = 
  if x1 < x2 then true else 
    if (x1=x2) && (y1 < y2) then true else 
      if (y1=y2) && (z1 < z2) then true else 
        false 
let isbefore x y = 
  if ((isdate x)&&(isdate y)) = true then x <<< y else 
    raise (Invalid_argument "not a valid date")
(*earliest date*)
let rec earliest_date ls = match ls with []-> None |[x]-> Some x | x::y::t ->  if x <= y then earliest_date (x::t) else earliest_date (y::t)
(*association list*)
(*cards*)

type suit = Hearts | Spades | Diamonds | Clubs

type rank = Num of int | Jack | Queen | King

type card = {x : rank; y : suit}

let aceofclubs = {x = Num 1;y = Clubs}

(*matching*)


(*quadrant*)
type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x:int) : sign = if x < 0 then Neg else if x = 0 then Zero else Pos

let quadrant : int*int -> quad option = fun (x,y) ->
  match (sign x, sign y) with
    |(Pos,Pos) -> Some I
    |(Neg,Pos) -> Some II
    |(Neg,Neg) -> Some III
    |(Pos,Neg) -> Some IV
    |(_,_) -> None

(*quadrant when*)
(*depth*)
type 'a tree = L | T of 'a * 'a tree * 'a tree
let rec depth tr = 
  match tr with 
  L -> 0 
  |T (_, l , r) -> max (depth l) (depth r)
(*shape*)
let rec same_form t1 t2 = 
  match t1,t2 with 
  |L,L->true
  |T (_,l,r),T(_,g,d) -> (same_form l g) && (same_form r d) 
  |_,_-> false

(*every sub tree should have the same*)

(*list max exn*)
(*list max exn string*)
(*list max exn ounit*)
(*is_bst*)
(*quadrant poly*)

