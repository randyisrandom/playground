(*Parameterize expressions(sub-modules) and types(sub-signatures).*)
module type ComplexSig = sig 
    type t
    val zero : t
    val add : t -> t -> t
end
module Complex : ComplexSig = struct
  type t = float * float
  let zero = (0., 0.)
  let add (r1, i1) (r2, i2) = r1 +. r2, i1 +. i2
end
(*a signature mismatch occurs*)
module type Queue = sig
  type 'a t
  exception Empty
  val empty : 'a t
  val is_empty : 'a t -> bool
  val enqueue : 'a -> 'a t -> 'a t
  val front : 'a t -> 'a
  val dequeue : 'a t -> 'a t
  val size : 'a t -> int  val to_list : 'a t -> 'a list
end

module ListQueue : Queue = struct
  type 'a t = 'a list
  exception Empty
  let empty = []
  let is_empty = function [] -> true | _ -> false
  let enqueue x q = q @ [x]
  let front = function [] -> raise Empty | x :: _ -> x
  let dequeue = function [] -> raise Empty | _ :: q -> q
  let size = List.length
  let to_list = Fun.id
end
let fill_listqueue n =
  let rec loop n q =
    if n = 0 then q
    else loop (n - 1) (ListQueue.enqueue n q) in
  loop n ListQueue.empty

module BatchedQueue : Queue = struct
  type 'a t = {o : 'a list; i : 'a list}
  exception Empty
  let empty = {o = []; i = []}
  let is_empty = function
    | {o = []} -> true
    | _ -> false
  let enqueue x = function
    | {o = []} -> {o = [x]; i = []}
    | {o; i} -> {o; i = x :: i}
  let front = function
    | {o = []} -> raise Empty
    | {o = h :: _} -> h
  let dequeue = function
    | {o = []} -> raise Empty
    | {o = [_]; i} -> {o = List.rev i; i = []}
    | {o = _ :: t; i} -> {o = t; i}
  let size {o; i} = List.(length o + length i)
  let to_list {o; i} = o @ List.rev i
end
let fill_batchedqueue n =
  let rec loop n q =
    if n = 0 then q
    else loop (n - 1) (BatchedQueue.enqueue n q) in
  loop n BatchedQueue.empty
module type Map = sig
  type ('k, 'v) t
  val empty  : ('k, 'v) t
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  val lookup : 'k -> ('k, 'v) t -> 'v
  val bindings : ('k, 'v) t -> ('k * 'v) list
end
(**)
(**)
module type Fraction = sig
  type t
  val make : int -> int -> t
  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float
  val add : t -> t -> t
  val mul : t -> t -> t
end
module FRACTION = struct 
  type t = {num: int; den: int} 
  let make n d = if d <> 0 then {num = n; den = d} else raise (Invalid_argument "division by zero")
  let numerator x = x.num
  let denominator x = x.den
  let to_string x = string_of_int x.num^"/"^string_of_int x.den
  let to_float x = float_of_int (numerator x) /. float_of_int (denominator x)
  let add x y = {num = x.num * y.den + y.num * x.den; den = x.den * y.den}

  let mul x y = {num = x.num * y.num; den = x.den * y.den}
end 
module  X : Fraction = FRACTION
module Reduced (FRACTION:Fraction) = struct  

  type t = FRACTION.t
  let rec gcd x y =
  if x = 0 then y
  else if (x < y) then gcd (y - x) x
  else gcd y (x - y)

  let reduce (x:t) = let d = FRACTION.(gcd (numerator x) (denominator x)) in FRACTION.(make ((numerator x)/d) ((denominator x)/d))
  let reduced x = if (x = reduce x) then x else reduce x
 end 
(*this functor implements dictionaries*)
module CharMap = Map.Make(Char)
(*use char map*)
let init = CharMap.add 'a' "Alpha" CharMap.empty  

let echo = CharMap.add 'e' "Echo" init

let sierra = CharMap.add 's' "Sierra" echo
let victor = CharMap.add 'v' "Victor" echo
let find_em x = CharMap.find x victor
let remove_em x = CharMap.remove x victor
(*bindings*)
let f () = CharMap.(empty |> add 'x' 0 |> add 'y' 1 |> bindings)

let g () = CharMap.(empty |> add 'y' 1 |> add 'x' 0 |> bindings)

let h () = CharMap.(empty |> add 'x' 2 |> add 'y' 1 |> remove 'x' |> add 'x' 0 |> bindings)
(*date order*)

type date = {month : int; day : int}

module Date : Map.OrderedType with type t = date = struct
    
    type t = date
  let compare d1 d2 =
    if d1.month = d2.month then d1.day - d2.day
    else d1.month - d2.month

end
(*calender*)
module DateMap = Map.Make (Date : Map.OrderedType)
type calender = string DateMap.t
let newyear = DateMap.(singleton {month =1;day=1} "New Year" )
let popl = DateMap.(add {month =1 ; day =17} "POPL 2026" newyear)

(*takes a calender and adds key value pairs from a list*)
let rec addlist (m: calender) = 
  function 
  []-> m 
  |(h,s) :: t-> 
    let n = DateMap.(add h s m) in addlist n t

(*create calender with a few important dates in it*)

(*print calender*)
let printdaytime d v =print_string (string_of_int d.month^"/"^string_of_int d.day^" is"^v) 
let printcalender (c : calender) : unit = DateMap.iter printdaytime c

(*now create a calender with a few important dates in it*)
(*is for*)
(*first after*)
(*sets*)
module MyOrd = struct 
  type t = string 
  let f x = String.map (Char.lowercase_ascii) x
  let compare x y = String.compare (f x) (f y) 
end


module Insensitive_Case_Tree = Set.Make (MyOrd) 
(*to string*)
module type To_String = sig 
  type t 
  val to_string: t -> string 
end 
(*print*)
module Print (M : To_String) = struct 
  let print x = print_string @@ M.to_string x
end
(*print int*)
module Int : To_String = struct 
  type t = int 
  let to_string = string_of_int 
end 

module PrintInt = Print (Int: To_String)
(*print string*)
module MyString : To_String = struct 
  type t = string 
  let to_string (x:t) :t = x  
end 
(*print string reuse*)
(**implementation without interface*)

(** in [.Modules]*)

(**implementation with interface*)
(** in [.Modules]*)
(*implementation with abstracted interface*)
(** in [.Modules]*)
(* printer for date*)
(** in [.Modules]*)
(*refactor arithmetic*)
(** in [.Modules]*)
