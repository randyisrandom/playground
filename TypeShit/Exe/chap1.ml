module type Wildcard
(********************************************************************)
(**                          LOGOS                                 **)

(*This chapter gives an overview of logic and*)
let vrai = ()

exception Unimplemented
exception False
exception Neither 

type void = |

let faux (x: 'a) : void = match x with _ -> raise False 

type ('a,'b) or_variant = 
  A of 'a 
 |B of 'b 

 (*via patern matching*)
let p_either (f: 'a -> 'c) (g: 'b -> 'd) = function 
  |A x -> f x
  |B y -> g y
(*|_ -> raise Neither*)

let is_A = function A x -> true |_ -> false 
let is_B = function B x -> true |_ -> false

let c_either  (f: 'a -> 'c) (g: 'b -> 'd) x = 
  if (is_A x) then f x 
    else if (is_B x) then g x 
      else raise Neither 

module Exercises2 (_:Wildcard) = struct
  print_string""
  (*REPL calculator drills*)
  (*see toplevels *)
  (*the volume of a sphere with radius 5*)
  (*shipping cost of books as function composition or imperative sequence*)
  (*5 mile morning jog time*)

  (*Recursive Fibonacci*)
  let rec fib = function 0 -> 1|1->1|n -> fib (n - 1) + fib (n-2) 
  (*Recursive Palindromes*)
  let first_char word = word.[0]
  let last_char word =
  let len = String.length word - 1 in
  word.[len]
  let middle word =
  let len = String.length word - 2 in
  String.sub word 1 len
  
  (*Euclid's GCD*)
  let rec gcd (x: int) (y: int) : int = 
    match y with 
    |0 -> x 
    |n when n >= x -> gcd (n-x) x
    |_ -> gcd y x
end