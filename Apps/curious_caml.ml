module type Wildcard
(********************************************************************)
(**                          LOGOS                                 **)

(*why'd I write this
  a shallow survey of logic*)
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
  type time = {hour :int; minute : int}

  let hour x = if (x.hour>12) then ((x.hour) mod 12) else x.hour 
  and minute x = if (x.minute>60) then ((x.minute) mod 60) else x.minute
  let easy x = 8.25 *. x
  let tempo x = 7.20 *. x

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
(********************************************************************)
(********************************************************************)
(**                          Algebra                               **)
(*Type inference: Pencil, Paper, and beyond*)
(*assign type variables to unknowns*)
(*generate constraints*)
(*solve *)

(*How the type checker is realized*)

(*the exception with exceptions*)


(*Curried form*)
module FunctionSyntax (_ : Wildcard) = struct
  let curried f e = fun x y -> (f e)
  let uncurried f e = fun x -> fun y -> (f e)

  let non_anon f phi psi = function A x -> phi x |B y -> psi y
  let anon f phi psi = fun p -> match p with A x -> phi x |B y -> psi y

  let f phi x = (phi x) 
  let f phi = fun x -> (phi x)
end
(*pattern matching*)
module PatternMatching (_ : Wildcard) = struct 
  (*The theory of logical condition and state*)

end 
(*ADTs as polynomials*)
let () = print_string "an initial object in a polynomial functor"
module PolynomialADTs (_ : Wildcard) = struct
  (*bool = 2*)
  type bool = true | false 
  (* x = 1 + x *)
  type 'a option = None | Some of 'a
  (* L = 1 + xL *)
  type 'a list = [] | (::) of 'a * 'a list

  (*core ml*)
  (*x+y*)
  type name = Name of string | Int of int
  (*aby*)
  type constant = {name:name; constr : bool; arity : int}
  (*x*)
  type var = string 
  (* E = x + aby + x*E + E*E + x*E*E *)
  type expr = 
    |Var of var
    |Const of constant
    |Fun of var * expr
    |App of expr * expr
    |Let of var * expr * expr
  
    (* T = 1 + x*T^2 *)
  type 'a tree = E | T of 'a * 'a tree * 'a tree
  (*isomorphism between adt polynomials*)
  (* T =  *) 
end
(*Differntiation*)
module Differentiation (_ : Wildcard) = struct
   
end

(********************************************************************)
(********************************************************************)
(**                        Computation                             **)
module _ (_ : Wildcard) = struct
  let (-|) f g x = f (g x)(*composition op*)
  let rec power f n = 
    if n <=0 then (fun x -> x) else (f -| power f (n-1))
  let add n = power ((+) 1) n
  let mult k n = power ((+) k) n 0
  end
module Exercises (_:Wildcard) = struct 
  (*1*)
  (*2*)
  (*3*)
end
(*reduction semantics*)
(*https://pauillac.inria.fr/~remy/cours/appsem/*)
module _ (_ : Wildcard) = struct
  (*Fix prmitive*)
  let e1 = ()
  let e2 = ()
  let fix f =
  let rec self x = f self x in
  self
  let rec f x = e1 in e2 
  let f = fix (fun f x -> e1) in e2
  "expressions values substitution redexes rule variables context rules fix"
end
module _ (_ : Wildcard) = struct 
  (* E = x + y + 4E^2 *)
  type expression =
    | Const of float
    | Var of string
    | Sum of expression * expression    (* e1 + e2 *)
    | Diff of expression * expression   (* e1 - e2 *)
    | Prod of expression * expression   (* e1 * e2 *)
    | Quot of expression * expression   (* e1 / e2 *)
  (*So what meaningful value would derivatives and isomorphic types have*)
  (*if a ADT's a & b are isomorphic how would functions between derivative a and b be defined*)
  exception Unbound_variable of string

  let rec eval env exp =
    match exp with
    | Const c -> c
    | Var v ->
      (try List.assoc v env with Not_found -> raise (Unbound_variable v))
    | Sum(f, g) -> eval env f +. eval env g(*f and g are one of the six variants that maybe further reduced*)
    | Diff(f, g) -> eval env f -. eval env g
    | Prod(f, g) -> eval env f *. eval env g
    | Quot(f, g) -> eval env f /. eval env g

  let rec deriv exp dv =
    match exp with
    | Const _ -> Const 0.0
    | Var v -> if v = dv then Const 1.0 else Const 0.0
    | Sum(f, g) -> Sum(deriv f dv, deriv g dv)
    | Diff(f, g) -> Diff(deriv f dv, deriv g dv)
    | Prod(f, g) -> Sum(Prod(f, deriv g dv), Prod(deriv f dv, g))
    | Quot(f, g) -> Quot(Diff(Prod(deriv f dv, g), Prod(f, deriv g dv)),
                         Prod(g, g))

  let x = Var "x"
  let y = Var "y"
  let z = Var "z"
  let (+:) f g = Sum (f, g)
  let (-:) f g = Diff (f, g)
  let ( *: ) f g = Prod (f, g)
  let (/:) f g = Quot (f, g)
  let (!:) i = Const i

  let print_expr ppf exp =
  let open_paren prec op_prec =
    if prec > op_prec then Format.fprintf ppf "(@["
    else Format.fprintf ppf "@[" in
  let close_paren prec op_prec =
    if prec > op_prec then Format.fprintf ppf "@])"
    else Format.fprintf ppf "@]" in
  let rec print prec exp =
    match exp with
    | Const c -> Format.fprintf ppf "%.2f" c
    | Var v -> Format.fprintf ppf "%s" v
    | Sum(f, g) ->
      open_paren prec 0;
      print 0 f; Format.fprintf ppf "@ +@ "; print 0 g;
      close_paren prec 0
    | Diff(f, g) ->
      open_paren prec 0;
      print 0 f; Format.fprintf ppf "@ -@ "; print 1 g;
      close_paren prec 0
    | Prod(f, g) ->
      open_paren prec 2;
      print 2 f; Format.fprintf ppf "@ *@ "; print 2 g;
      close_paren prec 2
    | Quot(f, g) ->
      open_paren prec 2;
      print 2 f; Format.fprintf ppf "@ /@ "; print 3 g;
      close_paren prec 2
  in
  print 0 exp
end
(*tail recursion*)
(*
  1. Make the parameters of the functions mutable references, 
      so they could be re-assigned at each loop iteration.
*)
(*
  2. Make the branch-condition of “base” case to be that of the while-loop. 
     Whatever post-processing of the result takes place in the base cases, 
      should now be done after the loop.
*)
(*
1.Put Iteration State First: 
   The iteration state (e.g., counters, indices) should 
    be the first argument.
*)
(*
2.Use Accumulators: Use additional parameters to 
   carry intermediate results.
*)
(*
3.Recursive Call as the Last Operation: Ensure the recursive call is the final action in the function.
*)
module _ (_ : Wildcard) = struct
  let rec count n = 
    if n<= 0 then 0 else 1 + (count (n-1))
  "While loop equivalence 
  constructing while loops from any loop" 
end
(*continuation passing system*)
module _ (_ : Wildcard) = struct
  (*STEPS*)
let rec depth_cps tree k = match tree with
  | Tip -> k 0
  | Node(_, left, right) ->
    depth_cps left (fun dleft ->
      depth_cps right (fun dright ->
        k (1 + (max dleft dright))))

let depth tree = depth_cps tree (fun d -> d)  
(*
1. 
  The Continuation Parameter:
*)
(*
2.
  The Base Case
*)
(*
3.
  The Recursive Case
*)
(*
4.
  The Wrapper Function
*)
  (*a continuation is a function that decides what to do next*)
end
module Exercises (_:Wildcard) = struct 
  (*1*)
  (*2*)
  (*3*)
end
(********************************************************************)
(********************************************************************)
(**                        Functions                               **)
(*Language and Rules of the Untyped Lambda*)


let cn0 = fun f x -> x        (* The same as c_false *)
let cn1 = fun f x -> f x      (* Behaves like identity when f = id *)
let cn2 = fun f x -> f (f x)
let cn3 = fun f x -> f (f (f x))
(*Fix point combinators*)
(*Encoding Lists and Trees*)
(*Looping and Recursion*)
module Exercises (_:Wildcard) = struct 
  (*1*)
  (*2*)
  (*3*)
end
(********************************************************************)
(********************************************************************)
(**                        Polymorphism                            **)
(*type inference*)

(********************************************************************)
(********************************************************************)
(**                  Folding & Backtracking                        **)
(*Identifying common recursion pattern and refactoring*)
(********************************************************************)
(********************************************************************)
(**                        Laziness                                **)
(*In Haskell*)
(*other languages*)
(*What a thunk is*)
(********************************************************************)
(********************************************************************)
(**                          Monads                                **)
(*in Haskell monads have stronger algebraic properties*)
(*monad laws*)
(*1*)
(*2*)
(*3*)
(*lwt library connect with pragmattic programmer guidlines*)

(********************************************************************)
(********************************************************************)
(**                             GADT's                             **)
(*concurrency connect with iit madras course*)
(********************************************************************)
(********************************************************************)
(**              Functional Reactive Programming                   **)
(*Introduce tradidional OO background for gui and graphics*)

(********************************************************************)
(*The Expression Problem *)
(********************************************************************)
(*Categories and Gadt's*)
(********************************************************************)