(*This Chapter is an introduction to the books theme of compliation by hand*)
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