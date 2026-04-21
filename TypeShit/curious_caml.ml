module type Wildcard
(********************************************************************)
(**                          LOGOS                                 **)
(* Write a script to make this file a concatenation of all the 
   chapters                                                         *)
(*why'd I write this
  a shallow survey of logic*)

(********************************************************************)
(********************************************************************)
(**                          Algebra                               **)
(*Type inference: Pencil, Paper, and beyond*)
(*assign type variables to unknowns ? and ?a *)
(*generate constraints*)
(*solve *)

(*How the type checker is realized*)

(*the exception with exceptions*)


(*Curried form*)


(********************************************************************)
(********************************************************************)
(**                        Computation                             **)
(*Introduces basic operational semantics, evaluation by hand, tail recursion and cps*)


(*reduction semantics*)
(*https://pauillac.inria.fr/~remy/cours/appsem/*)
(*https://en.wikipedia.org/wiki/Operational_semantics*)
(*Understanding the simplicity of Type Inference by hand helps for future endeavors*)
(*in languages with powerful but undecidable type systems like agda *)

(*lambda calculus with 4 extensions*)
(*constructors*)
(*built in values of arity n*)
(*name binding*)
(*pattern matching variables*)
(*pattern variables*)
(*tuple patterns*)
(*variant patterns*)

(*Variables and Pattern Variables*)
(*fix combinator and recursion in ocaml*)
(*grammar for values*)
(*substitution*)
(*redexes*)


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

(********************************************************************)
(********************************************************************)
(**                        Functions                               **)
(*Language and Rules of the Untyped Lambda*)
(*computation by hand review*)

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
(*rocq/engine logic monad modules*)
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