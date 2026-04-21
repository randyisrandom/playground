(********************************************************************)
(**                        Computation                             **)
(*Introduces basic operational semantics, evaluation by hand, tail recursion and cps*)
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
(*Understanding the simplicity of Type Inference by hand helps for future endeavors*)
(*in languages with powerful but undecidable type systems like agda *)
(*Variables and Pattern Variables*)
(*fix combinator and recursion in ocaml*)
(*grammar for values*)
(*substitution*)
(*redexes*)

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