(*Function Specifications*)
(*spec game*)
(*poly spec*)
(** [Poly] represents immutable polynomials with integer coefficients. *)
module type Poly = sig
  (** [t] is the type of polynomials. *)
  type t

  (** [eval x p] is [p] evaluated at [x]. Example: if [p] represents
      $3x^3 + x^2 + x$, then [eval 10 p] is [3110]. *)
  val eval : int -> t -> int
end
(*poly impl*)
(*interval arithmetic*)
(*function maps*)
(*set black box*)
(*set glass box*)
(*random lists*)
(*qcheck odd divisor*)
(** [odd_divisor x] is an odd divisor of [x].
    Requires: [x >= 0]. *)
let odd_divisor x =
  if x < 3 then 1 else
    let rec search y =
      if y >= x then y  (* exceeded upper bound *)
      else if x mod y = 0 then y  (* found a divisor! *)
      else search (y + 2) (* skip evens *)
    in search 3
(*qcheck avg*)
(*exp*)
(*fibi*)
(*expsq*)
(*expsq simplified*)
(*mult*)
(*append nil*) 
(*rev dist append*)
(*rev involutive*)
(*reflect size*)
(*fold theorem 2*)
(*propositions*)
(*list spec*)
(*bag spec*)