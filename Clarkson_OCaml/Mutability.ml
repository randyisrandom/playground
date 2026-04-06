module Pointer = struct 
  (**)
  type 'a pointer = 'a ref option
  let null : 'a pointer = None
  let malloc (x : 'a) : 'a pointer = Some (ref x)
  let p = malloc 42 

  exception Segfault
  let deref (ptr : 'a pointer) : 'a =
    match ptr with None -> raise Segfault | Some r -> !r
  let ( ~* ) = deref
  let assign (ptr : 'a pointer) (x : 'a) : unit =
    match ptr with None -> raise Segfault | Some r -> r := x
  let ( =* ) = assign
end 

module IMP_FACTORIAL = struct 
  let rec fact_rec n = if n = 0 then 1 else n * fact_rec (n - 1)  
  let fact0 = ref (fun x -> x + 0)
  let f () = !fact0 5
  let fact n = if n = 0 then 1 else n * !fact0 (n - 1)
end 

module ExampleC = struct
  (*defined via mutually recursive types*)
  type 'a node = { next : 'a mlist; value : 'a ref }
and 'a mlist = 'a node option ref

let empty () : 'a mlist = ref None

let insert_first (lst : 'a mlist) (v : 'a) : unit =
  lst := Some { next = ref !lst; value = ref v }

let rec set (lst : 'a mlist) (n : int) (v : 'a) : unit =
  match (!lst, n) with
  | None, _ -> invalid_arg "out of bounds"
  | Some { value }, 0 -> value := v
  | Some { next }, _ -> set next (n - 1) v

let rec to_list (lst : 'a mlist) : 'a list =
  match !lst with None -> [] | Some { next; value } -> !value :: to_list next
end
module type MutableStack = sig
  type 'a t
  (** Raised if [peek] or [pop] encounter the empty stack. *)
  exception Empty
  val empty : unit -> 'a t
  val push : 'a -> 'a t -> unit
  val peek : 'a t -> 'a
  val pop : 'a t -> unit
end
module MutableRecordStack : MutableStack = struct

  type 'a node = {value : 'a; mutable next : 'a node option}
  type 'a t = {mutable top : 'a node option}
  exception Empty
  let empty () = {top = None}
  let push x s = s.top <- Some {value = x; next = s.top}
  let peek s =
    match s.top with
    | None -> raise Empty
    | Some {value} -> value

  let pop s =
    match s.top with
    | None -> raise Empty
    | Some {next} -> s.top <- next
end

(*muatable field*)
type student = {name : string; mutable gpa : float}
let alice = {name = "alice"; gpa = 3.7}
let perfect x = x.gpa <- 4.0; x
(*refs*)

(*inc fun*)
let inc = ref (fun x -> x + 1)
let _ = let x =3109 in ref x  
(*addition assignment *)
let (+:=) x y = x := !x + y
(*physical equality*)
let x = ref 0
let y = x
let z = ref 0
(*norm*)
(* AF: the float array [| x1; ...; xn |] represents the
 *     vector (x1, ..., xn)
 * RI: the array is non-empty *)
type vector = float array

let norm v = Array.fold_left (-.) 0. (Array.map (fun x -> x *. x) v) |> sqrt

(*normalize*)

let normalize v = Array.map (fun x -> x/.(norm v)) v
  (*normalize : vector -> unit*)

(*norm loop *)

let norm a = 
  let init = ref 0.0 in 
  for i=0 to (Array.length a - 1) do 
    init := a.(i) *. a.(i) +. !init done; 
    sqrt !init  

(*init matrix*)

let init_matrix n o f = ()
(**Postcondition: *)
(**Example: [init_matrix 2 2 + = [|0;1|][|1;2|]]*)