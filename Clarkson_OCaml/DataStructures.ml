(*hash insert*)
module type TableMap = sig
  type ('k, 'v) t
  val insert : 'k -> 'v -> ('k, 'v) t -> unit
  val find : 'k -> ('k, 'v) t -> 'v option
  val remove : 'k -> ('k, 'v) t -> unit
  val create : ('k -> int) -> int -> ('k, 'v) t
  val bindings : ('k, 'v) t -> ('k * 'v) list
  val of_list : ('k -> int) -> ('k * 'v) list -> ('k, 'v) t
end

module HashMap : TableMap = struct
  type ('k, 'v) t = {
    hash : 'k -> int;
    mutable size : int;
    mutable buckets : ('k * 'v) list array
  }
  let capacity {buckets} =
    Array.length buckets
  let load_factor tab =
    float_of_int tab.size /. float_of_int (capacity tab)
  let create hash n =
    {hash; size = 0; buckets = Array.make n []}

  let index k tab =
    (tab.hash k) mod (capacity tab)

    let insert_no_resize k v tab =
      let b = index k tab in 
      let old_bucket = tab.buckets.(b) in
      tab.buckets.(b) <- (k,v) :: List.remove_assoc k old_bucket; (* O(L) *)
      if not (List.mem_assoc k old_bucket) then
        tab.size <- tab.size + 1;
    ()

  let rehash tab new_capacity =
    let rehash_binding (k, v) =
      insert_no_resize k v tab
    in
    let rehash_bucket bucket =
      List.iter rehash_binding bucket
    in
    let old_buckets = tab.buckets in
    tab.buckets <- Array.make new_capacity []; 
    tab.size <- 0;
    Array.iter rehash_bucket old_buckets 
  let resize_if_needed tab =
    let lf = load_factor tab in
    if lf > 2.0 then
      rehash tab (capacity tab * 2)
    else if lf < 0.5 then
      rehash tab (capacity tab / 2)
    else ()

  (** Efficiency: O(n). *)
  let insert k v tab =
    insert_no_resize k v tab; (* O(L) *)
    resize_if_needed tab (* O(n) *)

  (** Efficiency: expected O(L). *)
  let find k tab =
    List.assoc_opt k tab.buckets.(index k tab)

  let remove_no_resize k tab =
    let b = index k tab in
    let old_bucket = tab.buckets.(b) in
    tab.buckets.(b) <- List.remove_assoc k tab.buckets.(b);
    if List.mem_assoc k old_bucket then
      tab.size <- tab.size - 1;
    ()

  let remove k tab =
    remove_no_resize k tab; (* O(L) *)
    resize_if_needed tab (* O(n) *)

  let bindings tab =
    Array.fold_left
      (fun acc bucket ->
         List.fold_left
           (* 1 cons for every binding, which is O(n) *)
           (fun acc (k,v) -> (k,v) :: acc)
           acc bucket)
      [] tab.buckets

  let of_list hash lst =
    let m = create hash (List.length lst) in  (* O(n) *)
    List.iter (fun (k, v) -> insert k v m) lst; (* n * O(n) is O(n^2) *)
    m
end

let table ():(int,string) HashMap.t = 
  let hash k = k mod 7 in HashMap.create hash 7
  
  let rec insert_list ls h = 
    match ls with 
    |[]-> h 
    | (k,v) :: t -> insert_list t (HashMap.insert k v h; h)

  let _ = insert_list [(4,"And");(8,"then");(15,"Yaweh");(16,"spoke");(23,"from the");(42,"whirlwhind! ")] (table ())

(*relax bucket RI*)

(*if duplicates were allowed then 
    insert would execute an additional operation increasing the size of the list
    find: would be shorter at least 
    remove: would have to act more than once*)

(*strengthen bucket RI*)

(*if each bucket were sorted by the key*)

(*hash values*)

let _ = Hashtbl.hash

(*hashtbl usage*)

let i_tab = Hashtbl.create 16 

let rec insert_list ls h = 
    match ls with 
    |[]-> h 
    |(k,v) :: t -> insert_list t (Hashtbl.add h k v; h)

let x = [(1, ""); (2, ""); (3, ""); (4, ""); (5, ""); (6, ""); (7, ""); (8, "");
   (9, ""); (10, ""); (11, ""); (12, ""); (13, ""); (14, ""); (15, "");
   (16, ""); (17, ""); (18, ""); (19, ""); (20, ""); (21, ""); (22, "");
   (23, ""); (24, ""); (25, ""); (26, ""); (27, ""); (28, ""); (29, "");
   (30, ""); (31, "")]

let tab = insert_list x i_tab 

let find n = Hashtbl.find i_tab n

(*hashtbl stats*)

let f1 _ = Hashtbl.stats tab 

(*amend to take tab result as input directly*)
let result _ = print_string "there are _ buckets that contain a single binding"

(* hashtbl bindings *)

(*rather complicated*)
let f (t:('a,'b) Hashtbl.t) = Hashtbl.fold (fun x ->fun y -> fun acc -> (x,y)::acc) t []


(*hashtbl load factor*)

let load_factor x = 
  let num h = (Hashtbl.stats h).num_bindings in 
  let den g = (Hashtbl.stats g).num_buckets in (num x) / (den x)
(*functorial interface*)

(*equals and hash*)

(*bad hash*)
(*the functorial interface*)

(*linear probing*)

(*functorized BST*)
module Clarksons_BST = struct 
  type 'a tree = Node of 'a * 'a tree * 'a tree | Leaf

(** [mem x t] is [true] iff [x] is a member of [t]. *)
let rec mem x = function
  | Leaf -> false
  | Node (y, l, r) ->
    if x < y then mem x l
    else if x > y then mem x r
    else true

(** [insert x t] is [t] with [x] inserted as a member. *)
let rec insert x = function
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (y, l, r) as t ->
    if x < y then Node (y, insert x l, r)
    else if x > y then Node (y, l, insert x r)
    else t
end 

(*efficient traversal*)
(*RB draw complete*)

  (*Drawn on paper, but use of graphic library should be learned*)

(*RB draw insert*)

  (*Drawn on paper, but use of graphic library should be learned*)

(*pow2*)
type 'a sequence = Cons of 'a * (unit -> 'a sequence)

let rec pow2 (Cons (h,t)) = Cons (h*h,fun () -> pow2 (t ()))
(*more sequences*)
(*nth*)
let rec nth n (Cons (h,t)) = match n with 0 -> h | n -> nth (n-1) (t ())

(*hd tl*)
let hd (Cons (h,t)) = h

let tl (Cons (h,t)) = t ()
(*filter*)
(*interleave*)
(*sift*)
(*primes*)
(* approximately e*)
(*better e*)
(*different sequence rep*)
(*lazy hello*)
(*lazy and*)
(*lazy sequence*)
