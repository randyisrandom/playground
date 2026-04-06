module type LinearData = sig 
  type 'a t
  val cons: 'a -> 'a t -> 'a t
  val hd: 'a t -> 'a 
 end 

module type Heap = sig 
  type elt
  type heap
  type 'a t 
  val parent: heap -> int 
  val left: heap -> int
  val right: heap -> int 
  val max_heapify: heap -> int -> heap
  val build_maxheap: 'a t -> heap
  val heap_sort: 'a t -> 'a t 
end 

module type PersistentQueue = sig end 

module type Ord = sig 
  type t 
  val cmp: t -> t -> int 
end

module Array (M : Ord) (N : LinearData) : Heap 
  with type elt = M.t 
  with type 'a t = 'a N.t 

module Tree (M : Ord) : Heap with type elt = M.t 

(*The rest covers sorting on linear data structures*)

val quick_sort: 'a -> 'a 
val partition: 'a -> int 

val random_partition: 'a -> int
val random_qsort: 'a -> 'a 

val hoare_partition: 'a -> int

val tre_quicksort: 'a -> int

val counting_sort: 'a -> int 

val bucketsort: 'a -> int 

val compare_exchange: 'a -> int -> int -> 'a

val minimum: 'a -> 'b

val randomized_select: 'a -> int -> int -> int -> 'a

val select: 'a -> int -> int -> int -> 'a

val simpler_rselect: 'a -> int -> int -> int -> 'a

val select3: 'a -> int -> int -> int -> 'a



