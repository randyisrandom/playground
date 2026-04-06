module type Stack = sig 
    type elt 
    type t
    val push: elt -> t -> t
    val pop: t -> t
    val enque: elt -> t -> t
    val deque: t -> t 
end 

module type DL_List = sig end 

module type Tree = sig end 

val compact_search: 'a -> 'a

module type DirectAddressTable = sig 
    type t
    val search: 'a -> t -> 'b
    val insert: 'a -> t -> t 
    val delete: 'a -> t -> t
    val hash: 'a -> int 
end 

(*redblack trees*)