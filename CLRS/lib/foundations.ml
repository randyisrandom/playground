(******************************
 *  DEPENDICIES: 
                array.ml 
                random.ml
 *  means of identifying modules 
       (.ml files) that are used:
                open M
                M.
                M : S 
    hence recognizing these syntactic features 
    allows all .ml files and libraries to be 
    identified so a sub(DAG) can be visualized. 
 ******************************)

 (******************************
 *  TODO: 
        Generalize to other linear data structures 
        rather than just mere linear arrays!

    Shell executable that create corresponding .ml file 
    with let fun_name = failwith "unimplemented" for 
    each val & `Unimplemented for each type  
    
    tel que: 
        lists 
        doubly linked lists 
        FIFO stacks 
 ******************************)
(*type 'a t = 'a Stack.t 
type 'b t = 'b Array.t
type 'c t = 'c Queue.t*)
(*now copy from soon deleted Foundations/main.ml*)

let l = Array.length

let sup a = l a - 1 

let is_sorted (m) (n) (cmp: 'a -> 'a -> int) (a :'a array) : bool = 
  let init = ref true in 
  for i= m to n -1 do if (cmp a.(i) a.(i+1) < 0) then init := true && !init else init := false done; !init 

let sorted_array cmp a = is_sorted 0 (sup a) cmp a 

let normal_progression (n:int) : 'a array = 
  let init = Array.make n 0 in 
    let acc = ref 0 in 
      for i = 0 to n-1 do 
      init.(i)<- !acc; incr acc 
      done; 
  init 

let insert_sort (a : 'a array) : 'a array = 
  for i = 1 to (l a - 1) do let key = ref a.(i) in (*reference arbitrary entries of input array*)
    let j = ref (i - 1) in (*reference predecessor index of array*)
    while ((!j >= 0) && (a.(!j) > !key)) do (*declare loop conditional*)
      a.(!j + 1) <- a.(!j);(**) 
      j := !j-1;(**) 
      a.(!j+1) <- !key (*declare loop action*)
    done 
  done;
  assert (sorted_array Int.compare a);
  a 

let sum (a : 'a array) : 'a = failwith "unimplemented"

let linear_search (a: 'a array) : int = failwith "unimplemented"

let merge (a: 'a array) : 'a array = failwith "unimplemented"

let merge_sort (a: 'a array) : 'a array = failwith "unimplemented"

(*2.1-1*)
(*using #trace illustrate insertion sort for the array*)

(*2.3-2*)
(*State a loop invariant for this procedure, and use its initialization, 
maintenance, and termination properties to show that the SUM-ARRAY 
procedure returns the sum*)

(*2.1-3*)
(*rewrite insertion sort to sort into custom order than specify decreasisng instead of increasing*)

(*2.1-4*)
(*Write code for insertion sort*)

(*2.1-5*)
(*Write ADD-BINARY-INTEGERS*)

(*2.2-1*)
(*Express the function n3 = 1000/100n^2 - 100n/3 in terms of ‚Θ-notation.*)

(*2.2-2*)
(*Implement SELECTION-SORT What loop invariant does this algorithm maintain? Why does it
need to run for only the ûrst n  1 elements, rather than for all n elements? Give the
worst-case running time of selection sort in Θ-notation.
Using ‚Θ-notation, give the average-case and worst-case running times of linear
search. Justify your answers. *)

(*2.2-3*)
(*Consider linear search again (see Exercise 2.1-4). How many elements of the input
array need to be checked on the average, assuming that the element being searched
for is equally likely to be any element in the array? How about in the worst case?*)

(*2.2-4*)
(*How can you modify any sorting algorithm to have a good best-case running time?*)

(*2.3-1*)
(*illustrate the operatiion of merge sort with*)
(*2.3-2*)

(*2.3-3*)
(*State a loop invariant for the while loop of lines 12318 of the MERGE procedure.
Show how to use it, along with the while loops of lines 20323 and 24327, to prove
that the MERGE procedure is correct.*)


(*2.3-4*)
(*Use mathematical induction to show that when n >= 2 is an exact power of 2, the
solution of the recurrence
T(n) = 2           if n = 2 
       2T(n/2) + n if n > 2
  is T(n) = n lg n *)

(*2.3-5*)
(*give a recursive version for insertion sort*)

(*2.3-6*)
(*write iterative or recursive implementation for binary search*)
(*2.3-7*)
(*The while loop of lines 537 of the INSERTION-SORT procedure in Section 2.1
uses a linear search to scan (backward) through the sorted subarray a.(1) to a.(j-1)
What if insertion sort used a binary search (see Exercise 2.3-6) instead of a linear
search? Would that improve the overall worst-case running time of insertion sort
to Θ(n lg n)*)

(*2.3-8*)
(*Describe an algorithm that, given a set S of n integers and another integer x , de
termines whether S contains two elements that sum to exactly x . Your algorithm
should take Θ(n lg n) time in the worst case*)

(**2-1*)
(*Implement megre sort where sublist of length k are inserted with insertion sor*)

(*a.*)
(*b.*)
(*c.*)
(*d.*)

(*2-2*)
(*BUBBLESORT*)
let bubble_sort (a: 'a array) : 'a array = failwith "unimplemented"

(*a.*)
(*b.*)
(*c.*)
(*d.*)

(*2-3*)
(*HORNERS RULE*)
let horner (a : 'a array) : 'a = failwith "unimplemented"
(*a.*)

(*b.*)

(*c.*)

(*2-4*)
(*INVERSION*)
let inversion x y a = failwith "unimplemented"

(*a.*)
(*b.*)
(*c.*)
(*d.*)

(*3.1-1*)
(**)

(*3.1-2*)
(**)

(*3.1-3*)
(**)


(*3.2-1*)
(**)

(*3.2-2*)
(**)

(*3.2-3*)
(**)

(*3.2-4*)
(**)

(*3.2-5*)
(**)

(*3.2-6*)
(**)

(*3.2-7*)
(**)

(*3.3-1*)
(**)

(*3.3-2*)
(**)

(*3.3-3*)
(**)

(*3.3-4*)
(**)

(*3.3-5*)
(**)

(*3.3-6*)
(**)

(*3.3-7*)
(**)

(*3.3-8*)
(**)

(*3.3-9*)
(**)

(*3-1*)
(*a.*)
(*b.*)
(*c.*)
(*d.*)
(*e.*)

(*3-2*)
(*a.*)
(*b.*)
(*c.*)
(*d.*)
(*e.*)
(*f.*)

(*3-3*)
(*a.*)
(*b.*)

(*3-4*)
(*a.*)
(*b.*)
(*c.*)
(*d.*)
(*e.*)
(*f.*)
(*g.*)
(*h.*)

(*3-5*)
(*a.*)
(*b.*)
(*c.*)
(*d.*)
(*e.*)
(*f.*)
(*g.*)

(*3-6*)
(*a.*)
(*b.*)
(*c.*)
(*d.*)
(*e.*)

(*3-7*)
(*a.*)
(*b.*)
(*c.*)
(*d.*)
(*e.*)
(*f.*)
(*g.*)

(*MATRIX MULTIPLY*)

(*MATRIX MULTIPLY RECURSIVE*)
let matrix_mult (a : 'a array array) (b : 'a array array) : 'a array array = failwith "unimplemented"

(*4.1-1*)
(*Generalize matrix-multiply-recursive to handle inputs of general size*)
let rec recursive (a : 'a array array) (b : 'a array array) : 'a array array = failwith "unimplemented"

(*4.1-2*)
(*multiply two rectangular matricies using matrix multiply recursive as a subroutine*)

(*4.1-3*)
(*matrix multiply recursive*)

(*4.1-4*)
(*matrix add recursive, Θ(n ^ 2)*)

(*4.2-1*)
(**multiply [ [|[|1;3|];[|7;5|]|] [|[|6;8|];[|4;2|]|] ] *)

(*4.2-2*)
(*STRASSENS ALGORITHM*)
let strassen (a : 'a array array) (b : 'a array array) : 'a array array = failwith "unimplemented"

(*4.2-3*)


(*4.2-4*)
(*Best running time for 68 x 68, 70 x 70, 72 x 72 matrices*)


(*4.2-5*)
(*define complex numbers as a type and 
  use strasssens algorithm to find them*)


(*4.2-6*)
(*use a Θ(n ^ α) algorithm that square a matrix to implement 
  a algorithm that multiplies two square matrices in Θ(n ^ α)*)


(*4.3-1*)
(*a*)
(*pencil & paper*)
(*b*)
(*c*)
(*d*)
(*e*)
(*f*)
(*g*)

(*4.3-2*)

(*4.3-3*)

(*4.4-1*)
(*a*)
(*b*)
(*c*)
(*d*)
(*4.4-2*)
(*4.4-3*)
(*4.4-4*)

(*4.5-1*)
(*a*)
(*b*)
(*c*)
(*d*)
(*e*)
(*4.5-2*)
(*4.5-3*)
(*4.5-4*)
(*4.5-5*)

(*4.6-1*)
(*4.6-2*)
(*4.6-3*)

(*4.7-1*)
(*4.7-2*)
(*4.7-3*)
(*4.7-4*)
(*4.7-5*)
(*a*)
(*b*)
(*c*)
(*d*)
(*e*)
(*4.7-6*)

(*4-1*)
(*pencil & paper*)

(*4-2*)
(*Give nine recurrences *)

(*4-3*)

(*4-4*)

(*4-5*)

(*4-6*)

(*4-7*)
(*MONGE-ARRAY*)
let monge_array : 'a array array = failwith "unimplemented"


let hire_assistant a = 
  let hired = ref [] in 
  let best = ref 0 in 
  for i = 0 to (Array.length a - 1) do
    if a.(i) > !best then (hired := a.(i)::!hired; best := a.(i)) else ()  
  done 

(*5.1-1*)
(*Show that the assumption that you are always able to determine which candidate is
best, in line 4 of procedure HIRE-ASSISTANT, implies that you know a total order
on the ranks of the candidates.*)

(*5.1-2*)
(**what is expected runtime [Random.int_in_range ~min: 0 ~max: 1]*)

(*5.1-3*)
(*give an algorithm that uses biased random 
as a subroutine and returns unbiased answer*)

let irv (e : bool) : int = 
  match e with 
  |true -> 1
  |false -> 0


(*5.2-1*)
(*Assuming the canidates are in random order*)

(*5.2-2*)
(*given a random permutation what is the probability*)

(*5.2-3*)
(*use irvs to compute the expected sum of n dice*)

(*5.2-4*)
(*demonstrate the linearity of expectation for irvs*)

(*5.2-5*)
(*what is the expected number of elements that remain invariant 
  given a random permutation in a n element list*)

(*5.2-6*)
(*for a random uniform permutation use irvs to calculate expected  
  number of inversions*)


(*5.3-1*)
(*rewrite random-permute so the loop invariant applies to a nonempty 
  subarray *)
let random_permute (a : 'a array) : 'a array = failwith "unimplemented"
(*5.3-2*)
(*is permute-without-id effective
  satisfy [pre/post]condtions*)
let sans_id (a: 'a array) : 'a array = failwith "unimplemented"


(*5.3-3*)
(*does permute with all produce a uniform random permutation*)
let with_all (a : 'a array) : 'a array = failwith "unimplemented"



(*5.3-4*)
(*show that permute by cycle does not produce a uniform random permutation*)
let permute_by_cycle (a : 'a array) : 'a array = failwith "unimplemented"


(*5.3-5*)
(*show random sample returns a m-element subset such that each subset is equally likelly to be created*)
let random_sample (m : int) (n: int) (a : 'a array) : 'a array = failwith "unimplemented"


let online_max (m : int) (n: int) (a : 'a array) : 'a array = failwith "unimplemented"

(*5.4-1*)
(**)

(*5.4-2*)
(**)
(*5.4-3*)
(**)

(*5.4-4*)
(**)

(*5.4-5*)
(**)

(*5.4-6*)
(**)


(*5.4-7*)
(**)

(*5.4-8*)
(**)

(*5-1*)
(*PROBABILISTIC COUNTING*)
(*a*)
(*b*)

(*5-2*)
(*SEARCHING AN UNSORTED ARRAY*)
(*a*)
(*b*)
(*c*)
(*d*)



let binary_sort (a: 'a array) : int = failwith "unimplemented"

module Arraylist = struct 
type t 
  let create n acc = Array.make n acc 
  let empty = [||]
  let hd a = a.(0) 
   let tl a = 
    let init = Array.make (Array.length a - 1) a.(0) in 
    for i=1 to (Array.length a -1) do init.(i-1) <- a.(i) done; init;;
  let cons a b = failwith "u"
end






