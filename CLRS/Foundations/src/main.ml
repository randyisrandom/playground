(*https://ilyasergey.net/YSC2229/index.html*)

(*convinent functions*)


let l = Array.length

let sup a = l a - 1 


let swap a b = 
  let t = !a in 
    a := !b; b := t  

let aswap a i n = let t = a.(i) in a.(i) <- a.(n); a.(n) <- t

let rantran a i = aswap a i (Random.int (l a))

let rec rndomls = function  
  0->[]
  |n -> (Random.int 100) :: rndomls (n-1)
(******************************
 *insert sort
 ******************************)
let insertsort a =
  for i = 1 to (l a - 1) do 
    let key = ref a.(i) in (*reference arbitrary entries of input array*)
    let j = ref (i - 1) in (*reference predecessor index of array*)
    while ((!j >= 0) && (a.(!j) > !key)) do (*declare loop conditional*)
      a.(!j + 1) <- a.(!j);(**) 
      j := !j-1;(**) 
      a.(!j+1) <- !key (*declare loop action*)
    done 
  done; a

let x = [|31; 41; 59; 26; 41; 58|]
(******************************
 *sum
 ******************************)
let sum a = 
  let acc = ref 0 in 
  for i = 0 to (l a - 1) do 
    acc := !acc + a.(i) done; !acc 
    
let std_sum = Array.fold_left (+) 0  
(******************************
 *optimized insert sort
 ******************************)
let insert_sort_min a =
  for i = 1 to (l a - 1) do 
    let key = ref a.(i) in 
    let j = ref (i - 1) in 
    while ((!j >= 0) && (a.(!j) < !key)) do 
      a.(!j + 1) <- a.(!j); j := !j-1; a.(!j+1) <- !key 
    done 
  done; a
(******************************
 *Search
 ******************************)
let aux_search x a = if a.(0) =x then for i = 0 to (l a - 1) do if a.(i) = x then print_string (string_of_int i) else if a.(i) <> x then print_string "" else raise Not_found done

let search x a = 
  let init = ref 0 in 
  for i = 0 to (l a - 1) do 
    if (a.(i) = x) then init := i else failwith "not found"
  done; !init

(******************************
 *BINARY-INTEGER-ADDITION 
 ******************************)

let binadd a1 a2 = 
  let c = Array.make (l a1 + 1) 0 in 
  for i = 0 to (l a1 - 1) do 
    if (a1.(i) + a2.(i) > 1) then 
      c.(i) <- (a1.(i)+a2.(i)) else 
        c.(i+1) <- (a1.(i)+a2.(i)) 
      done; c 

(**Time Complexity of insertion sort*)

(******************************
 *selection sort
 ******************************)
 let find_min a m = let init = ref a.(m) in for i = m to (l a - 1) do if a.(i) <= !init then init := a.(i) else () done; !init
 (*loop process*)
let selection_sort a = 
  let init = ref (-1) in 
  for i = 0 to (l a - 1) do 
    init := !init +1; 
    a.(i)<- (find_min a (!init))  
  done; a 
(******************************
 *linear search
 ******************************)

(******************************
 * How can you modify any sorting algorithm to have a good best-case running time?
 ******************************)

(******************************
 *merge sort
 ******************************)
 (**Precondition [A[p,q] && A[q+1,r] are sorted]*)
 let merge a p q r =
  let nl = q - p + 1 and nr = r - q in (*define *)
  let init_l = Array.make (nl) 0 and init_r = Array.make (nr) 0 in (**)
  for i = 0 to (nl - 1) do init_l.(i) <- a.(p+i) done; (**)
  for j = 0 to (nr - 1) do init_r.(j) <- a.(q+j+1) done;
  let i = ref 0 and
  j = ref 0 and
  k = ref p in 
  while ((!i < nl) && (!j < nr)) do 
    if (init_l.(!i) <= init_r.(!j)) then 
      (a.(!k) <- init_l.(!i); i := !i + 1) else 
        (a.(!k) <- init_r.(!j); j:= !j+1); k := !k + 1; done;   
        while !i < nl do a.(!k) <- init_l.(!i); i := !i + 1; k := !k + 1 done; 
        while !j < nr do a.(!k) <- init_r.(!j); j := !j + 1; k := !k + 1 done

 (*let merge a1 a2 = let init1 = Array.make (l a1 -1) in 
 let init2 = Array.make (l a2 -1) in for i = 0 to (l a1 -1) do*) 
 
 let rec merge_sort a p r = 
  if p >=r then failwith "invalid indicies" else 
    let q = (p+r)/2 in 
    (merge_sort a p q; merge_sort a (q+1) r; merge a p q r) 
    
  let return f a = f a; a
(******************************
 *binary search
 ******************************)
 (**Precondition: [is_sorted _ = true]
                  []
 *)

 let binary search x a = 
  if x <= a.((l a -1)/2) then 
    failwith "implement linear search half" else 
      failwith "(*linear search half*)"

(******************************
 *bubblesort
 ******************************)

let bubblesort = failwith "unimplemented"

(******************************
 *horners rule
 ******************************)

let horner = failwith "unimplemented"

(******************************
 *inversion
 ******************************)

let inversion = failwith "unimplemented"

(******************************
 * matrix multiply 
 ******************************)

module Matrix = struct 
  type 'a t = 'a array array
  let (|*|) (a: 'a t) (b:'a t) = failwith "unimplemented" 

end

(******************************
 *MATRIX MULTIPLY RECURSIVE
 ******************************)

 let rec (|*|) (a : 'a array) (b: 'a array) : 'a array = failwith "unimplemented"

(******************************
 *STRASSENS ALGORITHM
 ******************************)

 let strassen a = failwith "unimplemented"
(**RANOMIZED ALGORITHMS*)

(******************************
 *HIRE ASSISTANT
 ******************************)

let hire_assistant = failwith "unimplemented"

(******************************
 *RANODMIZED HIRING ASSISTANT
 ******************************)

let random_hire = failwith "unimplemented"

(******************************
 *RANDOMLY PERMUTE
 ******************************)

let rand_permute (a:'a array) = 
  for i = 0 to (l a - 1) do (*O(l a - 1)*) 
    rantran a i(*nO(?)*) 
  done; a 
  
(******************************
 *PERMUTE WITHOUT IDENTITY
 ******************************)

let permute_sans_id = failwith "unimplemented"

(******************************
 *PERMUTE WITH ALL
 ******************************)

let permute_all = failwith "unimplemented"

(******************************
 *PERMUTE BY CYCLE 
 ******************************)

let permute_by_cycle = failwith "unimplemented"

(******************************
 *RANDOM SAMPLE
 ******************************)

let random_sample = failwith "unimplemented"

(******************************
 *ONLINE MAXIMUM
 ******************************)

let permute_all = failwith "unimplemented"

(******************************
 *PROBALISTIC COUNTING
 ******************************)

let pcount = failwith "unimplemented"

(******************************
 *DETERMINISTIC SEARCH
 ******************************)

let deterministic_search = failwith "unimplemented"