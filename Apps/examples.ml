(********************************************************************)
(* Review of modern and legacy use of objective caml and            *)
(* architecture common projects *)
(**)
(**)
(**)
(**)
(********************************************************************)
(*A composition of development stratigies for OCaml 3. *)


(*syntax*)
fun x -> x

let id = function x -> x

let cond x = if x = true then x else false 
let cond_match = function 
  true -> true
  |false->false 
(*lambda expressions and loops*)

(*semantics*)
(*https://ocaml.org/manual/5.2/polymorphism.html*)

(*functional & imperative*)

(*exceptions*)

(*laziness and strictenss*)

(*idioms*)
(*defining useful input modules and 
extending server provided libraries*)

(*handling io data*)
module type Input = sig
  (*key data type*)
  type t 
  (*compound*)
  type 'a d
  (*defines how key data is represented for given in_channel data*)
  val data: in_channel -> t 
  (*defines how key data is compounded for given in_channel data*)
  val buf_function: in_channel -> t d
  (*defines how compound data type is read given a filename*)
  val in_function: string -> t d 
end
module type Output = sig
  (**key data type declaration*)
  type t 
  (**compound data type declaration*)
  type 'a d
  (*defines operation on key data for given output type*)
  val data: out_channel -> t -> unit 
  (*defines operation on compounded data for given output type*)
  val buf_function: out_channel -> t d -> unit
  (*defines how compounded data type is written given a filename *)
  val out_function: string -> t d -> unit
end
module In = struct
  type t = string
  type 'a d = 'a list
  let data (ch : in_channel)  = input_line ch 

  let rec buf_function ch = 
    try
      let e = data ch in (*reads input until newline*)
      e :: buf_function ch (*recursively reads input until EOF*)
    with
    End_of_file -> []
            
  let in_function filename =
    let ch = open_in filename in(*invokes input buffer *)
    let ls = buf_function ch in (*intinialize function*)
    close_in ch;(*close input buffer*)
    ls (*return function output*)
  end 
module Out = struct
  (**key-datatype defintion*)
  type t = string
  type 'a d = 'a list

  (**key data type function*)
  let data ch v = (*ch = open_out filename*)
  output_string ch v; (*prints string to stdout*)
  output_char ch '\n' (*prints new line*)

  (**defines operation on d [M.op (data ch)] d*)
  let buf_function ch d =
    List.iter (data ch) d 

  (**executes operation given a file access type and compound data type*)
  let out_function filename dict =
    let ch = open_out filename in(*invokes outchannel via string*)
    buf_function ch dict;(*runs iterative list on input data*)
    close_out ch(**)
  end 
module Copy (M1:Input) (M2: Output with type t = M1.t and type 'a d = 'a M1.d) = struct 
  let copy file1 file2 = M2.out_function file2 (M1.in_function file1) 
end
module Listcopy = Copy (In) (Out)

(*avec formatted i/o libraries*)
module type G_I = sig end
module type G_O = sig end

module G_In =struct end 
module G_Out=struct end 

let copylist x = let open Listcopy in copy x;;
(*concurrency*)

(*Browser Based*)
exception Not_a_library
exception Not_a_module
(*A much larger browser*)
let learn_ocaml = 
  "Utop 
    main :
    grader-plugins
    grader
    ppx-meatquot 
    repo
    server
    utils
    state
    toplevel
    toploop"
(*desktop based*)
let _ = 
  "Utop 
    uTop_main
    uTop_complete
    uTop_styles
    uTop
    uTop_lexer
    uTop_compat
    uTop_private
    uTop_history 
    uTop_token"

 let uTop_token = "uTop_token is a brief 60 line implementation of 
                   the valid types for the location and input monoid"
 let uTop_history = "uTop_history collection of functions to handle 
                     the input history"
 let uTop_compat = "uTop_compat manages compatability with various 
                    versions of ocaml & unix environments"
 let uTop_private = "uTop_private manages collection of functions 
                     defining interaction with user directories and 
                     file"
 let uTop_lexer = "turns client-given input into a collection of tokens"
 let uTop = "acts as driver for compat, history, and private"
 let uTop_styles = "stylizes buffer depending on utop, utop_private, and the tokens"
 let uTop_complete = "A thousand line module implementing autocompletion"
 let uTop_main = "defines driver" 

 let _ = "modules outside Stdlib: Lterm "

(*Proof Assistants*)
let _ = "divided into directories of modules with topbin being driver directory"
let whytheorem_provers : unit = 
  let one = "" in 
  let two = "" in 
  let three = "Unification of mathematical concepts across centuries" in 
  let four = "" in 
  let five = "" in 
    print_string (one^" "^two^" "^three^" "^four^" "^five) 
(*rocq*)
(*hol light*)

(*Metalanguages*)
let _ = "Meta-OCaml?"

(*Concurrency*)

(*Systems Programming*)


(*Web Development*)

let _ = "What is js_of_ocaml and how it used"



(*how the application of an app proportions its architecture*)
let _ = "many ocaml applications 
         utilize some combination
         of the above"

(*COMPILERS*)
(*lexer*)
(*parser*)
(*parsing actions*)
(*semantic analysis*)
(*translate*)
(*canonicalize*)
(*Instruction selection*)
(*Control flow analysis*)
(*data flow analysis*)
(*register allocation*)
(*code emission*)
(*assembler*)
(*linker*)

(*https://ocamlverse.net/content/compilers.html*)
(*js_ofocaml*)
(*compcert*)
(*eff*)
(*stanc3*)


(*GUI/Graphics*)
let _ = "important considerations for graphics"
let _ = "images are not functions"
let _ = "managing overhead costs"

(*Hybrid language environments*)
let _ = "gnu make"
let _ = "C programming is an important foundation
         for implementing the backend, runtime and
         other language features"

let _ = "js"

let _ = "html/css"

let _ = "python"

