(********************************************************************)
(*                        APPLICATION SURVEY                        *)
(*                                                                  *)
(*                                                                  *)
(********************************************************************)
(*# REVIEW #*)
(*Syntax & Semantics*)
(*CoreML*)

(*Modules*)
let _ = "the sugar";;(*why the delimiter?*)
(*Objects*)

(*Classes*)

(*Idioms*)

fun () -> "anonymus toplevel function execution unit -> 'a "

let f () = "labeled execution unit f: unit -> string"

let _ = "Objects and Classes"
let _ = "minimizing their use and understanding 
         the unique design constraints they satisfy" 
(*Tools*)
let _ = "The means of building applications"

let _ = "Traditional gnu_make"
(*dune*)
let _ = "uses S-expressions"
(*opam*)
let _ = "structured and functions simlarly to a debian
         directory, controls language environment the
         build systems execute in"

(*libraries*)
let _ = "means of navigating and exploring"

let _ = "using the repl"

let _ = "using testing libraries for
         larger codebases"

let _ = "using scripts to explore dependecies 
         and other information, that can be 
         explored in terms of regex's and globs"
(*Stdlib*)
let _ = "These modules are used to define 
         common data types and operations 
         on them"
module _ = Stdlib 

module _ = Bool
module _ = Char
module _ = Float
module _ = Int 
module _ = Int32
module _ = Int64
module _ = String 
module _ = StringLabels
module _ = Unit 

module _ = Array
module _ = Bytes
module _ = Bigarray
module _ = Buffer
module _ = Dynarray
module _ = Either
module _ = Fun
module _ = List 
(*External iterator example*)
module _ = Seq
module _ = Option
module _ = Pair
module _ = Queue
module _ = Stack

let _ = "These modules are used to define 
         algorithms & data structures as 
         functors with client provided input"

module _ = Map
module _ = Set 
module _ = Hashtbl
module _ = Pqueue 
module _ = Weak
module _ = Ephemeron

let _ = "Computation and operations"
module _ = Gc
module _ = Lazy
module _ = Obj 
module _ = Oo 
module _ = Result
module _ = Type 


let _ = "formating & printing"
module _ = Format
module _ = Printexc 

let _ = "these modules control I/O 
         based functions & filename"
module _ = In_channel
module _ = Out_channel
module _ = Scanf
module _ = Printf
module _ = Marshal

let _ = "these modules are made for OS
         interaction"
module _ = Digest 
module _ = Unix
module _ = UnixLabels
module _ = Sys
module _ = Arg 

let _ = "these realize control structures"

module _ = Atomic
(*went over testing this at bobkonf?*)
module _ = Condition
module _ = Domain 
module _ = Dynlink
module _ = Effect
module _ = Event 
module _ = Mutex 
module _ = Thread 

let _ = "these provide various levels of 
         runtime support"
module CALLBACK = Callback
module LEXING = Lexing 
module PARSING = Parsing 
module RESULT = Result
module RUNTIME = Runtime_events
module FORMAT = CamlinternalFormat

module BASICS = CamlinternalFormatBasics
module INTERNALLAZY = CamlinternalLazy
module MOD = CamlinternalMod
module INTERNAOO = CamlinternalOO

(*Jane Street*)


