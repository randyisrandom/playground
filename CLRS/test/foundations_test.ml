open CLRS  
open Foundations
open Sorting_order_stat
open Data_structures 

(*define the tests*)

let test_function1 () = Alcotest.(check int array) "Equivalent" [1;2;3;4;5;6] (To_test.normal_progression 6)
let test_function2 () = Alcotest.(check output_type) "Equivalent" output (To_test.function_name input)
let test_function3 () = Alcotest.(check output_type) "Equivalent" output (To_test.function input)
let test_function4 () = Alcotest.(check output_type) "Equivalent" output (To_test.function input)

(* Provide required input to run the test*)
let () =
  Alcotest.run "Utils"
    [(* (string * 'b list ) list *)
      ( "First Message",
        [
          Alcotest.test_case "function1 behavior" `Quick test_function1;
          Alcotest.test_case "function2 behavior" `Quick test_function2;
        ] );
      ( "Second Message",
        [ Alcotest.test_case "function4 behavior" `Quick test_function3 ] );
      ( "list-concat",
        [ Alcotest.test_case "function3 behavior" `Slow test_function4 ] );
    ]

