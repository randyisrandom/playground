open OUnit2
open Product

let tests = "product test" >::: [
  "empty" >:: (fun _ -> assert_equal 1 (product []));
  "singleton" >:: (fun _ -> assert_equal 8 (product [8]));
  "two_elements" >:: (fun _ -> assert_equal 12 (product [3;4]));
]

let _ = run_test_tt_main tests