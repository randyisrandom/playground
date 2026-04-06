(*QCheck, QCheck-Core, QCheck-OUnit2
  https://github.com/c-cube
  cmd .opam/default/lib/qcheck-ounit/QCheck_ounit.ml
*)
open OUnit2
open Is_even

let t = QCheck.Test.make (QCheck.make QCheck.Gen.int) is_even 

(*QCheck_runner.run_tests [t]*)

let unit_test = QCheck_ounit.to_ounit2_test t 

let _ = run_test_tt_main unit_test
