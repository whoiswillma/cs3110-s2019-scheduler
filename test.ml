open OUnit2

let suite = "course maker test suite" >::: List.flatten [
    Coursedata_test.tests;
    Makeschedule_test.tests;
  ]
let _ = run_test_tt_main suite