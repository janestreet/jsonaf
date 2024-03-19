open! Jsonaf
open! Expect_test_helpers_core

let%expect_test "parse error contains input" =
  show_raise (fun () -> ignore (of_string "{a: \001}"));
  [%expect
    {|
    (raised (
      "Jsonaf.of_string: parse error"
      (error "json > object: char '}'")
      (input "{a: \001}")))
    |}]
;;
