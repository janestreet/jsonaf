open Core
open Of_json
open Expect_test_helpers_core

let%expect_test "nice exception printing" =
  let json =
    `Object
      [ "foo", `Object [ "bar", `String "baz" ]
      ; "whizz", `Array [ `String "zip"; `String "zang"; `String "ding"; `String "dong" ]
      ; "is_a_test", `True
      ]
  in
  (try ignore (("foo" @. "bar" @. int) json : int) with
   | Of_json_conv_failed err -> print_endline (Conv_failure.to_string_hum err)
   | _ -> assert false);
  [%expect
    {|
    ("JSON type error" (expected number) (got string))
    in json:

        "baz"


    at key [bar] in json:

        {
          "bar": "baz"
        }


    at key [foo] in json:

        {
          "foo": {
            "bar": "baz"
          },
          "whizz": [
            "zip",
            "zang",
            "ding",
            "dong"
          ],
          "is_a_test": true
        }
  |}]
;;

let%expect_test "errors for missing keys" =
  let json = `Object [ "foo", `Object [ "bar", `String "baz" ] ] in
  require_does_raise [%here] (fun () -> ("foo" @. "baz" @. string) json);
  [%expect
    {|
    (helpers.ml.Of_json_conv_failed (
      "Of_json failed to convert"
      ("Key not in object" (key baz))
      ("json context [1]" (Object ((bar (String baz)))))
      ("json context [0], at key [foo]" (
        Object ((foo (Object ((bar (String baz)))))))))) |}]
;;

let%expect_test "errors for bad types" =
  let json = `Object [ "foo", `Object [ "bar", `String "baz" ] ] in
  require_does_raise [%here] (fun () -> ("foo" @. "bar" @. int) json);
  [%expect
    {|
    (helpers.ml.Of_json_conv_failed (
      "Of_json failed to convert"
      ("JSON type error"
        (expected number)
        (got      string))
      ("json context [2]" (String baz))
      ("json context [1], at key [bar]" (Object ((bar (String baz)))))
      ("json context [0], at key [foo]" (
        Object ((foo (Object ((bar (String baz)))))))))) |}]
;;

let%expect_test "errors in mapping functions" =
  let json = `Object [ "foo", `Object [ "bar", `String "baz" ] ] in
  require_does_raise [%here] (fun () -> ("foo" @. "bar" @. string @> Int.of_string) json);
  [%expect
    {|
    (helpers.ml.Of_json_conv_failed (
      "Of_json failed to convert"
      (Failure "Int.of_string: \"baz\"")
      ("json context [2]" (String baz))
      ("json context [1], at key [bar]" (Object ((bar (String baz)))))
      ("json context [0], at key [foo]" (
        Object ((foo (Object ((bar (String baz)))))))))) |}]
;;

let%expect_test "safe" =
  print_s [%sexp (safe int (`String "foo") : int option)];
  [%expect {| () |}];
  print_s [%sexp (safe int (`Number "11") : int option)];
  [%expect {| (11) |}]
;;

let%expect_test "<|>" =
  let of_json = int @> Int.to_string <|> bool @> Bool.to_string <|> string in
  print_s [%sexp (of_json (`String "foo") : string)];
  [%expect {| foo |}]
;;

let%expect_test "choice" =
  let of_json = choice [ int @> Int.to_string; bool @> Bool.to_string; string ] in
  print_s [%sexp (of_json (`String "foo") : string)];
  [%expect {| foo |}]
;;

let%expect_test "errors with alternatives" =
  let of_json =
    int @> Int.to_string
           <|> bool @> Bool.to_string
                       <|> list string @> String.concat ~sep:" "
  in
  show_raise (fun () -> of_json (`String "foo"));
  [%expect
    {|
    (raised (
      helpers.ml.Alternative_error (
        "expected one non-failure"
        ("branch [0]" (
          "JSON type error"
          (expected number)
          (got      string)))
        ("branch [1]" (
          "JSON type error"
          (expected bool)
          (got      string)))
        ("branch [2]" (
          "JSON type error"
          (expected array)
          (got      string)))
        ("branch context" (String foo))))) |}]
;;

let%expect_test "errors in [choice]" =
  (* We would expect this to be the same as the above test chaining [<|>] together *)
  let of_json =
    choice
      [ int @> Int.to_string
      ; bool @> Bool.to_string
      ; list string @> String.concat ~sep:" "
      ]
  in
  show_raise (fun () -> of_json (`String "foo"));
  [%expect
    {|
    (raised (
      helpers.ml.Alternative_error (
        "expected one non-failure"
        ("branch [0]" (
          "JSON type error"
          (expected number)
          (got      string)))
        ("branch [1]" (
          "JSON type error"
          (expected bool)
          (got      string)))
        ("branch [2]" (
          "JSON type error"
          (expected array)
          (got      string)))
        ("branch context" (String foo))))) |}]
;;

let%expect_test "alternative errors in nested json provide correct context" =
  let int_or_bool = int @> Int.to_string <|> bool @> Bool.to_string in
  let of_json = "root" @. ("bar" @. int_or_bool <|> "foo" @. int_or_bool) in
  show_raise (fun () -> of_json (`Object [ "root", `Object [ "foo", `String "foo" ] ]));
  [%expect
    {|
    (raised (
      helpers.ml.Of_json_conv_failed (
        "Of_json failed to convert"
        (helpers.ml.Alternative_error (
          "expected one non-failure"
          ("branch [0]" ("Key not in object" (key bar)))
          ("branch [1]" (
            helpers.ml.Alternative_error (
              "expected one non-failure"
              ("branch [0]" (
                "JSON type error"
                (expected number)
                (got      string)))
              ("branch [1]" (
                "JSON type error"
                (expected bool)
                (got      string)))
              ("branch context" (String foo)))))
          ("branch context" (Object ((foo (String foo)))))))
        ("json context [0], at key [root]" (
          Object ((root (Object ((foo (String foo))))))))))) |}]
;;

let%expect_test "tuple" =
  let of_json =
    tuple
      Array_as_tuple.(
        [%map
          let a = shift @@ string
          and b = shift @@ number @> Int.of_string
          and c = shift @@ list bool in
          a, b, c])
  in
  let valid = [ `String "foo"; `Number "10"; `Array [ `True; `False ] ] in
  (* Not a list *)
  show_raise (fun () -> of_json (`String "foo"));
  [%expect
    {|
    (raised (
      helpers.ml.Of_json_conv_failed (
        "Of_json failed to convert"
        ("JSON type error"
          (expected array)
          (got      string))
        ("json context [0]" (String foo)))))
  |}];
  (* Not enough elements *)
  show_raise (fun () -> of_json (`Array (List.take valid 2)));
  [%expect
    {|
    (raised (
      helpers.ml.Of_json_conv_failed (
        "Of_json failed to convert"
        "ran out of elements while parsing tuple"
        ("json context [0]" (
          Array (
            (String foo)
            (Number 10)))))))
  |}];
  (* Too many elements *)
  show_raise (fun () -> of_json (`Array (valid @ [ `String "extra" ])));
  [%expect
    {|
    (raised (
      helpers.ml.Of_json_conv_failed (
        "Of_json failed to convert"
        ("array_as_tuple has unparsed elements" (elems ((String extra))))
        ("json context [0]" (
          Array (
            (String foo)
            (Number 10)
            (Array (True False))
            (String extra)))))))
  |}];
  print_s [%sexp (of_json (`Array valid) : string * int * bool list)];
  [%expect {| (foo 10 (true false)) |}]
;;

let%expect_test "floats to string" =
  let test_float_to_string f =
    let float = To_json.float f in
    print_s [%sexp (float : Json.t)]
  in
  test_float_to_string 0.;
  [%expect {| (Number 0) |}];
  test_float_to_string 1.34;
  [%expect {| (Number 1.34) |}]
;;
