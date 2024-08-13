open Core
open Jsonaf
open Expect_test_helpers_core

module Status = struct
  type t = bool [@@deriving compare, sexp_of]

  include Jsonafable.Of_stringable (Bool)
end

module Wrapped_status = struct
  type t = { status : Status.t } [@@deriving compare, sexp_of]

  include
    Jsonafable.Of_jsonafable
      (Status)
      (struct
        type nonrec t = t

        let to_jsonafable t = t.status
        let of_jsonafable jsonable = { status = jsonable }
      end)
end

let%expect_test "round trip" =
  let open struct
    module type S = sig
      type t [@@deriving compare, jsonaf, sexp_of]
    end
  end in
  let test (type a) (module M : S with type t = a) expected =
    let jsonaf = M.jsonaf_of_t expected in
    let observed = M.t_of_jsonaf jsonaf in
    require_compare_equal (module M) observed expected;
    print_endline (Jsonaf.to_string jsonaf)
  in
  test (module Status) true;
  [%expect {| "true" |}];
  test (module Wrapped_status) { status = true };
  [%expect {| "true" |}]
;;

let%expect_test "error" =
  let test jsonaf =
    require_does_raise (fun () -> ignore (Status.t_of_jsonaf jsonaf : Status.t))
  in
  test (`String "maybe");
  [%expect
    {|
    (Of_jsonaf_error
      (Invalid_argument "Bool.of_string: expected true or false but got maybe")
      (String maybe))
    |}];
  test `Null;
  [%expect {| (Of_jsonaf_error "string expected" Null) |}]
;;
