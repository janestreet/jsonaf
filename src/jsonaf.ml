open Base

type t =
  [ `Null
  | `False
  | `True
  | `String of string
  | `Number of string
  | `Object of (string * t) list
  | `Array of t list
  ]
  constraint t = Jsonaf_kernel.t
[@@deriving sexp, equal ~localize, globalize]

let mode_cross = Jsonaf_kernel.mode_cross
let jsonaf_of_t t = t
let t_of_jsonaf t = t
let exactly_equal = equal
let to_string t = Jsonaf_kernel.Serializer.run t
let to_string_hum t = Jsonaf_kernel.Serializer.run_hum ~spaces:2 t
let parse input = Jsonaf_kernel.Parser.run input |> Result.map_error ~f:Error.of_string

let parse_many input =
  Jsonaf_kernel.Parser.run_many input |> Result.map_error ~f:Error.of_string
;;

let of_string input =
  match parse input with
  | Ok t -> t
  | Error error ->
    raise_s [%message "Jsonaf.of_string: parse error" (error : Error.t) (input : string)]
;;

module Jsonafable = Jsonafable
module Parser = Jsonaf_kernel.Parser
module Serializer = Jsonaf_kernel.Serializer

include Pretty_printer.Register (struct
    type nonrec t = t

    let module_name = "Jsonaf"
    let to_string = to_string_hum
  end)

[%%template
let maybe_globalize_t t = t [@@mode __ = global]
let maybe_globalize_t (t @ l) = globalize t [@@mode l = local]
let maybe_globalize_string str = str [@@mode __ = global]
let maybe_globalize_string (str @ l) = String.globalize str [@@mode l = local]]

module%template Or_null_shim = struct
  [@@@mode.default l = (local, global)]

  let index index = function
    | `Array xs -> (List.nth_or_null [@mode l]) xs index [@exclave_if_local l]
    | _ -> Null
  ;;

  let member (key @ l) (json @ l) : t or_null @ l =
    match json with
    | `Object xs ->
      (List.Assoc.find_or_null [@mode l])
        ~equal:(String.equal [@mode l])
        xs
        key [@exclave_if_local l]
    | _ -> Null
  ;;

  let bool (json : t) =
    match json with
    | `True -> This true
    | `False -> This false
    | _ -> Null
  ;;

  let int (json : t) =
    match json with
    | `Number number ->
      (try This (Int.of_string number) with
       | _ -> Null)
    | _ -> Null
  ;;

  let float (json : t) =
    match json with
    | `Number number ->
      (try This (Float.of_string number) with
       | _ -> Null)
    | _ -> Null
  ;;

  let string (json : t) : string or_null @ l =
    match json with
    | `String i -> This i
    | _ -> Null
  ;;

  let list (json : t) : t list or_null @ l =
    match json with
    | `Array xs -> This xs
    | _ -> Null
  ;;

  let assoc_list (json : t) : (string * t) list or_null @ l =
    match json with
    | `Object xs -> This xs
    | _ -> Null
  ;;
end

module Jsonaf_conv = Jsonaf_kernel.Conv
module Export = Jsonaf_conv.Primitives

let () =
  let module Sexp_conv = Sexplib0.Sexp_conv in
  Sexp_conv.Exn_converter.add
    [%extension_constructor Jsonaf_conv.Of_jsonaf_error]
    (function
    | Jsonaf_conv.Of_jsonaf_error (v0, v1) ->
      let v0 =
        match v0 with
        | Failure v0 -> Sexplib0.Sexp.Atom v0
        | v0 -> Sexp_conv.sexp_of_exn v0
      and v1 = sexp_of_t (mode_cross v1) in
      Sexplib0.Sexp.(List [ Atom "Of_jsonaf_error"; v0; v1 ])
    | _ -> assert false)
;;

[%%template
[@@@mode.default l = (local, global)]

let index index (json @ l) =
  ((Or_null_shim.index [@mode l]) index json |> (Or_null.to_option [@mode l]))
  [@exclave_if_local l ~reasons:[ May_return_regional; Will_return_unboxed ]]
;;

let index_exn index (json @ l) =
  match[@exclave_if_local l ~reasons:[ May_return_regional ]] json with
  | `Array xs as json ->
    (match (List.nth_or_null [@mode l]) xs index with
     | This x -> x
     | Null ->
       let json = (maybe_globalize_t [@mode l]) json in
       raise_s [%message "Jsonaf.index: index out of ranage" (index : int) (json : t)])
  | _ as json ->
    let json = (maybe_globalize_t [@mode l]) json in
    raise_s [%message "Jsonaf.index: json is not an array" (index : int) (json : t)]
;;

let member (key @ l) (json @ l) : t option @ l =
  ((Or_null_shim.member [@mode l]) key json |> (Or_null.to_option [@mode l]))
  [@exclave_if_local l ~reasons:[ May_return_regional; Will_return_unboxed ]]
;;

let member_exn (key @ l) (json @ l) =
  match[@exclave_if_local l ~reasons:[ May_return_regional ]] json with
  | `Object xs ->
    (match (List.Assoc.find_or_null [@mode l]) ~equal:(String.equal [@mode l]) xs key with
     | This x -> x
     | Null ->
       let json = (maybe_globalize_t [@mode l]) json in
       let key = (maybe_globalize_string [@mode l]) key in
       raise_s
         [%message "Jsonaf.member: key is not in the object" (key : string) (json : t)])
  | json ->
    let json = (maybe_globalize_t [@mode l]) json in
    let key = (maybe_globalize_string [@mode l]) key in
    raise_s [%message "Jsonaf.member: json is not an object" (key : string) (json : t)]
;;

let member_or_null key (json @ l) =
  match[@exclave_if_local l ~reasons:[ May_return_regional; Will_return_unboxed ]]
    (Or_null_shim.member [@mode l]) key json
  with
  | This x -> x
  | Null -> `Null
;;

let bool (json @ l) =
  ((Or_null_shim.bool [@mode l]) json |> (Or_null.to_option [@mode l]))
  [@exclave_if_local l ~reasons:[ May_return_regional; Will_return_unboxed ]]
;;

let bool_exn json =
  match json with
  | `True -> true
  | `False -> false
  | json ->
    let json = (maybe_globalize_t [@mode l]) json in
    raise_s [%message "Jsonaf.bool_exn: not a bool" (json : t)]
;;

let int (json @ l) =
  ((Or_null_shim.int [@mode l]) json |> (Or_null.to_option [@mode l]))
  [@exclave_if_local l ~reasons:[ May_return_regional; Will_return_unboxed ]]
;;

let int_exn json =
  match json with
  | `Number number -> Int.of_string number
  | json ->
    let json = (maybe_globalize_t [@mode l]) json in
    raise_s [%message "Jsonaf.int_exn: not an int" (json : t)]
;;

let float (json @ l) =
  ((Or_null_shim.float [@mode l]) json |> (Or_null.to_option [@mode l]))
  [@exclave_if_local l ~reasons:[ May_return_regional; Will_return_unboxed ]]
;;

let float_exn json =
  match json with
  | `Number number -> Float.of_string number
  | json ->
    let json = (maybe_globalize_t [@mode l]) json in
    raise_s
      [%message "Jsonaf.float_exn: not a float" (json : t)]
    [@exclave_if_local l ~reasons:[ May_return_regional; Will_return_unboxed ]]
;;

let string (json @ l) =
  ((Or_null_shim.string [@mode l]) json |> (Or_null.to_option [@mode l]))
  [@exclave_if_local l ~reasons:[ May_return_regional; Will_return_unboxed ]]
;;

let string_exn json =
  match json with
  | `String i -> i
  | json ->
    let json = (maybe_globalize_t [@mode l]) json in
    raise_s [%message "Jsonaf.string_exn: not a string" (json : t)]
;;

let list (json @ l) =
  ((Or_null_shim.list [@mode l]) json |> (Or_null.to_option [@mode l]))
  [@exclave_if_local l ~reasons:[ May_return_regional; Will_return_unboxed ]]
;;

let list_exn json =
  match json with
  | `Array xs -> xs
  | json ->
    let json = (maybe_globalize_t [@mode l]) json in
    raise_s [%message "Jsonaf.list_exn: not a list" (json : t)]
;;

let assoc_list (json @ l) =
  ((Or_null_shim.assoc_list [@mode l]) json |> (Or_null.to_option [@mode l]))
  [@exclave_if_local l ~reasons:[ May_return_regional; Will_return_unboxed ]]
;;

let assoc_list_exn json =
  match json with
  | `Object xs -> xs
  | json ->
    let json = (maybe_globalize_t [@mode l]) json in
    raise_s [%message "Jsonaf.assoc_list_exn: not an assoc_list" (json : t)]
;;]

[%%template
let keys (json : t) : string list option @ l =
  match json with
  | `Object xs -> Some ((List.map [@mode l] [@alloc a]) xs ~f:fst) [@exclave_if_stack a]
  | _ -> None
[@@alloc a @ l = (heap_global, stack_local)]
;;

let keys_exn (json @ l) : string list @ l =
  match json with
  | `Object xs -> (List.map [@mode l] [@alloc a]) xs ~f:fst [@exclave_if_stack a]
  | json ->
    let json = (maybe_globalize_t [@mode l]) json in
    raise_s [%message "Jsonaf.keys_exn: not an assoc_list" (json : t)]
[@@alloc a @ l = (heap_global, stack_local)]
;;]

module Or_null = Or_null_shim
