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

module Util = struct
  let index_exn index json =
    match json with
    | `Array xs as json ->
      (match List.nth xs index with
       | Some x -> x
       | None ->
         raise_s [%message "Jsonaf.index: index out of ranage" (index : int) (json : t)])
    | _ as json ->
      raise_s [%message "Jsonaf.index: json is not an array" (index : int) (json : t)]
  ;;

  let index index = function
    | `Array xs -> List.nth xs index
    | _ -> None
  ;;

  let member_exn key json =
    match json with
    | `Object xs ->
      (match List.Assoc.find ~equal:String.equal xs key with
       | Some x -> x
       | None ->
         raise_s
           [%message "Jsonaf.member: key is not in the object" (key : string) (json : t)])
    | json ->
      raise_s [%message "Jsonaf.member: json is not an object" (key : string) (json : t)]
  ;;

  let member key json =
    match json with
    | `Object xs -> List.Assoc.find ~equal:String.equal xs key
    | _ -> None
  ;;

  let member_or_null key json =
    match member key json with
    | Some x -> x
    | None -> `Null
  ;;

  let bool json =
    match json with
    | `True -> Some true
    | `False -> Some false
    | _ -> None
  ;;

  let bool_exn json =
    match json with
    | `True -> true
    | `False -> false
    | json -> raise_s [%message "Jsonaf.bool_exn: not a bool" (json : t)]
  ;;

  let int json =
    match json with
    | `Number number ->
      (try Some (Int.of_string number) with
       | _ -> None)
    | _ -> None
  ;;

  let int_exn json =
    match json with
    | `Number number -> Int.of_string number
    | json -> raise_s [%message "Jsonaf.int_exn: not an int" (json : t)]
  ;;

  let float json =
    match json with
    | `Number number ->
      (try Some (Float.of_string number) with
       | _ -> None)
    | _ -> None
  ;;

  let float_exn json =
    match json with
    | `Number number -> Float.of_string number
    | json -> raise_s [%message "Jsonaf.float_exn: not a float" (json : t)]
  ;;

  let string json =
    match json with
    | `String i -> Some i
    | _ -> None
  ;;

  let string_exn json =
    match json with
    | `String i -> i
    | json -> raise_s [%message "Jsonaf.string_exn: not a string" (json : t)]
  ;;

  let list json =
    match json with
    | `Array xs -> Some xs
    | _ -> None
  ;;

  let list_exn json =
    match json with
    | `Array xs -> xs
    | json -> raise_s [%message "Jsonaf.list_exn: not a list" (json : t)]
  ;;

  let assoc_list json =
    match json with
    | `Object xs -> Some xs
    | _ -> None
  ;;

  let assoc_list_exn json =
    match json with
    | `Object xs -> xs
    | json -> raise_s [%message "Jsonaf.assoc_list_exn: not an assoc_list" (json : t)]
  ;;

  let keys json =
    match json with
    | `Object xs -> Some (List.map xs ~f:fst)
    | _ -> None
  ;;

  let keys_exn json =
    match json with
    | `Object xs -> List.map xs ~f:fst
    | json -> raise_s [%message "Jsonaf.keys_exn: not an assoc_list" (json : t)]
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
      and v1 = sexp_of_t v1 in
      Sexplib0.Sexp.(List [ Atom "Of_jsonaf_error"; v0; v1 ])
    | _ -> assert false)
;;

include Util
