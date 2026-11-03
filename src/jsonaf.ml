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
[@@deriving sexp, equal, compare, globalize]

let mode_cross = Jsonaf_kernel.mode_cross
let jsonaf_of_t t = t
let t_of_jsonaf t = t
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

module With_structural_compare = struct
  type nonrec t = t [@@deriving sexp, equal, compare, globalize, string]

  include Comparable.Make (struct
      type nonrec t = t [@@deriving sexp, compare]
    end)

  let jsonaf_of_t t = t
  let t_of_jsonaf t = t
end

let exactly_equal = With_structural_compare.equal

module Jsonafable = Jsonafable
module Parser = Jsonaf_kernel.Parser
module Serializer = Jsonaf_kernel.Serializer

include Pretty_printer.Register (struct
    type nonrec t = t

    let module_name = "Jsonaf"
    let to_string = to_string_hum
  end)

module Or_raw = struct
  type t =
    [ `Raw_json_string of string
    | `Null
    | `False
    | `True
    | `String of string
    | `Number of string
    | `Object of (string * t) list
    | `Array of t list
    ]
    constraint t = (string, t) Jsonaf_kernel.Expert.Or_raw.t

  let serialize t faraday =
    Jsonaf_kernel.Expert.Serializer.create
      (fun f num -> Faraday.write_string f num)
      t
      faraday
  ;;

  let to_string (t : t) =
    let faraday = Faraday.create 0x1000 in
    serialize t faraday;
    Faraday.serialize_to_string faraday
  ;;
end

let maybe_globalize_t t = t
let maybe_globalize_string str = str

module Or_null_shim = struct
  let index index = function
    | `Array xs -> List.nth_or_null xs index
    | _ -> Null
  ;;

  let member key json : t or_null =
    match json with
    | `Object xs -> List.Assoc.find_or_null ~equal:String.equal xs key
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

  let string (json : t) : string or_null =
    match json with
    | `String i -> This i
    | _ -> Null
  ;;

  let list (json : t) : t list or_null =
    match json with
    | `Array xs -> This xs
    | _ -> Null
  ;;

  let assoc_list (json : t) : (string * t) list or_null =
    match json with
    | `Object xs -> This xs
    | _ -> Null
  ;;
end

module Conv = Jsonaf_kernel.Conv
module Export = Conv.Primitives

let () =
  let module Sexp_conv = Sexplib0.Sexp_conv in
  Sexp_conv.Exn_converter.add [%extension_constructor Conv.Of_jsonaf_error] (function
    | Conv.Of_jsonaf_error (v0, v1) ->
      let v0 =
        match v0 with
        | Failure v0 -> Sexplib0.Sexp.Atom v0
        | v0 -> Sexp_conv.sexp_of_exn v0
      and v1 = sexp_of_t (mode_cross v1) in
      Sexplib0.Sexp.(List [ Atom "Of_jsonaf_error"; v0; v1 ])
    | _ -> assert false)
;;

let index index json = Or_null_shim.index index json |> Or_null.to_option

let index_exn index json =
  match json with
  | `Array xs as json ->
    (match List.nth_or_null xs index with
     | This x -> x
     | Null ->
       let json = maybe_globalize_t json in
       raise_s [%message "Jsonaf.index: index out of ranage" (index : int) (json : t)])
  | _ as json ->
    let json = maybe_globalize_t json in
    raise_s [%message "Jsonaf.index: json is not an array" (index : int) (json : t)]
;;

let member key json : t option = Or_null_shim.member key json |> Or_null.to_option

let member_exn key json =
  match json with
  | `Object xs ->
    (match List.Assoc.find_or_null ~equal:String.equal xs key with
     | This x -> x
     | Null ->
       let json = maybe_globalize_t json in
       let key = maybe_globalize_string key in
       raise_s
         [%message "Jsonaf.member: key is not in the object" (key : string) (json : t)])
  | json ->
    let json = maybe_globalize_t json in
    let key = maybe_globalize_string key in
    raise_s [%message "Jsonaf.member: json is not an object" (key : string) (json : t)]
;;

let member_or_null key json =
  match Or_null_shim.member key json with
  | This x -> x
  | Null -> `Null
;;

let bool json = Or_null_shim.bool json |> Or_null.to_option

let bool_exn json =
  match json with
  | `True -> true
  | `False -> false
  | json ->
    let json = maybe_globalize_t json in
    raise_s [%message "Jsonaf.bool_exn: not a bool" (json : t)]
;;

let int json = Or_null_shim.int json |> Or_null.to_option

let int_exn json =
  match json with
  | `Number number -> Int.of_string number
  | json ->
    let json = maybe_globalize_t json in
    raise_s [%message "Jsonaf.int_exn: not an int" (json : t)]
;;

let float json = Or_null_shim.float json |> Or_null.to_option

let float_exn json =
  match json with
  | `Number number -> Float.of_string number
  | json ->
    let json = maybe_globalize_t json in
    raise_s [%message "Jsonaf.float_exn: not a float" (json : t)]
;;

let string json = Or_null_shim.string json |> Or_null.to_option

let string_exn json =
  match json with
  | `String i -> i
  | json ->
    let json = maybe_globalize_t json in
    raise_s [%message "Jsonaf.string_exn: not a string" (json : t)]
;;

let list json = Or_null_shim.list json |> Or_null.to_option

let list_exn json =
  match json with
  | `Array xs -> xs
  | json ->
    let json = maybe_globalize_t json in
    raise_s [%message "Jsonaf.list_exn: not a list" (json : t)]
;;

let assoc_list json = Or_null_shim.assoc_list json |> Or_null.to_option

let assoc_list_exn json =
  match json with
  | `Object xs -> xs
  | json ->
    let json = maybe_globalize_t json in
    raise_s [%message "Jsonaf.assoc_list_exn: not an assoc_list" (json : t)]
;;

let keys (json : t) : string list option =
  match json with
  | `Object xs -> Some (List.map xs ~f:fst)
  | _ -> None
;;

let keys_exn json : string list =
  match json with
  | `Object xs -> List.map xs ~f:fst
  | json ->
    let json = maybe_globalize_t json in
    raise_s [%message "Jsonaf.keys_exn: not an assoc_list" (json : t)]
;;

module Or_null = Or_null_shim
