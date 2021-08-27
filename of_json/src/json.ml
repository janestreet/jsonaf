open Core

type t =
  [ `Null
  | `True
  | `False
  | `String of string
  | `Number of string
  | `Object of (string * t) list
  | `Array of t list
  ]
  constraint t = Jsonaf.t
[@@deriving sexp]

let of_string s =
  match Jsonaf.parse s with
  | Ok json -> json
  | Error error -> raise_s [%message "Json.of_string: parse error" (error : Error.t)]
;;

let to_string t = Jsonaf.to_string t

let typeof = function
  | `Object _ -> "object"
  | `True | `False -> "bool"
  | `Number _ -> "number"
  | `Array _ -> "array"
  | `Null -> "null"
  | `String _ -> "string"
;;

let type_error json ~expected =
  let got = typeof json in
  raise_s [%message "JSON type error" (expected : string) (got : string)]
;;

let as_assoc = function
  | `Object assoc -> assoc
  | json -> raise_s (type_error json ~expected:"object")
;;

let as_number = function
  | `Number number -> number
  | json -> raise_s (type_error json ~expected:"number")
;;

let as_int json =
  let number = as_number json in
  try Int.of_string number with
  | exn -> raise_s [%message "Json.as_int: unable to convert number to int" (exn : exn)]
;;

let as_float json =
  let number = as_number json in
  try Float.of_string number with
  | exn ->
    raise_s [%message "Json.as_float: unable to convert number to float" (exn : exn)]
;;

let as_string = function
  | `String string -> string
  | json -> raise_s (type_error json ~expected:"string")
;;

let as_bool = function
  | `True -> true
  | `False -> false
  | json -> raise_s (type_error json ~expected:"bool")
;;

let as_list = function
  | `Array list -> list
  | json -> raise_s (type_error json ~expected:"array")
;;

let as_option = function
  | `Null -> None
  | x -> Some x
;;

let keys json = as_assoc json |> List.map ~f:fst

let member name = function
  | `Object assoc -> List.Assoc.find assoc name ~equal:String.equal
  | json -> raise_s (type_error json ~expected:"object")
;;
