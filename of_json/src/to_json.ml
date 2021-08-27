open! Core

type 'a t = 'a -> Json.t


let bool b = if b then `True else `False

let float f =
  (* The default OCaml [Float.to_string] will serialize 1. as "1.", which is not valid
     JSON, so we simply try to strip it off. *)
  `Number (Float.to_string f |> String.rstrip ~drop:(Char.equal '.'))
;;

let int i = `Number (Int.to_string i)
let to_json_list to_json xs = `Array (List.map xs ~f:to_json)
