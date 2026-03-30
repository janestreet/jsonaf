open struct
  (* These globalize functions are written here as we don’t depend on Core/Async/Basement *)
  external globalize_string : string -> string = "caml_obj_dup"

  let[@tail_mod_cons] rec globalize_list globalize_elt xs =
    match xs with
    | [] -> []
    | x :: xs -> globalize_elt x :: globalize_list globalize_elt xs
  ;;
end

type t =
  [ `Null
  | `False
  | `True
  | `String of string
  | `Number of string
  | `Object of (string * t) list
  | `Array of t list
  ]
  constraint t = string Expert.t
[@@deriving globalize]

external mode_cross : t -> t = "%identity"
