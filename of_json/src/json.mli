open! Core

type t = Jsonaf.t

include Sexpable.S with type t := t
include Stringable.S with type t := t

val typeof : t -> string
val as_assoc : t -> (string, t) List.Assoc.t
val as_number : t -> string
val as_int : t -> int
val as_float : t -> float
val as_string : t -> string
val as_bool : t -> bool
val as_list : t -> t list
val as_option : t -> t option
val keys : t -> string list
val member : string -> t -> t option
