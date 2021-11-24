open! Base

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
[@@deriving sexp]

(** Note that we intentionally do not expose [compare] or [equal] functions for [t].
    Objects in JSON are considered unordered, so two different representations of [t]
    may be unequal using the derived equal but the same according to the JSON spec. *)

(** [exactly_equal] checks equality including exact key order within objects *)
val exactly_equal : t -> t -> bool

(** [parse s] parses a single JSON object from [s]. It is an error if [s] does not contain
    exactly one JSON object. See [parse_many]. *)
val parse : string -> t Or_error.t

(** [parse_many] parses zero or more JSON objects from [s].

    Caveats: [parse_many] succeeds only if all JSON objects parse, and its error messages
    may be significantly worse than those from [parse].

    To get the well-formed objects up to the syntax error, and then a good error message,
    consider piping through [jq -c], splitting on newlines, and then parsing each line
    with [parse]. But this is much slower than [run_many].
*)
val parse_many : string -> t list Or_error.t

include Stringable.S with type t := t

(** human-readable output, indenting all fields/array elements by two spaces. *)
val to_string_hum : t -> string

include Pretty_printer.S with type t := t
module Jsonafable = Jsonafable
include Jsonafable.S with type t := t

module Parser : sig
  val t : t Angstrom.t
  val run : string -> (t, string) Result.t
end

module Serializer : sig
  val serialize : t -> Faraday.t -> unit
  val run : t -> string
end

val index : int -> t -> t option
val index_exn : int -> t -> t
val member : string -> t -> t option
val member_exn : string -> t -> t
val bool : t -> bool option
val bool_exn : t -> bool

(** Same as [member], but returns [`Null] instead of [None] if the member isn't present.
    This function is useful when migrating from [Yojson], as it is equivalent to
    [Yojson.Safe.Util.member]. If writing new code using Jsonaf, you should probably avoid
    it. Consider using [Of_json] instead. *)
val member_or_null : string -> t -> t

(** If [t] is a json number but not parseable as a [float], [float t] returns [None].
    Similarly [int t] will return [None] if the number is not parseable as an [int]. *)

val int : t -> int option
val int_exn : t -> int
val float : t -> float option
val float_exn : t -> float
val string : t -> string option
val string_exn : t -> string
val list : t -> t list option
val list_exn : t -> t list

(** If [t] is an object, return the association list between keys and values. Otherwise,
    return [None]. O(1). *)
val assoc_list : t -> (string * t) list option

(** If [t] is an object, return the association list between keys and values. Otherwise,
    raise. O(1). *)
val assoc_list_exn : t -> (string * t) list

(** If [t] is an object, return the keys of that object. Otherwise,
    return [None]. O(n). *)
val keys : t -> string list option

(** If [t] is an object, return the keys of that object. Otherwise,
    raise. O(n). *)
val keys_exn : t -> string list

val jsonaf_of_unit : unit -> Jsonaf_kernel.t
val jsonaf_of_bool : bool -> Jsonaf_kernel.t
val jsonaf_of_string : string -> Jsonaf_kernel.t
val jsonaf_of_bytes : bytes -> Jsonaf_kernel.t
val jsonaf_of_char : char -> Jsonaf_kernel.t
val jsonaf_of_int : int -> Jsonaf_kernel.t
val jsonaf_of_float : float -> Jsonaf_kernel.t
val jsonaf_of_int32 : int32 -> Jsonaf_kernel.t
val jsonaf_of_int64 : int64 -> Jsonaf_kernel.t
val jsonaf_of_nativeint : nativeint -> Jsonaf_kernel.t
val jsonaf_of_ref : ('a -> Jsonaf_kernel.t) -> 'a ref -> Jsonaf_kernel.t
val jsonaf_of_lazy_t : ('a -> Jsonaf_kernel.t) -> 'a lazy_t -> Jsonaf_kernel.t
val jsonaf_of_option : ('a -> Jsonaf_kernel.t) -> 'a option -> Jsonaf_kernel.t
val jsonaf_of_list : ('a -> Jsonaf_kernel.t) -> 'a list -> Jsonaf_kernel.t
val jsonaf_of_array : ('a -> Jsonaf_kernel.t) -> 'a array -> Jsonaf_kernel.t

val jsonaf_of_hashtbl
  :  ('a -> Jsonaf_kernel.t)
  -> ('b -> Jsonaf_kernel.t)
  -> ('a, 'b) Caml.Hashtbl.t
  -> Jsonaf_kernel.t

val unit_of_jsonaf : Jsonaf_kernel.t -> unit
val bool_of_jsonaf : Jsonaf_kernel.t -> bool
val string_of_jsonaf : Jsonaf_kernel.t -> string
val bytes_of_jsonaf : Jsonaf_kernel.t -> bytes
val char_of_jsonaf : Jsonaf_kernel.t -> char
val int_of_jsonaf : Jsonaf_kernel.t -> int
val float_of_jsonaf : Jsonaf_kernel.t -> float
val int32_of_jsonaf : Jsonaf_kernel.t -> int32
val int64_of_jsonaf : Jsonaf_kernel.t -> int64
val nativeint_of_jsonaf : Jsonaf_kernel.t -> nativeint
val ref_of_jsonaf : (Jsonaf_kernel.t -> 'a) -> Jsonaf_kernel.t -> 'a ref
val lazy_t_of_jsonaf : (Jsonaf_kernel.t -> 'a) -> Jsonaf_kernel.t -> 'a lazy_t
val option_of_jsonaf : (Jsonaf_kernel.t -> 'a) -> Jsonaf_kernel.t -> 'a option
val list_of_jsonaf : (Jsonaf_kernel.t -> 'a) -> Jsonaf_kernel.t -> 'a list
val array_of_jsonaf : (Jsonaf_kernel.t -> 'a) -> Jsonaf_kernel.t -> 'a array

val hashtbl_of_jsonaf
  :  (Jsonaf_kernel.t -> 'a)
  -> (Jsonaf_kernel.t -> 'b)
  -> Jsonaf_kernel.t
  -> ('a, 'b) Caml.Hashtbl.t
