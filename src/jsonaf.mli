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
[@@deriving sexp, globalize]

(** Note that we intentionally do not expose [compare] or [equal] functions for [t].
    Objects in JSON are considered unordered, so two different representations of [t] may
    be unequal using the derived equal but the same according to the JSON spec. *)

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
    with [parse]. But this is much slower than [run_many]. *)
val parse_many : string -> t list Or_error.t

include Stringable.S with type t := t

(** human-readable output, indenting all fields/array elements by two spaces. *)
val to_string_hum : t -> string

include Pretty_printer.S with type t := t
module Jsonafable = Jsonafable
include Jsonafable.S with type t := t

module Parser : sig
  (** For parsing a JSON stream, e.g., when using [Angstrom_async.parse_many], you should
      instead use [t_without_trailing_whitespace]. *)
  val t : t Angstrom.t

  (** [t_without_trailing_whitespace] will parse a single JSON value without consuming any
      trailing whitespace. This is useful in the context of streaming multiple JSON values
      because it will immediately return [t] when it is parsed, but it may fail in other
      parsing contexts where the input buffer is expected to be fully consumed. *)
  val t_without_trailing_whitespace : t Angstrom.t

  val run : string -> (t, string) Result.t
  val run_many : string -> (t list, string) Result.t
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

(** If [t] is an object, return the keys of that object. Otherwise, return [None]. O(n). *)
val keys : t -> string list option

(** If [t] is an object, return the keys of that object. Otherwise, raise. O(n). *)
val keys_exn : t -> string list

module Export : Jsonaf_kernel.Conv.Primitives
