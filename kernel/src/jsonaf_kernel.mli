module Conv = Conv
module Expert = Expert
module Jsonafable = Jsonafable_intf

type t =
  [ `Null
  | `False
  | `True
  | `String of string
  | `Number of string
  | `Object of (string * t) list
  | `Array of t list
  ]

module Parser : sig
  val t : t Angstrom.t
  val run : string -> (t, string) result
end

module Serializer : sig
  (** Serialize a json object without any unnecessary whitespace. *)
  val serialize : t -> Faraday.t -> unit

  (** Convert a json object to a string as {!serialize} would. *)
  val run : t -> string

  (** [serialize_hum ~spaces t] serializes [t] in a “human readable” form with newlines
      separating elements of objects and arrays and an additional indentation of [spaces]
      spaces for their contents. A typical choice is [~spaces:2].

      Passing [~spaces:0] gives the exact same output as {!serialize}. *)
  val serialize_hum : spaces:int -> t -> Faraday.t -> unit

  (** [run_hum ~spaces, t] should produce very similar output to
      [JSON.stringify(t,null,spaces)] in JavaScript. *)
  val run_hum : spaces:int -> t -> string
end
