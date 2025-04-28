(** The expert type and functions abstract over the type of [`Number], so one could build
    a parser that worked specifically for, say, [float t]s or [Bigdecimal.t t]s instead of
    using string as an intermediate type. *)

type 'number t =
  [ `Null
  | `False
  | `True
  | `String of string
  | `Number of 'number
  | `Object of (string * 'number t) list
  | `Array of 'number t list
  ]

module Or_raw : sig
  (** [Or_raw.t] is a JSON object that can contain raw, unquoted JSON. This can be used
      to, for instance, incorporate already-serialized JSON into new JSON objects without
      parsing them first. But there's no guarantee that an [Or_raw.t] represents valid
      JSON. The type is specified such that every [t] is also a valid [Or_raw.t]. *)
  type ('number, 't) t =
    [< `Raw_json_string of string
    | `Null
    | `False
    | `True
    | `String of string
    | `Number of 'number
    | `Object of (string * 't) list
    | `Array of 't list
    ]
    as
    't
end

module Parser : sig
  type 'number parser := string -> ('number, string) result

  val create_without_trailing_whitespace : 'number parser -> 'number t Angstrom.t
  val create : 'number parser -> 'number t Angstrom.t
end

module Serializer : sig
  type 'number serializer := Faraday.t -> 'number -> unit

  (** Faraday serializer for a json object given a serializer for the number type. This
      minimizes whitespace in the serialized representation. *)
  val create : 'number serializer -> ('number, _) Or_raw.t -> Faraday.t -> unit

  (** [create_hum ~spaces] is like [serialize] but it adds newlines and spaces in the same
      way that [JSON.stringify(_,_,spaces)] does in JavaScript. If [spaces] is 0 then the
      result is the same as that of {!serialize}. *)
  val create_hum
    :  spaces:int
    -> 'number serializer
    -> ('number, _) Or_raw.t
    -> Faraday.t
    -> unit
end
