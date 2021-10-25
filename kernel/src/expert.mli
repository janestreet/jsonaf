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

module Parser : sig
  type 'number parser := string -> ('number, string) result

  val t_without_trailing_whitespace : 'number parser -> 'number t Angstrom.t
  val t : 'number parser -> 'number t Angstrom.t
  val run : 'number parser -> string -> ('number t, string) result
  val run_many : 'number parser -> string -> ('number t list, string) result
end

module Serializer : sig
  type 'number serializer := Faraday.t -> 'number -> unit

  (** Faraday serializer for a json object given a serializer for the number type. This
      minimizes whitespace in the serialized representation. *)
  val serialize : 'number serializer -> 'number t -> Faraday.t -> unit

  (** serialize a json object to a string. Using {!serialize}, this produces a string
      which minimizes whitesapce. *)
  val run : 'number serializer -> 'number t -> string

  (** [serialize_hum ~spaces] is like [serialize] but it adds newlines and spaces in the
      same way that [JSON.stringify(_,_,spaces)] does in JavaScript. If [spaces] is 0 then
      the result is the same as that of {!serialize}. *)
  val serialize_hum : spaces:int -> 'number serializer -> 'number t -> Faraday.t -> unit

  (** [run_hum] is to {!run} as {!serialize_hum} is to {!serialize}: [run_hum ~spaces]
      will add newlines after each opening or closing bracket, and indent the contents an
      additional [spaces] spaces. No newlines are inserted if [spaces] is 0. It is typical
      to use [~spaces:2]. *)
  val run_hum : spaces:int -> 'number serializer -> 'number t -> string
end
