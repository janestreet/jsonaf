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
  constraint t = Type.t

module Parser = struct
  let parse_number = Result.ok

  let t_without_trailing_whitespace =
    Expert.Parser.t_without_trailing_whitespace parse_number
  ;;

  let t = Expert.Parser.t parse_number
  let run = Expert.Parser.run parse_number
  let run_many = Expert.Parser.run_many parse_number
end

module Serializer = struct
  let serialize_number f n = Faraday.write_string f n
  let serialize = Expert.Serializer.serialize serialize_number
  let run = Expert.Serializer.run serialize_number
  let serialize_hum ~spaces = Expert.Serializer.serialize_hum ~spaces serialize_number
  let run_hum ~spaces = Expert.Serializer.run_hum ~spaces serialize_number
end
