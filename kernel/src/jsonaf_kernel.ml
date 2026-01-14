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

let mode_cross = Type.mode_cross

module Parser = struct
  let parse_number = Result.ok

  let t_without_trailing_whitespace =
    Expert.Parser.create_without_trailing_whitespace parse_number
  ;;

  let t = Expert.Parser.create parse_number
  let run_angstrom parser_ = Angstrom.parse_string ~consume:All parser_
  let run = run_angstrom t
  let run_many = run_angstrom (Angstrom.many t)
end

module Serializer = struct
  let serialize_number f n = Faraday.write_string f n
  let serialize = Expert.Serializer.create serialize_number
  let serialize_hum ~spaces = Expert.Serializer.create_hum ~spaces serialize_number

  let run t =
    let faraday = Faraday.create 0x1000 in
    serialize t faraday;
    Faraday.serialize_to_string faraday
  ;;

  let run_hum ~spaces t =
    let faraday = Faraday.create 0x1000 in
    serialize_hum ~spaces t faraday;
    Faraday.serialize_to_string faraday
  ;;
end
