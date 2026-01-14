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

external mode_cross : t @ contended -> t @ portable @@ portable = "%identity"
