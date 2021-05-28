# Jsonaf

This is a library for parsing, manipulating, and serializing data structured as
JSON. It's built on the [Angstrom][] and [Faraday][] libraries. The underlying
type is a simple polymorphic type:

```ocaml
type t =
  [ `Null
  | `False
  | `True
  | `String of string
  | `Number of string
  | `Object of (string * t) list
  | `Array of t list
  ]
```

Note that `Number` has a type of `string`! This is intentional, as it is left up
to the user to decide how numbers should be interpreted. Certain fields might be
integers while others might be floats.

The most bare-bones use case of this library is to pattern match on the type
above and use the following two functions:

```ocaml
val of_string : string -> t
val to_string : t -> string
```

## Jsonaf_kernel

The kernel library simply contains the type and functions to serialize and
deserialize the type to a string. It has no other dependencies outside of
Angstrom and Faraday.

[angstrom]: https://github.com/inhabitedtype/angstrom
[faraday]: https://github.com/inhabitedtype/faraday
