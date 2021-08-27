open! Core
open Type

module Conv_failure : sig
  type t

  (** [to_string_hum] returns the JSON context stack as a string that reads well if
      printed directly. That string cannot be used as a sexp atom: the backslash-escaping
      would render it illegible. *)
  val to_string_hum : t -> string

  (** [extract_exn] returns the exception that caused the failure *)
  val extract_exn : t -> exn
end

exception Of_json_conv_failed of Conv_failure.t


(** Wrap a [t] such that if it raises an exception, additional json context information
    will be added to help in debugging *)
val annotate : ?location:string -> 'a t -> 'a t

(** Pull a value out of an object by key, and use another [t] to reify that. Raises if
    the key does not exist. *)
val using : string -> 'a t -> 'a t

(** Same as [using], except returns an ['a option] in the event the key is missing. If the
    key is present the value will be passed on to the ['a t], even if it is [null]. If the
    value might be null, you probably want [option] instead. *)
val using_opt : string -> 'a t -> 'a option t

(* All operators are right-associative, with the exception of forward composition, which
   is an associative operation anyway anyway. *)

(** Operator of [using] for path traversal: "foo" @. "bar" @. int.
    Raises for non-objects.
*)
val ( @. ) : string -> 'a t -> 'a t

(** Operator of [using_opt] for path traversal with optional: "foo" @? "bar" @? int.
    Raises for non-objects.
*)
val ( @? ) : string -> 'a t -> 'a option t

(** A combination of [using_opt] and [option], to preserve the previous semantics of the
    [@?] operator that didn't distinguish between "key not present" and "key present but
    null".

    Do not use this function. Use [@?] or [option]. This only exists so that we can change
    [@?] without breaking all (untestable) uses of it. If you see this function used in an
    existing parser, and you are in a position to test it, you should update it to use
    either [@?] or [option].

    If you think you want to use this function -- if a key is sometimes absent, sometimes
    present but null, sometimes present but not null, and there's no semantic difference
    between the first two scenarios -- then you should still write that down with an
    explicit [Option.join] and a comment and some tests, because that's a crazy thing. *)
val ( @?? ) : string -> 'a t -> 'a option t

(** Suffix [map] for converting: "foo" @. int @> Satoshi.of_int *)
val ( @> ) : 'a t -> ('a -> 'b) -> 'b t


(** Simple forward composition: int @> Foo.of_int >>> Bar.of_foo *)
val ( >>> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

(** Simple accessors by type. Will raise if the type does not match *)

val int : int t

(** Always returns a float even if the JSON does not have a literal '.' in it *)
val float : float t

(** Returns the string representation of a JSON decimal number *)
val number : string t

val string : string t
val bool : bool t
val list : 'a t -> 'a list t

(** Returns [None] if the value is [`Null], and runs [t] otherwise *)
val option : 'a t -> 'a option t

(** Returns the string representation of either a JSON number or string. *)
val number_string : string t

(** Parse a JSON number or JSON string as an OCaml float or int. These are typically used
    as a last resort for inconsistent APIs. *)

val float_string : float t
val int_string : int t

(** Iterates over the keys of an object, passing each key to the [~f] provided and
    running the resulting [t] on the value of that key. Collects all results as a
    list. *)
val map_object : f:(string -> 'a t) -> 'a list t

(** [safe t] runs a generic [t] but returns an option instead of an exception *)
val safe : 'a t -> 'a option t

(** [a <|> b] first runs [a], and if it fails, it runs [b]. Will raise if [b] raises. *)
val ( <|> ) : 'a t -> 'a t -> 'a t

(** [choice [a; b; c]] returns the first parser that succeeds, or raises with the last
    if all raise. *)
val choice : 'a t list -> 'a t

(** A sexp embedded in a JSON string. *)
val as_sexp : (Sexp.t -> 'a) -> 'a t

module Array_as_tuple : sig
  (** When a JSON API returns an Array of elements of different types as if it were a
      tuple, you can use this module to parse each element in a different manner and
      assert that you didn't miss any. Example:

      {[
        let of_json =
          let open Of_json.Let_syntax in
          let%map_open parsed =
            "key" @. tuple Array_as_tuple.(
              [%map
                let a = shift @@ string
                and b = shift @@ number @> Int.of_string
                in (a, b)])
          in parsed
      ]}
  *)
  type 'a t

  (** Wrap your normal [Of_json] combinators with [shift] to have them operate on the head
      of the list element, passing the tail on to the next combinator. Build up a [t] by
      combining these with [apply] or [bind]. *)
  val shift : 'a Type.t -> 'a t

  val drop_rest : unit t

  module Let_syntax : sig
    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t
  end
end

(** Turn an [Array_as_tuple.t] into a [t] that will expect a JSON array and parse each
    element as a different type. If any elements are left unparsed, an error is raised. *)
val tuple : 'a Array_as_tuple.t -> 'a t
