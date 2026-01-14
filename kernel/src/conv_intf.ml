module type Primitives = sig
  include sig @@ portable
    val jsonaf_of_unit : unit -> Type.t
    val jsonaf_of_bool : bool -> Type.t
    val jsonaf_of_string : string -> Type.t
    val jsonaf_of_bytes : bytes -> Type.t
    val jsonaf_of_char : char -> Type.t
    val jsonaf_of_int : int -> Type.t
    val jsonaf_of_float : float -> Type.t
    val jsonaf_of_int32 : int32 -> Type.t
    val jsonaf_of_int64 : int64 -> Type.t
    val jsonaf_of_nativeint : nativeint -> Type.t
    val jsonaf_of_ref : ('a -> Type.t) -> 'a ref -> Type.t
    val jsonaf_of_lazy_t : ('a -> Type.t) -> 'a lazy_t -> Type.t
    val jsonaf_of_option : ('a -> Type.t) -> 'a option -> Type.t
    val jsonaf_of_list : ('a -> Type.t) -> 'a list -> Type.t
    val jsonaf_of_array : ('a -> Type.t) -> 'a array -> Type.t

    val jsonaf_of_hashtbl
      :  ('a -> Type.t)
      -> ('b -> Type.t)
      -> ('a, 'b) Hashtbl.t
      -> Type.t
  end

  val unit_of_jsonaf : Type.t -> unit
  val bool_of_jsonaf : Type.t -> bool
  val string_of_jsonaf : Type.t -> string
  val bytes_of_jsonaf : Type.t -> bytes
  val char_of_jsonaf : Type.t -> char
  val int_of_jsonaf : Type.t -> int
  val float_of_jsonaf : Type.t -> float
  val int32_of_jsonaf : Type.t -> int32
  val int64_of_jsonaf : Type.t -> int64
  val nativeint_of_jsonaf : Type.t -> nativeint
  val ref_of_jsonaf : (Type.t -> 'a) -> Type.t -> 'a ref
  val lazy_t_of_jsonaf : (Type.t -> 'a) -> Type.t -> 'a lazy_t
  val option_of_jsonaf : (Type.t -> 'a) -> Type.t -> 'a option
  val list_of_jsonaf : (Type.t -> 'a) -> Type.t -> 'a list
  val array_of_jsonaf : (Type.t -> 'a) -> Type.t -> 'a array
  val hashtbl_of_jsonaf : (Type.t -> 'a) -> (Type.t -> 'b) -> Type.t -> ('a, 'b) Hashtbl.t
end

module type Conv = sig
  (** Utility Module for Jsonaf_kernel Conversions *)

  (** Conversion of OCaml-values to Jsonaf_kernels *)

  include sig @@ portable
    (** [jsonaf_of_unit ()] converts a value of type [unit] to an Jsonaf_kernel. *)
    val jsonaf_of_unit : unit -> Type.t

    (** [jsonaf_of_bool b] converts the value [b] of type [bool] to an Jsonaf_kernel. *)
    val jsonaf_of_bool : bool -> Type.t

    (** [jsonaf_of_string str] converts the value [str] of type [string] to an
        Jsonaf_kernel. *)
    val jsonaf_of_string : string -> Type.t

    (** [jsonaf_of_bytes str] converts the value [str] of type [bytes] to an
        Jsonaf_kernel. *)
    val jsonaf_of_bytes : bytes -> Type.t

    (** [jsonaf_of_char c] converts the value [c] of type [char] to an Jsonaf_kernel. *)
    val jsonaf_of_char : char -> Type.t

    (** [jsonaf_of_int n] converts the value [n] of type [int] to an Jsonaf_kernel. *)
    val jsonaf_of_int : int -> Type.t

    (** [jsonaf_of_float n] converts the value [n] of type [float] to an Jsonaf_kernel. *)
    val jsonaf_of_float : float -> Type.t

    (** [jsonaf_of_int32 n] converts the value [n] of type [int32] to an Jsonaf_kernel. *)
    val jsonaf_of_int32 : int32 -> Type.t

    (** [jsonaf_of_int64 n] converts the value [n] of type [int64] to an Jsonaf_kernel. *)
    val jsonaf_of_int64 : int64 -> Type.t

    (** [jsonaf_of_nativeint n] converts the value [n] of type [nativeint] to an
        Jsonaf_kernel. *)
    val jsonaf_of_nativeint : nativeint -> Type.t

    (** [jsonaf_of_ref conv r] converts the value [r] of type ['a ref] to an
        Jsonaf_kernel. Uses [conv] to convert values of type ['a] to an Jsonaf_kernel. *)
    val jsonaf_of_ref : ('a -> Type.t) -> 'a ref -> Type.t

    (** [jsonaf_of_lazy_t conv l] converts the value [l] of type ['a lazy_t] to an
        Jsonaf_kernel. Uses [conv] to convert values of type ['a] to an Jsonaf_kernel. *)
    val jsonaf_of_lazy_t : ('a -> Type.t) -> 'a lazy_t -> Type.t

    (** [jsonaf_of_option conv opt] converts the value [opt] of type ['a option] to an
        Jsonaf_kernel. Uses [conv] to convert values of type ['a] to an Jsonaf_kernel. *)
    val jsonaf_of_option : ('a -> Type.t) -> 'a option -> Type.t

    (** [jsonaf_of_pair conv1 conv2 pair] converts a pair to an Jsonaf_kernel. It uses its
        first argument to convert the first element of the pair, and its second argument
        to convert the second element of the pair. *)
    val jsonaf_of_pair : ('a -> Type.t) -> ('b -> Type.t) -> 'a * 'b -> Type.t

    (** [jsonaf_of_triple conv1 conv2 conv3 triple] converts a triple to an Jsonaf_kernel
        using [conv1], [conv2], and [conv3] to convert its elements. *)
    val jsonaf_of_triple
      :  ('a -> Type.t)
      -> ('b -> Type.t)
      -> ('c -> Type.t)
      -> 'a * 'b * 'c
      -> Type.t

    (** [jsonaf_of_list conv lst] converts the value [lst] of type ['a list] to an
        Jsonaf_kernel. Uses [conv] to convert values of type ['a] to an Jsonaf_kernel. *)
    val jsonaf_of_list : ('a -> Type.t) -> 'a list -> Type.t

    (** [jsonaf_of_array conv ar] converts the value [ar] of type ['a array] to an
        Jsonaf_kernel. Uses [conv] to convert values of type ['a] to an Jsonaf_kernel. *)
    val jsonaf_of_array : ('a -> Type.t) -> 'a array -> Type.t

    (** [jsonaf_of_hashtbl conv_key conv_value htbl] converts the value [htbl] of type
        [('a, 'b) Hashtbl.t] to an Jsonaf_kernel. Uses [conv_key] to convert the hashtable
        keys of type ['a], and [conv_value] to convert hashtable values of type ['b] to
        Jsonaf_kernels. *)
    val jsonaf_of_hashtbl
      :  ('a -> Type.t)
      -> ('b -> Type.t)
      -> ('a, 'b) Hashtbl.t
      -> Type.t

    (** [jsonaf_of_opaque x] converts the value [x] of opaque type to an Jsonaf_kernel.
        This means the user need not provide converters, but the result cannot be
        interpreted. *)
    val jsonaf_of_opaque : 'a -> Type.t

    (** [jsonaf_of_fun f] converts the value [f] of function type to a dummy
        Jsonaf_kernel. Functions cannot be serialized as Jsonaf_kernels, but at least a
        placeholder can be generated for pretty-printing. *)
    val jsonaf_of_fun : ('a -> 'b) -> Type.t
  end

  (** Conversion of Jsonaf_kernels to OCaml-values *)

  (** [Of_jsonaf_error (exn, jsonaf)] the exception raised when an Jsonaf_kernel could not
      be successfully converted to an OCaml-value. *)
  exception Of_jsonaf_error of exn * Type.t @@ contended portable

  (** [record_check_extra_fields] checks for extra (= unknown) fields in record
      Jsonaf_kernels. *)
  val record_check_extra_fields : bool ref

  (** [of_jsonaf_error reason jsonaf]
      @raise Of_jsonaf_error (Failure reason, jsonaf). *)
  val of_jsonaf_error : string -> Type.t -> 'a

  (** [of_jsonaf_error_exn exc jsonaf]
      @raise Of_jsonaf_error (exc, jsonaf). *)
  val of_jsonaf_error_exn : exn -> Type.t -> 'a

  (** [unit_of_jsonaf jsonaf] converts Jsonaf_kernel [jsonaf] to a value of type [unit]. *)
  val unit_of_jsonaf : Type.t -> unit

  (** [bool_of_jsonaf jsonaf] converts Jsonaf_kernel [jsonaf] to a value of type [bool]. *)
  val bool_of_jsonaf : Type.t -> bool

  (** [string_of_jsonaf jsonaf] converts Jsonaf_kernel [jsonaf] to a value of type
      [string]. *)
  val string_of_jsonaf : Type.t -> string

  (** [bytes_of_jsonaf jsonaf] converts Jsonaf_kernel [jsonaf] to a value of type [bytes]. *)
  val bytes_of_jsonaf : Type.t -> bytes

  (** [char_of_jsonaf jsonaf] converts Jsonaf_kernel [jsonaf] to a value of type [char]. *)
  val char_of_jsonaf : Type.t -> char

  (** [int_of_jsonaf jsonaf] converts Jsonaf_kernel [jsonaf] to a value of type [int]. *)
  val int_of_jsonaf : Type.t -> int

  (** [float_of_jsonaf jsonaf] converts Jsonaf_kernel [jsonaf] to a value of type [float]. *)
  val float_of_jsonaf : Type.t -> float

  (** [int32_of_jsonaf jsonaf] converts Jsonaf_kernel [jsonaf] to a value of type [int32]. *)
  val int32_of_jsonaf : Type.t -> int32

  (** [int64_of_jsonaf jsonaf] converts Jsonaf_kernel [jsonaf] to a value of type [int64]. *)
  val int64_of_jsonaf : Type.t -> int64

  (** [nativeint_of_jsonaf jsonaf] converts Jsonaf_kernel [jsonaf] to a value of type
      [nativeint]. *)
  val nativeint_of_jsonaf : Type.t -> nativeint

  (** [ref_of_jsonaf conv jsonaf] converts Jsonaf_kernel [jsonaf] to a value of type
      ['a ref] using conversion function [conv], which converts an Jsonaf_kernel to a
      value of type ['a]. *)
  val ref_of_jsonaf : (Type.t -> 'a) -> Type.t -> 'a ref

  (** [lazy_t_of_jsonaf conv jsonaf] converts Jsonaf_kernel [jsonaf] to a value of type
      ['a lazy_t] using conversion function [conv], which converts an Jsonaf_kernel to a
      value of type ['a]. *)
  val lazy_t_of_jsonaf : (Type.t -> 'a) -> Type.t -> 'a lazy_t

  (** [option_of_jsonaf conv jsonaf] converts Jsonaf_kernel [jsonaf] to a value of type
      ['a option] using conversion function [conv], which converts an Jsonaf_kernel to a
      value of type ['a]. *)
  val option_of_jsonaf : (Type.t -> 'a) -> Type.t -> 'a option

  (** [pair_of_jsonaf conv1 conv2 jsonaf] converts Jsonaf_kernel [jsonaf] to a pair of
      type ['a * 'b] using conversion functions [conv1] and [conv2], which convert
      Jsonaf_kernels to values of type ['a] and ['b] respectively. *)
  val pair_of_jsonaf : (Type.t -> 'a) -> (Type.t -> 'b) -> Type.t -> 'a * 'b

  (** [triple_of_jsonaf conv1 conv2 conv3 jsonaf] converts Jsonaf_kernel [jsonaf] to a
      triple of type ['a * 'b * 'c] using conversion functions [conv1], [conv2], and
      [conv3], which convert Jsonaf_kernels to values of type ['a], ['b], and ['c]
      respectively. *)
  val triple_of_jsonaf
    :  (Type.t -> 'a)
    -> (Type.t -> 'b)
    -> (Type.t -> 'c)
    -> Type.t
    -> 'a * 'b * 'c

  (** [list_of_jsonaf conv jsonaf] converts Jsonaf_kernel [jsonaf] to a value of type
      ['a list] using conversion function [conv], which converts an Jsonaf_kernel to a
      value of type ['a]. *)
  val list_of_jsonaf : (Type.t -> 'a) -> Type.t -> 'a list

  (** [array_of_jsonaf conv jsonaf] converts Jsonaf_kernel [jsonaf] to a value of type
      ['a array] using conversion function [conv], which converts an Jsonaf_kernel to a
      value of type ['a]. *)
  val array_of_jsonaf : (Type.t -> 'a) -> Type.t -> 'a array

  (** [hashtbl_of_jsonaf conv_key conv_value jsonaf] converts Jsonaf_kernel [jsonaf] to a
      value of type [('a, 'b) Hashtbl.t] using conversion function [conv_key], which
      converts an Jsonaf_kernel to hashtable key of type ['a], and function [conv_value],
      which converts an Jsonaf_kernel to hashtable value of type ['b]. *)
  val hashtbl_of_jsonaf : (Type.t -> 'a) -> (Type.t -> 'b) -> Type.t -> ('a, 'b) Hashtbl.t

  (** [opaque_of_jsonaf jsonaf]
      @raise Of_jsonaf_error
        when attempting to convert an Jsonaf_kernel to an opaque value. *)
  val opaque_of_jsonaf : Type.t -> 'a

  (** [fun_of_jsonaf jsonaf]
      @raise Of_jsonaf_error when attempting to convert an Jsonaf_kernel to a function. *)
  val fun_of_jsonaf : Type.t -> 'a

  module type Primitives = Primitives

  module Primitives : Primitives
end
