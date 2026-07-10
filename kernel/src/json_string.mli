(** [Json_string] provides serialization and deserialization functions for the [`String]
    type of [Jsonaf_kernel.t]. It correctly handles different types of string encoding and
    character escaping. *)

val parse : string Angstrom.t
val serialize : Faraday.t -> string -> unit
