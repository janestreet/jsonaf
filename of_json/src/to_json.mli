open! Core

type 'a t = 'a -> Json.t

(** Helpers for generating [Json.t] objects. *)

val bool : bool t
val float : float t
val int : int t
val to_json_list : 'a t -> 'a list t
