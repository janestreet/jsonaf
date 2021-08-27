
(** An exposed reader monad that allows you to write pretty terse json
    reification functions with extra exception wrapping to add much-needed
    context to errors. *)

open! Core

type 'a t = Json.t -> 'a

val run : 'a t -> Json.t -> 'a

include Applicative.S with type 'a t := 'a t
include Monad.S_without_syntax with type 'a t := 'a t
