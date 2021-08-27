open! Core

module Float_ms_since_epoch : sig
  (** JSON representation is a floating-point number of milliseconds since epoch. *)
  type t = Time_ns.t [@@deriving sexp]

  (* Per https://www.elastic.co/guide/en/elasticsearch/reference/current/date.html,

     > JSON doesn't have a date datatype, so dates in Elasticsearch can either be:

     >   1. strings containing formatted dates, e.g. "2015-01-01" or "2015/01/01 12:10:30".
     >   2. a long number representing milliseconds-since-the-epoch.
     >   3. an integer representing seconds-since-the-epoch.

     This module uses representation #2, milliseconds since the epoch.
  *)

  val of_json : t Type.t
  val to_json : t -> Json.t
end
