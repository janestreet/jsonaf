open! Core
open Helpers

module Float_ms_since_epoch = struct
  type t = Time_ns.t

  include (Time_ns.Alternate_sexp : Sexpable.S with type t := t)

  let of_json = float @> Time_ns.Span.of_ms >>> Time_ns.of_span_since_epoch
  let to_json t = t |> Time_ns.to_span_since_epoch |> Time_ns.Span.to_ms |> To_json.float
end
