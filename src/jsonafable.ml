open! Base
include Jsonaf_kernel.Jsonafable

module Stable = struct
  module Of_jsonafable = struct
    module V1
        (Jsonafable : S)
        (M : sig
           type t

           val of_jsonafable : Jsonafable.t -> t
           val to_jsonafable : t -> Jsonafable.t
         end) =
    struct
      let t_of_jsonaf jsonaf = M.of_jsonafable (Jsonafable.t_of_jsonaf jsonaf)
      let jsonaf_of_t mt = M.to_jsonafable mt |> Jsonafable.jsonaf_of_t
    end
  end

  module Of_stringable = struct
    module V1 (M : Stringable.S) : S with type t := M.t = struct
      let t_of_jsonaf = function
        | `String s as json ->
          (match M.of_string s with
           | x -> x
           | exception exn -> Jsonaf_kernel.Conv.of_jsonaf_error_exn exn json)
        | json -> Jsonaf_kernel.Conv.of_jsonaf_error "string expected" json
      ;;

      let jsonaf_of_t (t : M.t) : Jsonaf_kernel.t = `String (M.to_string t)
    end
  end

  module Of_floatable = struct
    module V1 (M : Floatable.S) : S with type t := M.t = struct
      let t_of_jsonaf = function
        | `Number s as json ->
          (match M.of_float (Float.of_string s) with
           | x -> x
           | exception exn -> Jsonaf_kernel.Conv.of_jsonaf_error_exn exn json)
        | json -> Jsonaf_kernel.Conv.of_jsonaf_error "number expected" json
      ;;

      let jsonaf_of_t (t : M.t) : Jsonaf_kernel.t =
        `Number ((Dynamic.get Sexplib0.Sexp_conv.default_string_of_float) (M.to_float t))
      ;;
    end
  end
end

module Of_jsonafable = Stable.Of_jsonafable.V1
module Of_stringable = Stable.Of_stringable.V1
module Of_floatable = Stable.Of_floatable.V1
