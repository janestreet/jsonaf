open! Base
include module type of Jsonaf_kernel.Jsonafable

module Of_jsonafable
    (Jsonafable : S)
    (M : sig
       type t

       val of_jsonafable : Jsonafable.t -> t
       val to_jsonafable : t -> Jsonafable.t
     end) : S with type t := M.t

module Of_stringable (M : Stringable.S) : S with type t := M.t
module Of_floatable (M : Floatable.S) : S with type t := M.t

module Stable : sig
  module Of_jsonafable : sig
    module V1 : module type of Of_jsonafable
  end

  module Of_stringable : sig
    module V1 : module type of Of_stringable
  end

  module Of_floatable : sig
    module V1 : module type of Of_floatable
  end
end
