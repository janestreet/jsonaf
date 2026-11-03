open! Base
include module type of Jsonaf_kernel.Jsonafable

module%template.portable Of_jsonafable
    (Jsonafable : S)
    (M : sig
       type t

       val of_jsonafable : Jsonafable.t -> t
       val to_jsonafable : t -> Jsonafable.t
     end) : S with type t := M.t

module%template.portable Of_stringable (M : Stringable.S) : S with type t := M.t
module%template.portable Of_floatable (M : Floatable.S) : S with type t := M.t

module Stable : sig
  module Of_jsonafable : sig
    [%%template:
    [@@@modality.default p = (portable, nonportable)]

    module V1 : module type of Of_jsonafable [@modality p]]
  end

  module Of_stringable : sig
    [%%template:
    [@@@modality.default p = (portable, nonportable)]

    module V1 : module type of Of_stringable [@modality p]]
  end

  module Of_floatable : sig
    [%%template:
    [@@@modality.default p = (portable, nonportable)]

    module V1 : module type of Of_floatable [@modality p]]
  end
end
