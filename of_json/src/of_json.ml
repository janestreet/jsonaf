open! Core

module Json = Json
module Timestamp = Timestamp
module To_json = To_json
include Type
include Helpers

module Let_syntax = struct
  let return = return

  module Let_syntax = struct
    let bind = bind
    let map = map
    let both = both

    module Open_on_rhs = Helpers
  end
end
