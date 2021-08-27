open Core

module T = struct
  type 'a t = Json.t -> 'a

  let run t json = t json
  let bind t ~f json = run (f (run t json)) json
  let return a = Fn.const a
  let map t ~f json = f (run t json)
  let map = `Custom map
  let apply af ax json = run af json @@ run ax json
end

include T
include Applicative.Make (T)
include (Monad.Make (T) : Monad.S_without_syntax with type 'a t := 'a t)
