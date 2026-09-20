let return_if condition return = if condition then Some (return ()) else None

module Infix = struct
  let ( >>= ) = Option.bind
end
