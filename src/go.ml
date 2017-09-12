module Repr = struct

  let of_bool = function
    | true -> "true"
    | false -> "false"

  let of_string s =
    (* TODO: escape *)
    "\"" ^ s ^ "\""

end
