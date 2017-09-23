open Core

module Repr = struct

  let of_bool = function
    | true -> "true"
    | false -> "false"

  let of_string s =
    (* TODO: escape *)
    "\"" ^ s ^ "\""

end

module Name = struct

  let property s = "Esca_" ^ s

end
