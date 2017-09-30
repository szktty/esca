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

  let prim_pkg = "esca.module"

  let pkg_name ~base mod_name =
    Printf.sprintf "%s.%s" base (Namepath.to_string ~sep:"." mod_name)

  let import_name mod_name =
    Printf.sprintf "__EscaModule_%s" (Namepath.to_string ~sep:"_" mod_name)

  let symbol ~kind name =
    let prefix = match kind with
      | `Fun -> "Fun"
      | `Var -> "Var"
      | `Type -> "Type"
    in
    Printf.sprintf "Esca%s_%s" prefix name

  let property s =
    Utils.camel_case s

  let value_path (path:string Namepath.t) =
    let prefix = import_name @@ Option.value_exn path.prefix in
    Printf.sprintf "%s.EscaValue_%s" prefix path.name

end
