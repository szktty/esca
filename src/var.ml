open Core

type t = {
  name : string;
  kind : [`Type | `Value];
  type_ : Type.t;
  scope : [`Attr of string Namepath.t | `Local];
}

type _t = t

module Map = struct

  type t = _t String.Map.t

  let empty : t = String.Map.empty

  let add vars var =
    String.Map.add vars ~key:var.name ~data:var

end

let attr name ~kind ~type_ ~path =
  { name; kind; type_; scope = `Attr path }

let local name ~kind ~type_ =
  { name; kind; type_; scope = `Local }

let local_value name ~type_ =
  local name ~kind:`Value ~type_

let to_string var =
  Printf.sprintf "Var(%s, %s, %s, %s)"
    var.name
    begin match var.kind with
      | `Type -> "type"
      | `Value -> "value"
    end
    (Type.to_string var.type_)
    begin match var.scope with
      | `Local -> "Local"
      | `Attr path ->
        Printf.sprintf "Attr(%s)" (Namepath.to_string path)
    end
