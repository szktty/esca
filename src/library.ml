open Core.Std

let top_modules : Module.t list ref = ref []

let top_imports : Module.t list ref = ref []

let find_module ?(path=[]) name =
  match path with
  | [] -> List.find !top_modules ~f:(fun m -> Module.name m = name)
  | _ :: rest ->
    match List.find !top_modules ~f:(fun m -> Module.name m = name) with
    | None -> None
    | Some m -> Module.find_module m ~prefix:rest ~name

let top_module_names () =
  List.fold_left !top_modules
    ~init:[]
    ~f:(fun accu m ->
        match Module.parent m with
        | Some _ -> accu
        | None -> Module.name m :: accu)

let top_module_attrs () =
  List.fold_left !top_modules
    ~init:String.Map.empty
    ~f:(fun attrs m ->
        String.Map.merge attrs (Module.attrs m)
          ~f:(fun ~key:_ attr ->
              match attr with
              | `Left a -> Some a
              | `Right a -> Some a
              | `Both (_, a) -> Some a))

let root_env () =
  Env.create ~imports:!top_modules ~attrs:(top_module_attrs ()) ()

module Spec = struct

  type t = {
    mod_name : string;
    init : bool;
    parent : string option;
    attrs : Value.Map.t;
    package : string;
  }

  let define ?parent ?(init=false) name ~package =
    { mod_name = name; init; parent; package; attrs = Value.Map.empty }

  let (+>) spec attr =
    { spec with attrs = Value.Map.add spec.attrs attr }

  let typ name type_ =
    Value.local name ~kind:`Type ~type_

  let value name type_ = 
    (* TODO: primitive *)
    Value.local name ~kind:`Value ~type_

  let int name =
    value name Type.int

  let string name =
    value name Type.string

  let fun_ name (spec:Type.Spec.t) =
    value name (Type.Spec.to_type spec)

  let end_ spec =
    (* TODO: parent *)
    Printf.printf "# add module %s\n" spec.mod_name;
    let attrs = Value.Map.map spec.attrs ~f:(fun attr ->
        { attr with scope = `Module (Namepath.create spec.mod_name) }) in
    let m = Module.create spec.mod_name ~attrs ~package:spec.package in
    top_modules := m :: !top_modules;
    if spec.init then begin
      top_imports := m :: !top_imports;
    end;
    ()

end

(*
let test () =
  let kernel = Spec.(define "Kernel"
                     +> fun_ "show" Type.Spec.(a @-> unit) "show"
                     +> string "version"
                    )
  in
  let sub = Module.define "Test" ~parent:kernel in
  sub
 *)
