open Core.Std

let type_modules : Type.t Module.t list ref = ref []

let type_imports : Type.t Module.t list ref = ref []

let value_modules : Type.t Module.t list ref = ref []

let value_imports : Type.t Module.t list ref = ref []

let find_module ?(path=[]) tops ~name =
  match path with
  | [] -> List.find tops ~f:(fun m -> Module.name m = name)
  | _ :: rest ->
    match List.find tops ~f:(fun m -> Module.name m = name) with
    | None -> None
    | Some m -> Module.find_module m ~prefix:rest ~name

let find_type_module name =
  find_module !type_modules ~name

let find_value_module name =
  find_module !value_modules ~name

let top_module_names mods =
  List.fold_left mods
    ~init:[]
    ~f:(fun accu m ->
        match Module.parent m with
        | Some _ -> accu
        | None -> Module.name m :: accu)

let top_module_attrs mods ~f =
  List.map (top_module_names mods) ~f:(fun name -> (name, f name))
  |> String.Map.of_alist_exn

let type_env () =
  let attrs = top_module_attrs !type_modules
      ~f:(fun name -> Type.module_ name) in
  Env.create ~imports:!type_modules ~attrs ()

let value_env () =
  let attrs = top_module_attrs !value_modules
      ~f:(fun name -> Type.module_ name) in
  Env.create ~imports:!value_modules ~attrs ()

module Spec = struct

  type vattr = {
    vattr_name : string;
    vattr_ty : Type.t;
  }

  type tattr = {
    tattr_name : string;
    tattr_ty : Type.t;
  }

  type t = {
    mod_name : string;
    init : bool;
    parent : string option;
    vattrs : vattr list;
    tattrs : tattr list;
  }

  let define ?parent ?(init=false) name =
    { mod_name = name; init; parent; vattrs = []; tattrs = [] }

  let (+>) spec attr =
    match attr with
    | `Type attr -> { spec with tattrs = attr :: spec.tattrs }
    | `Value attr -> { spec with vattrs = attr :: spec.vattrs }

  let typ name ty =
    `Type { tattr_name = name; tattr_ty = ty }

  let attr name ty = 
    `Value { vattr_name = name;
             vattr_ty = Type.prim name ty }

  let int name =
    attr name Type.int

  let string name =
    attr name Type.string

  let fun_ name (spec:Type.Spec.t) =
    attr name (Type.Spec.to_type spec)

  let end_ spec =
    (* TODO: parent *)
    Printf.printf "# add module %s\n" spec.mod_name;
    let tattrs = List.fold_left spec.tattrs
        ~init:String.Map.empty
        ~f:(fun accu attr ->
            String.Map.add accu ~key:attr.tattr_name ~data:attr.tattr_ty)
    in
    let vattrs = List.fold_left spec.vattrs
        ~init:String.Map.empty
        ~f:(fun accu attr ->
            String.Map.add accu ~key:attr.vattr_name ~data:attr.vattr_ty)
    in
    let tmod = Module.create spec.mod_name ~attrs:tattrs in
    let vmod = Module.create spec.mod_name ~attrs:vattrs in
    type_modules := tmod :: !type_modules;
    value_modules := vmod :: !value_modules;
    if spec.init then begin
      type_imports := tmod :: !type_imports;
      value_imports := vmod :: !value_imports;
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
