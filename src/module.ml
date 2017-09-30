open Core.Std

type t = {
  parent : t option;
  name : string;
  mutable submodules : t list;
  mutable imports : t list;
  mutable attrs : Var.Map.t;
}

let create ?parent ?(submodules=[]) ?(imports=[]) ?attrs name =
  { parent; name; submodules; imports;
    attrs = Option.value attrs ~default:String.Map.empty }

let name m = m.name

let rec root m =
  match m.parent with
  | None -> Some m
  | Some m -> root m

let is_root m = Option.is_none m.parent

let parent m = m.parent

let import m x =
  m.imports <- x :: m.imports

let rec namepath m =
  match m.parent with
  | None -> Namepath.create m.name
  | Some m ->
    Namepath.create ~prefix:(Some (namepath m)) m.name

let rec find_module ?(prefix=[]) m ~name =
  match prefix with
  | [] -> failwith "must not be empty"
  | fst :: rest ->
    match find_module m ~name:fst with
    | None -> None
    | Some m -> find_module ~prefix:rest m ~name

let add_module m x =
  m.submodules <- x :: m.submodules

let attrs m =
  (* TODO: import *)
  m.attrs

let rec find_attr m key =
  match String.Map.find m.attrs key with
  | Some _ as res -> res
  | None ->
    match List.find_mapi m.imports ~f:(fun _ m -> find_attr m key) with
    | Some _ as v -> v
    | None -> None

let add_attr m attr =
  m.attrs <- Var.Map.add m.attrs attr
