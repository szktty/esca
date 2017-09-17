open Core

type t = {
  imports : Module.t list;
  attrs : Module.attr_map;
}

let create ?(imports=[]) ?attrs () =
  { imports;
    attrs = Option.value attrs ~default:(String.Map.empty);
  }

let import env m =
  { env with imports = m :: env.imports }

let find env key =
  match String.Map.find env.attrs key with
  | Some _ as res -> res
  | None -> List.find_mapi env.imports
              ~f:(fun _ m -> Module.find_attr m key)

let add env ~key ~data =
  { env with attrs = String.Map.add env.attrs ~key ~data }

let concat env =
  let f attrs accu =
    String.Map.fold attrs ~init:accu
      ~f:(fun ~key ~data accu -> String.Map.add accu ~key ~data)
  in
  f env.attrs String.Map.empty

let merge env src =
  { env with attrs = String.Map.merge env.attrs src
                 ~f:(fun ~key:_ owner ->
                     match owner with
                     | `Left v | `Right v -> Some v
                     | `Both (_, v2) -> Some v2)
  }

let debug env =
  let open Printf in
  printf "{\n";
  printf "    imports = {\n";
  List.iter env.imports ~f:(fun m ->
      printf "        %s\n" (Module.name m));
  printf "    }\n";
  printf "    attrs = {\n";
  String.Map.iteri env.attrs ~f:(fun ~key ~data ->
      printf "        %s = %s\n" key (Type.to_string data.attr_type));
  printf "    }\n";
  printf "}\n"
