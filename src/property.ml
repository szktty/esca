open Core

type value =
  | Method of Type.t
  | Value of Type.t

type t = {
  target : Type.t;
  value : value;
}

type map = t list String.Map.t

let shared : map ref = ref String.Map.empty

let add target ~name ~value =
  shared := String.Map.add_multi !shared ~key:name ~data:{ target; value }

let add_prim target ~name ~id ~value =
  let pkg, id = id in
  add target
    ~name
    ~value:(Method (Type.prim pkg id (Type.Spec.to_type value)))

let find target name : value option =
  Option.find_map (String.Map.find !shared name)
    ~f:(fun props ->
        List.find_map props
          ~f:(fun prop ->
              if Type.equal prop.target target then
                Some prop.value
              else
                None))
