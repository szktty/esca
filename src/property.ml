open Core

type t = {
  target : Type.t;
  ty : Type.t;
}

type map = t list String.Map.t

let shared : map ref = ref String.Map.empty

let add target ~name ~(ty:Type.t) =
  shared := String.Map.add_multi !shared ~key:name ~data:{ target; ty }

let add_method target ~name ~spec =
  add target ~name ~ty:(Type.fun_to_method @@ Type.Spec.to_type spec)

let find target name : Type.t option =
  Option.find_map (String.Map.find !shared name)
    ~f:(fun props ->
        List.find_map props
          ~f:(fun prop ->
              if Type.equal prop.target target then
                Some prop.ty
              else
                None))
