open Core.Std

type 'a t = {
  prefix : 'a t option;
  name : 'a;
}

type value_path = string t

let create ?(prefix=None) name =
  { prefix; name }

let rec to_rev_list path =
  match path.prefix with
  | None -> [path.name]
  | Some prefix -> path.name :: to_rev_list prefix

let of_list (names:'a list) : 'a t =
  Option.value_exn
    (List.fold_right names
       ~init:None
       ~f:(fun name prefix -> Some (create name ~prefix)))

let to_list path =
  List.rev @@ to_rev_list path

let iter path ~f =
  List.iter (to_list path) ~f

let fold path ~init ~f =
  List.fold_right (to_rev_list path) ~init ~f

let to_string ?(sep=".") path =
  String.concat ~sep @@ to_list path
