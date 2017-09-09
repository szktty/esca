open Core

type t =
  | Void
  | Bool
  | Uint
  | Uint8
  | Uint16
  | Uint32
  | Uint64
  | Int
  | Int8
  | Int16
  | Int32
  | Int64
  | Float32
  | Float64
  | String
  | List of t
  | Option of t
  | Fun of t list * t

let rec of_type (ty:Type.t) : t =
  match (Type.unwrap ty).desc with
  | `Meta { contents = var } ->
    begin match var with
      | None -> failwith "must not be none"
      | Some ty -> of_type ty
    end
  | `App (`Unit, []) -> Void
  | `App (`Bool, []) -> Bool
  | `App (`Int, []) -> Int
  | `App (`Float, []) -> Float32
  | `App (`String, []) -> String
  | `App (`Fun, args) ->
    let rec collect args accu =
      match args with
      | [] -> failwith "error"
      | ret :: [] -> Fun (List.rev accu, of_type ret)
      | arg :: args ->
        collect args (of_type arg :: accu)
    in
    collect args []
  | _ -> Printf.printf "type = %s\n" (Type.to_string ty);
    failwith "not supported"

let rec to_string = function
  | Void -> ""
  | Bool -> "bool"
  | Uint -> "uint"
  | Uint8 -> "uint8"
  | Uint16 -> "uint16"
  | Uint32 -> "uint32"
  | Uint64 -> "uint64"
  | Int -> "int"
  | Int8 -> "int8"
  | Int16 -> "int16"
  | Int32 -> "int32"
  | Int64 -> "int64"
  | Float32 -> "float32"
  | Float64 -> "float64"
  | String -> "string"
  | Fun (args, ret) ->
    let open Buffer in
    let buf = create 16 in
    let rec add_args args =
      match args with
      | [] -> ()
      | arg :: [] ->
        add_string buf @@ to_string arg;
        add_args [arg]
      | arg :: args ->
        add_string buf @@ to_string arg;
        add_string buf ", ";
        add_args args
    in
    add_string buf "func (";
    add_args args;
    add_string buf " ";
    add_string buf @@ to_string ret;
    contents buf
  | _ -> failwith "not impl"
