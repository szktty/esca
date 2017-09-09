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

let of_type (ty:Type.t) : t =
  match (Type.unwrap ty).desc with
  | `App (`Unit, []) -> Void
  | `App (`Bool, []) -> Bool
  | `App (`Int, []) -> Int
  | `App (`Float, []) -> Float32
  | `App (`String, []) -> String
  | _ -> Printf.printf "type = %s\n" (Type.to_string ty);
    failwith "not supported"

let to_string = function
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
  | _ -> failwith "not impl"
