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
  | Range
  | Fun of t list * t
  | Ptr of t

let rec of_type (ty:Type.t) : t =
  match (Type.unwrap ty).desc with
  | Meta { contents = var } ->
    begin match var with
      | None -> failwith "must not be none"
      | Some ty -> of_type ty
    end
  | App (Tycon_void, []) -> Void
  | App (Tycon_bool, []) -> Bool
  | App (Tycon_int, []) -> Int
  | App (Tycon_float, []) -> Float32
  | App (Tycon_string, []) -> String
  | App (Tycon_range, []) -> Range
  | App (Tycon_ref, [arg]) -> Ptr (of_type arg)
  | App (Tycon_fun, args)
  | App (Tycon_method _, args) ->
    let rec collect args accu =
      match args with
      | [] -> failwith "error"
      | ret :: [] -> Fun (List.rev accu, of_type ret)
      | arg :: args ->
        collect args (of_type arg :: accu)
    in
    collect args []
  | Prim { prim_type } -> of_type prim_type
  | _ -> Printf.printf "type = %s\n" (Type.to_string ty);
    failwith "of_type: not supported"

let kernel_decl = Printf.sprintf "%sKernel" Go.Name.import_prefix
let range_decl = Printf.sprintf "%s.Range" kernel_decl

let rec to_decl = function
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
  | Range -> range_decl
  | Ptr ty -> Printf.sprintf "&%s" (to_decl ty)
  | Fun (args, ret) ->
    let open Buffer in
    let buf = create 16 in
    let rec add_args args =
      match args with
      | [] -> ()
      | arg :: [] ->
        add_string buf @@ to_decl arg
      | arg :: args ->
        add_string buf @@ to_decl arg;
        add_string buf ", ";
        add_args args
    in
    add_string buf "func (";
    add_args args;
    add_string buf ") ";
    add_string buf @@ to_decl ret;
    contents buf
  | _ -> failwith "not impl"

let return_ty = function
  | Fun (_, ret) -> Some ret
  | _ -> None

let return_ty_exn ty =
  Option.value_exn (return_ty ty)

let zero = function
  | Bool -> "false"
  | Uint | Uint8 | Uint16 | Uint32 | Uint64
  | Int | Int8 | Int16 | Int32 | Int64 -> "0"
  | Float32 | Float64 -> "0.0"
  | String -> ""
  | _ -> "nil"

let symbol_prefix = function
  | Void -> "Void"
  | Bool -> "Bool"
  | Uint -> "Uint"
  | Uint8 -> "Uint8"
  | Uint16 -> "Uint16"
  | Uint32 -> "Uint32"
  | Uint64 -> "Uint64"
  | Int -> "Int"
  | Int8 -> "Int8"
  | Int16 -> "Int16"
  | Int32 -> "Int32"
  | Int64 -> "Int64"
  | Float32 -> "Float32"
  | Float64 -> "Float64"
  | String -> "String"
  | _ -> failwith "not impl"

let symbol_method ty name =
  Printf.sprintf "EscaMeth_%s_%s" (symbol_prefix ty) name
