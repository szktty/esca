open Core.Std

type t = desc Located.t

and desc =
  | App of tycon * t list
  | Var of tyvar
  | Poly of [ `Preunify of tyvar list * t | `Unify of t ] ref
  | Prim of primitive
  | Unique of t * Valuepath.t
  | Meta of metavar

and tycon =
  | Tycon_any
  | Tycon_void
  | Tycon_bool
  | Tycon_int
  | Tycon_float
  | Tycon_string
  | Tycon_list
  | Tycon_tuple
  | Tycon_range
  | Tycon_option
  | Tycon_ref
  | Tycon_struct of string list
  | Tycon_enum of string list
  | Tycon_fun
  | Tycon_printf
  | Tycon_method of t
  | Tycon_module of string
  | Tycon_stream
  | Tycon_tyfun of tyvar list * t

and tyvar = string

and metavar = t option ref

and primitive = {
  prim_pkg : string;
  prim_id : string;
  prim_type : t;
}

let create loc desc =
  Located.create loc desc

let metavar loc =
  Located.create loc (Meta (ref None))

let metavar_some loc =
  metavar (Some loc)

let var_names = [|
  "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m"; "n";
  "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z";
|]

let equal (ty1:t) (ty2:t) =
  ty1.desc = ty2.desc

let rec to_string (ty:t) =
  match ty.desc with
  | App (tycon, args) ->
    let tycon_s = tycon_to_string tycon in
    let args_s =
      List.map args ~f:to_string
      |> String.concat ~sep:", "
    in
    Printf.sprintf "App(%s, [%s])" tycon_s args_s
  | Meta { contents = None } -> "Meta(?)"
  | Meta { contents = Some ty } -> "Meta(" ^ to_string ty ^ ")"
  | Var name -> "Var(" ^ name ^ ")"
  | Poly { contents = `Preunify (tyvars, ty) } ->
    Printf.sprintf "Poly([%s], %s)"
      (String.concat tyvars ~sep:", ") (to_string ty)
  | Poly { contents = `Unify ty } ->
    Printf.sprintf "Poly(%s)" (to_string ty)
  | Prim prim ->
    Printf.sprintf "Prim(\"%s.%s\", %s)"
      prim.prim_pkg prim.prim_id (to_string prim.prim_type)
  | Unique (ty, path) ->
    Printf.sprintf "Unique(%s, \"%s\")"
      (to_string ty) (Namepath.to_string path) 

and tycon_to_string = function
  | Tycon_any -> "Any"
  | Tycon_void -> "Void"
  | Tycon_bool -> "Bool"
  | Tycon_int -> "Int"
  | Tycon_float -> "Float"
  | Tycon_string -> "String"
  | Tycon_list -> "List"
  | Tycon_tuple -> "Tuple"
  | Tycon_range -> "Range"
  | Tycon_fun -> "Fun"
  | Tycon_printf -> "Fun_printf"
  | Tycon_method ty -> Printf.sprintf "Method(%s)" (to_string ty)
  | Tycon_stream -> "Stream"
  | Tycon_option -> "Option"
  | Tycon_ref -> "Ref"
  | Tycon_module name -> Printf.sprintf "Module(%s)" name
  | _ -> failwith "not impl"

let rec unwrap (ty:t) =
  match ty.desc with
  | Meta { contents = Some ty }
  | Poly { contents = `Preunify (_, ty) }  -> unwrap ty
  | Poly { contents = `Unify ty } -> unwrap ty
  | Prim { prim_type } -> unwrap prim_type
  | _ -> ty

let rec fun_params (ty:t) =
  match (unwrap ty).desc with
  | App (Tycon_fun, args) -> List.rev args |> List.tl_exn |> List.rev
  | Prim { prim_type } -> fun_params prim_type
  | _ -> failwith "not function"

let rec fun_return (ty:t) =
  match ty.desc with
  | Meta { contents = Some ty }
  | Poly { contents = `Preunify (_, ty) } -> fun_return ty
  | Poly { contents = `Unify ty } -> fun_return ty
  | App (Tycon_fun, args)
  | App (Tycon_method _, args) -> List.last_exn args
  | Prim { prim_type } -> fun_return prim_type
  | _ -> failwith @@ Printf.sprintf "not function %s" (to_string ty)

let module_name (ty:t) =
  match (unwrap ty).desc with
  | App (Tycon_module name, _) -> Some name
  | _ -> None

let prim_id (ty:t) =
  match (unwrap ty).desc with
  | Prim { prim_id } -> Some prim_id
  | _ -> None

let prim_id_exn ty =
  Option.value_exn (prim_id ty)

let tyvar name = Located.less @@ Var name
let tyvar_a = tyvar "a"
let tyvar_b = tyvar "b"
let tyvar_c = tyvar "c"
let tyvar_d = tyvar "d"

let poly tyvars (ty:t) : desc = Poly (ref (`Preunify (tyvars, ty)))

let app ?(args=[]) tycon = App (tycon, args)
let desc_any = app Tycon_any
let desc_void = app Tycon_void
let desc_bool = app Tycon_bool
let desc_int = app Tycon_int
let desc_float = app Tycon_float
let desc_string = app Tycon_string
let desc_range = app Tycon_range
let desc_list e = app ~args:[e] Tycon_list
let desc_tuple es = app ~args:es Tycon_tuple
let desc_option e = app ~args:[e] Tycon_option
let desc_ref e = app ~args:[e] Tycon_ref
let desc_fun params ret =
  app ~args:(List.append params [ret]) Tycon_fun
let desc_fun_printf = app Tycon_printf
let desc_stream = app Tycon_stream

let void = Located.less desc_void
let bool = Located.less desc_bool
let int = Located.less desc_int
let float = Located.less desc_float
let string = Located.less desc_string
let range = Located.less desc_range
let list e = Located.less @@ desc_list e
let list_gen = Located.less @@ poly ["a"] (list tyvar_a)
let tuple es = Located.less @@ desc_tuple es
let option e = Located.less @@ desc_option e
let option_gen = Located.less @@ poly ["a"] (option tyvar_a)
let ref e = Located.less @@ desc_ref e
let ref_gen = Located.less @@ poly ["a"] (ref tyvar_a)
let fun_ loc params ret = Located.create loc @@ desc_fun params ret
let fun_printf = Located.less @@ desc_fun_printf

let fun_to_method (f:t) : t = 
  let rec to_method (ty:t) =
    match ty.desc with
    | Meta { contents = Some ty } -> to_method ty
    | App (Tycon_fun, recv :: args) -> App (Tycon_method recv, args)
    | Poly { contents = `Preunify (tyvars, ty) } ->
      Poly (Ref.create (`Preunify (tyvars, (Located.create ty.loc (to_method ty)))))
    | Poly { contents = `Unify ty } -> to_method ty
    | _ -> failwith "not supported"
  in
  Located.create f.loc (to_method f)

let prim pkg name ty =
  Located.less @@ Prim { prim_pkg = pkg;
                         prim_id = name;
                         prim_type = ty }

let module_ name = Located.less @@ app (Tycon_module name)
let stream = Located.less @@ desc_stream 

let unique ty ~path =
  Located.less @@ Unique (ty, path)

let struct_ fields =
  let names, tys = List.fold_left fields ~init:([], [])
      ~f:(fun (names, tys) (name, ty) ->
          name :: names, ty :: tys)
  in
  App (Tycon_struct (List.rev names), List.rev tys)

let parse_format s =
  let module F = Utils.Format in
  let fmt = Utils.Format.parse s in
  List.map (F.params fmt)
    ~f:(function
        | F.Int -> int
        | F.String -> string
        | _ -> failwith "parse_format")

module Spec = struct

  type t = [
    | `Tyvar of string
    | `Any
    | `Void
    | `Bool
    | `Int
    | `Float
    | `String
    | `List of t
    | `Tuple of t list
    | `Range
    | `Option of t
    | `Ref of t
    | `Fun of t list
    | `Printf
    | `Stream
  ]

  let any = `Any
  let void = `Void
  let bool = `Bool
  let int = `Int
  let float = `Float
  let string = `String
  let list e = `List e
  let tuple es = `Tuple es
  let range = `Range
  let option e = `Option e
  let ref e = `Ref e
  let printf = `Printf
  let stream = `Stream

  let a = `Tyvar "a"
  let b = `Tyvar "b"
  let c = `Tyvar "c"
  let d = `Tyvar "d"
  let e = `Tyvar "e"

  let (@->) x y =
    match x with
    | `Fun args -> `Fun (List.append args [y])
    | _ -> `Fun [x; y]

  let flat_tyvars tyvars =
    let tyvars =
      List.fold_left tyvars
        ~init:[]
        ~f:(fun accu tyvar ->
            if List.existsi accu ~f:(fun _ e -> e = tyvar) then
              accu
            else
              tyvar :: accu)
    in
    List.sort tyvars ~cmp:String.Caseless.descending

  let collect_tyvars (spec:t) =
    let rec f (tyvars:string list) spec =
      match spec with
      | `Void -> tyvars, desc_void
      | `Bool -> tyvars, desc_bool
      | `Int -> tyvars, desc_int
      | `Float -> tyvars, desc_float
      | `String -> tyvars, desc_string
      | `Stream -> tyvars, desc_stream
      | `Printf -> tyvars, desc_fun_printf
      | `Tyvar name -> (name :: tyvars), Var name
      | `List e ->
        let tyvars', ty = f tyvars e in
        tyvars', desc_list (Located.less ty)
      | `Fun args ->
        let tyvars', args' =
          List.fold_left args ~init:(tyvars, [])
            ~f:(fun (tyvars, args) arg ->
                let tyvars', arg' = f tyvars arg in
                tyvars', Located.less arg' :: args)
        in
        tyvars', App (Tycon_fun, List.rev args')
      | _ -> failwith "not yet support"
    in
    f [] spec

  let to_type spec =
    match collect_tyvars spec with
    | [], desc -> Located.less desc
    | tyvars, desc ->
      let tyvars = flat_tyvars tyvars in
      Located.less @@ poly tyvars (Located.less desc)

end
