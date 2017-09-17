open Core.Std

type t

type attr_kind = [`Type | `Value]

type attr = {
  attr_kind : attr_kind;
  attr_type : Type.t;
}

type attr_map = attr String.Map.t

val create :
  ?parent:t
  -> ?submodules:t list
  -> ?imports:t list
  -> ?attrs:attr_map
  -> string
  -> t

val name : t -> string

val root : t -> t option

val is_root : t -> bool

val parent : t -> t option

val import : t -> t -> unit

val namepath : t -> string Namepath.t

val find_module : ?prefix:string list -> t -> name:string -> t option

val add_module : t -> t -> unit

val attrs : t -> attr_map

val find_attr : t -> string -> attr option

val add_attr : t -> key:string -> data:attr -> unit

val attr : attr_kind -> Type.t -> attr

val type_attr : Type.t -> attr

val value_attr : Type.t -> attr
