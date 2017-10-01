type t

val create :
  ?parent:t
  -> ?submodules:t list
  -> ?imports:t list
  -> ?attrs:Value.Map.t
  -> string
  -> package:string
  -> t

val name : t -> string

val root : t -> t option

val is_root : t -> bool

val parent : t -> t option

val import : t -> t -> unit

val namepath : t -> string Namepath.t

val find_module : ?prefix:string list -> t -> name:string -> t option

val add_module : t -> t -> unit

val attrs : t -> Value.Map.t

val find_attr : t -> string -> Value.t option

val add_attr : t -> Value.t -> unit

val package : t -> string

val use : t -> unit

val is_used : t -> bool
