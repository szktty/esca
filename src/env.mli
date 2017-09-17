type t

val create :
  ?imports:Module.t list
  -> ?attrs:Module.attr_map
  -> unit
  ->t

val find : t -> string -> Module.attr option

val import : t -> Module.t -> t

val add : t -> key:string -> data:Module.attr -> t

val merge : t -> Module.attr_map -> t

val concat : t -> Module.attr_map

val debug : t -> unit
