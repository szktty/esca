type t

val create :
  ?imports:Module.t list
  -> ?attrs:Value.Map.t
  -> unit
  ->t

val find : t -> string -> Value.t option

val import : t -> Module.t -> t

val add : t -> Value.t -> t

val merge : t -> Value.Map.t -> t

val concat : t -> Value.Map.t

val debug : t -> unit
