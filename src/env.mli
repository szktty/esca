type t

val create :
  ?imports:Module.t list
  -> ?attrs:Var.Map.t
  -> unit
  ->t

val find : t -> string -> Var.t option

val import : t -> Module.t -> t

val add : t -> Var.t -> t

val merge : t -> Var.Map.t -> t

val concat : t -> Var.Map.t

val debug : t -> unit
