type unity_exn = {
  uniexn_ex : Type.t;
  uniexn_ac : Type.t;
}

type mismatch = {
  mismatch_node: Ast.t;
  mismatch_ex : Type.t;
  mismatch_ac : Type.t;
}

exception Unify_error of unity_exn

exception Type_mismatch of mismatch

exception Deref_error of Type.t * string

val run : Module.t -> Ast.t -> Module.t
