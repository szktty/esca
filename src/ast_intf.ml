type text = string Located.t

type op = op_desc Located.t

and op_desc = [
  | `Pos              (* "+" + integer *)
  | `Fpos             (* "+." + float *)
  | `Neg              (* "-" + integer *)
  | `Fneg             (* "-." + float *)
  | `Eq               (* "==" *)
  | `Ne               (* "!=" *)
  | `Lt               (* "<" *)
  | `Le               (* "<=" *)
  | `Gt               (* ">" *)
  | `Ge               (* ">=" *)
  | `List_add         (* "++" *)
  | `List_diff        (* "--" *)
  | `Add              (* "+" *)
  | `Fadd             (* "+." *)
  | `Sub              (* "-" *)
  | `Fsub             (* "-." *)
  | `Mul              (* "*" *)
  | `Fmul             (* "*." *)
  | `Div              (* "/" *)
  | `Fdiv             (* "/." *)
  | `Pow              (* "**" *)
  | `Mod              (* "%" *)
  | `And              (* "and" *)
  | `Or               (* "or" *)
  | `Xor              (* "xor" *)
  | `Land             (* "band" *)
  | `Lor              (* "bor" *)
  | `Lxor             (* "bxor" *)
  | `Lcomp            (* "<<" *)
  | `Rcomp            (* ">>" *)
  | `Lpipe            (* "<|" *)
  | `Rpipe            (* "|>" *)
]

type tyexp = tyexp_desc Located.t

and tyexp_desc =
  | Ty_var of text
  | Ty_namepath of text Namepath.t
  | Ty_app of tyexp * tyexp list
  | Ty_list of tyexp
  | Ty_tuple of tyexp list
  | Ty_option of tyexp

type t = [
  | `Nop of Location.t (* internal use *)
  | `Chunk of chunk
  | `Package of text
  | `Vardef of vardef
  | `Assign of assign
  | `Fundef of fundef
  | `Strdef of strdef
  | `Return of t list
  | `If of if_
  | `For of for_
  | `Switch of switch
  | `Block of t list
  | `Funcall of funcall
  | `Binexp of binexp
  | `Unexp of unexp
  | `Unwrap of t
  | `Directive of (text * t list)
  | `Var of var
  | `Index of index
  | `Unit of Location.t
  | `Bool of bool Located.t
  | `String of string Located.t
  | `Int of int Located.t
  | `Float of float Located.t
  | `List of t list
  | `Tuple of t list
  | `Range of (int Located.t * int Located.t)
  | `Struct of t struct_
  | `Enum of (text, t) enum
]

and chunk = {
  ch_attrs : t list;
  ch_stats : t list;
}

and exp = {
  exp : t;
  mutable exp_type : Type.t option;
}

and assign = {
  asg_var : t;
  asg_exp : t;
  mutable asg_type : Type.t option;
}

and vardef = {
  vdef_pub : bool;
  vdef_ptn : pattern;
  vdef_exp : t;
}

and fundef = {
  fdef_name : text;
  fdef_params : text list;
  fdef_block : t list;
  mutable fdef_type : Type.t option;
}

and strdef = {
  sdef_name : text;
  sdef_fields : sdef_field list;
  mutable sdef_type : Type.t option;
}

and sdef_field = {
  sdef_field_name : text;
  sdef_field_tyexp : tyexp;
  mutable sdef_field_type : Type.t option;
}

and var = {
  var_prefix : t option;
  var_name : text;
  mutable var_type : Type.t option;
}

and if_ = {
  if_actions : (t * t list) list;
  if_else : t list;
  mutable if_type : Type.t option;
}

and for_ = {
  for_var : text;
  for_range : t;
  for_block : t list;
}

and switch = {
  sw_val : t;
  sw_cls : switch_cls list;
  mutable sw_val_type : Type.t option;
  mutable sw_cls_type : Type.t option;
}

and switch_cls = {
  sw_cls_var : text option;
  sw_cls_ptn : pattern;
  sw_cls_guard : t option;
  sw_cls_action : t list;
  mutable sw_cls_act_type : Type.t option;
}

and funcall = {
  fc_fun : t;
  fc_args : t list;
  mutable fc_fun_type : Type.t option;
  mutable fc_arg_types : Type.t list option;
}

and index = {
  idx_prefix : t;
  idx_index : t;
  mutable idx_type : Type.t option;
}

and 'a struct_ = {
  str_namepath : text Namepath.t;
  str_fields : (text * 'a option) list;
  mutable str_type : Type.t option;
}

and ('name, 'value) enum = {
  enum_name : 'name;
  enum_params : ('name option * 'value);
  mutable enum_type : Type.t option;
}

and binexp = {
  binexp_left : t;
  binexp_op : op;
  binexp_right : t;
  mutable binexp_type : Type.t option;
}

and unexp = {
  unexp_op : op;
  unexp_exp : t;
  mutable unexp_type : Type.t option;
}

and pattern = {
  ptn_cls : ptn_cls;
  mutable ptn_type : Type.t option;
}

and ptn_cls = [
  | `Nop of Location.t (* internal use *)
  | `Unit of Location.t
  | `Bool of bool Located.t
  | `String of string Located.t
  | `Int of int Located.t
  | `Float of float Located.t
  | `Variant of (text * pattern list)
  | `Cons of (pattern * text)
  | `List of pattern list
  | `Tuple of pattern list
  | `Enum of (text, pattern) enum
  | `Var of text
  | `Pin of text
]

let nop : t = `Nop Location.zero

let ptn_nop : pattern =
  { ptn_cls = `Nop Location.zero; ptn_type = None }