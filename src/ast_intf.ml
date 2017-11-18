type text = string Located.t

type op = op_desc Located.t

and op_desc = [
  | `Pos              (* "+" + integer *)
  | `Neg              (* "-" + integer *)
  | `Eq               (* "==" *)
  | `Ne               (* "!=" *)
  | `Lt               (* "<" *)
  | `Le               (* "<=" *)
  | `Gt               (* ">" *)
  | `Ge               (* ">=" *)
  | `List_add         (* "++" *)
  | `List_diff        (* "--" *)
  | `Add              (* "+" *)
  | `Sub              (* "-" *)
  | `Mul              (* "*" *)
  | `Div              (* "/" *)
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
  | Ty_ref of tyexp

type t = [
  | `Nop of Location.t (* internal use *)
  | `Chunk of chunk
  | `Import_attr of text * text
  | `Go_attr of text
  | `Extension of extension
  | `Vardef of vardef
  | `Assign of assign
  | `Fundef of fundef
  | `Fundecl of fundecl
  | `Strdef of strdef
  | `Sdef_field of sdef_field
  | `Return of t option
  | `Break
  | `If of if_
  | `For of for_
  | `Switch of switch
  | `Tyswitch of type_switch
  | `Block of t list
  | `Funcall of funcall
  | `Binexp of binexp
  | `Unexp of unexp
  | `Unwrap of t
  | `Directive of (text * t list)
  | `Var of var
  | `Index of index
  | `Void of Location.t
  | `Bool of bool Located.t
  | `String of string Located.t
  | `Int of int Located.t
  | `Float of float Located.t
  | `List of t list
  | `Tuple of t list
  | `Range of range
  | `Ref of t * Type.t
  | `Struct of t struct_
  | `Enum of (text, t) enum
  | `Fun of func
]

and chunk = {
  ch_attrs : t list;
  ch_stats : t list;
}

and extension = {
  ext_tyexp : tyexp;
  ext_items : t list;
}

and exp = {
  exp : t;
  exp_type : Type.t;
}

and assign = {
  asg_var : t;
  asg_exp : t;
  asg_type : Type.t;
}

and vardef = {
  vdef_pub : bool;
  vdef_ptn : pattern;
  vdef_exp : t;
}

and fundef_prefix = [`Public | `Tailrec]

and fundef = {
  fdef_name : text;
  fdef_params : text list;
  fdef_ret : tyexp option;
  fdef_block : t list;
  fdef_type : Type.t;
}

and fun_body = {
  fbody_params : text list;
  fbody_ret : tyexp option;
  fbody_block : t list;
  fbody_type : Type.t;
}

and fundecl = {
  fdecl_attrs : t list;
  fdecl_name : text;
  fdecl_params : text list;
  fdecl_ret : tyexp option;
  fdecl_type : Type.t;
}

and strdef = {
  sdef_quals : [`Extern] list;
  sdef_name : text;
  sdef_items : t list;
  sdef_type : Type.t;
}

and sdef_field = {
  sdef_field_attrs : t list;
  sdef_field_mut : [`Var | `Let];
  sdef_field_name : text;
  sdef_field_tyexp : tyexp;
  sdef_field_type : Type.t;
}

and var = {
  var_prefix : t option;
  var_name : text;
  var_type : Type.t;
  mutable var_value : Value.t option;
}

and if_ = {
  if_actions : (t * t list) list;
  if_else : t list;
  if_type : Type.t;
}

and for_ = {
  for_var : text;
  for_range : t;
  for_block : t list;
}

and switch = {
  sw_val : t;
  sw_cls : switch_cls list;
  sw_default : t list option;
  sw_val_type : Type.t;
  sw_cls_type : Type.t;
}

and switch_cls = {
  sw_cls_var : text option;
  sw_cls_ptn : pattern;
  sw_cls_guard : t option;
  sw_cls_action : t list;
  sw_cls_act_type : Type.t;
}

and type_switch = {
  tysw_val : t;
  tysw_cls : type_switch_cls list;
  tysw_default : t list option;
  tysw_val_type : Type.t;
  tysw_cls_type : Type.t;
}

and type_switch_cls = {
  tysw_cls_var : text option;
  tysw_cls_ptn : tyexp;
  tysw_cls_guard : t option;
  tysw_cls_action : t list;
  tysw_cls_act_type : Type.t;
}

and funcall = {
  fc_fun : t;
  fc_args : t list;
  fc_fun_type : Type.t;
}

and index = {
  idx_prefix : t;
  idx_index : t;
  idx_type : Type.t;
}

and range = {
  range_begin : t;
  range_end : t;
  range_kind : [`Half_open | `Closed];
}

and 'a struct_ = {
  str_namepath : text Namepath.t;
  str_fields : (text * 'a option) list;
  str_type : Type.t;
}

and ('name, 'value) enum = {
  enum_name : 'name;
  enum_params : ('name option * 'value);
  enum_type : Type.t;
}

and func = {
  fun_body : fun_body;
}

and binexp = {
  binexp_left : t;
  binexp_op : op;
  binexp_right : t;
  binexp_type : Type.t;
}

and unexp = {
  unexp_op : op;
  unexp_exp : t;
  unexp_type : Type.t;
}

and pattern = {
  ptn_cls : ptn_cls;
  ptn_type : Type.t;
}

and ptn_cls = [
  | `Nop of Location.t (* internal use *)
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
  { ptn_cls = `Nop Location.zero;
    ptn_type = Type.metavar_some Location.zero }
