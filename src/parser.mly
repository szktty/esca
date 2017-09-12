%{

open Ast
open Located

let create_binexp left op_loc op right =
  let op = create (Some op_loc) op in
  `Binexp {
        binexp_left = left;
        binexp_op = op;
        binexp_right = right;
        binexp_type = None }

let create_unexp op_loc op exp =
  let op = create (Some op_loc) op in
  `Unexp { unexp_op = op; unexp_exp = exp; unexp_type = None }

let create_exp exp =
  { exp = exp; exp_type = None }

%}

%token <Ast.text> IDENT
%token <Ast.text> CHAR
%token <Ast.text> STRING
%token <int Located.t> INT
%token <float Located.t> FLOAT
%token <Location.t> TRUE
%token <Location.t> FALSE
%token <Location.t> LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token LBRACE
%token RBRACE
%token COMMA                        (* "," *)
%token DOT                          (* "." *)
%token DOT2                         (* ".." *)
%token COLON                        (* ":" *)
%token COLON2                       (* "::" *)
%token SEMI                         (* ";" *)
%token LARROW                       (* "<-" *)
%token RARROW                       (* "->" *)
%token BAR                          (* "|" *)
%token CARET                        (* "^" *)
%token Q                            (* "?" *)
%token BANG                         (* "!" *)
%token NSIGN                        (* "#" *)
%token SQUOTE                       (* "'" *)
%token <Location.t> AT              (* "@" *)
%token <Location.t> AMP             (* "&" *)
%token <Location.t> LPIPE           (* "<|" *)
%token <Location.t> RPIPE           (* "|>" *)
%token <Location.t> EQ              (* "=" *)
%token <Location.t> EQQ             (* "==" *)
%token <Location.t> NE              (* "!=" *)
%token <Location.t> LT              (* "<" *)
%token <Location.t> LE              (* "<=" *)
%token <Location.t> GT              (* ">" *)
%token <Location.t> GE              (* ">=" *)
%token <Location.t> PLUS            (* "+" *)
%token <Location.t> FPLUS           (* "+." *)
%token <Location.t> MINUS           (* "-" *)
%token <Location.t> FMINUS          (* "-." *)
%token <Location.t> AST             (* "*" *)
%token <Location.t> FAST            (* "*." *)
%token <Location.t> AST2            (* "**" *)
%token <Location.t> SLASH           (* "/" *)
%token <Location.t> FSLASH          (* "/." *)
%token <Location.t> PCT             (* "%" *)
%token <Location.t> POS             (* for positive integer *)
%token <Location.t> FPOS            (* for positive float *)
%token <Location.t> NEG             (* for negative integer *)
%token <Location.t> FNEG            (* for negative float *)
%token <Location.t> AND             (* "and" *)
%token <Location.t> OR              (* "or" *)
%token CASE                         (* "case" *)
%token ELSE                         (* "else" *)
%token ELSEIF                       (* "elseif" *)
%token ENUM                         (* "enum" *)
%token FOR                          (* "for" *)
%token FUNC                         (* "func" *)
%token IF                           (* "if" *)
%token IN                           (* "in" *)
%token LAND                         (* "land" *)
%token LET                          (* "let" *)
%token LOR                          (* "lor" *)
%token LSL                          (* "lsl" *)
%token LSR                          (* "lsr" *)
%token LXOR                         (* "lxor" *)
%token MODULE                       (* "module" *)
%token PACKAGE                      (* "package" *)
%token PUBLIC                       (* "public" *)
%token RETURN                       (* "return" *)
%token STRUCT                       (* "struct" *)
%token SWITCH                       (* "switch" *)
%token WHEN                         (* "when" *)
%token HASH_NEW                     (* "#new" *)
%token EOF

%left OR AND
%right EQ LARROW
%nonassoc NE EQQ
%left LT GT LE GE
%left RPIPE LPIPE
%left PLUS FPLUS MINUS FMINUS
%left AST FAST SLASH FSLASH PCT
%right AST2

%nonassoc trail
%nonassoc LBRACE

%nonassoc app
%nonassoc LPAREN

%start <Ast.t> prog

%%

prog:
  | chunk EOF { $1 }

chunk:
  | top_stat_list { `Chunk { ch_attrs = []; ch_stats = $1 } }
  | attr_list top_stat_list { `Chunk { ch_attrs = $1; ch_stats = $1 } }

attr_list:
  | rev_attr_list { Core.Std.List.rev $1 }
  | rev_attr_list SEMI { Core.Std.List.rev $1 }

rev_attr_list:
  | attr { [$1] }
  | rev_attr_list attr { $2 :: $1 }
  | rev_attr_list SEMI attr { $3 :: $1 }

attr:
  | PACKAGE IDENT { `Package $2 }

top_stat_list:
  | rev_top_stat_list { Core.Std.List.rev $1 }
  | rev_top_stat_list SEMI { Core.Std.List.rev $1 }

rev_top_stat_list:
  | top_stat { [$1] }
  | rev_top_stat_list top_stat { $2 :: $1 }
  | rev_top_stat_list SEMI top_stat { $3 :: $1 }

top_stat:
  | directive { $1 }
  | module_def { $1 }
  | struct_def { $1 }
  | enum_def { $1 }
  | vardef { $1 }
  | fundef { $1 }

block:
  | (* empty *) { [] }
  | last_stat { [$1] }
  | last_stat SEMI { [$1] }
  | stat_list { $1 }
  | stat_list last_stat { List.rev ($2 :: $1) }
  | stat_list last_stat SEMI { List.rev ($2 :: $1) }

stat_list:
  | rev_stat_list { Core.Std.List.rev $1 }
  | rev_stat_list SEMI { Core.Std.List.rev $1 }

rev_stat_list:
  | stat { [$1] }
  | rev_stat_list stat { $2 :: $1 }
  | rev_stat_list SEMI stat { $3 :: $1 }

stat:
  | vardef { $1 }
  | fundef { $1 }
  | var EQ exp (* TODO: exp_list *)
  { `Assign { asg_var = $1; asg_exp = $3; asg_type = None } }
  | LBRACE block RBRACE { `Block $2 }
  | FOR IDENT IN exp LBRACE block RBRACE
  { `For { for_var = $2; for_range = $4; for_block = $6 } }
  | if_stat { $1 }
  | switch_stat { $1 }
  | funcall %prec app { $1 }

last_stat:
  | RETURN { `Return [] }
  | RETURN exp_list { `Return $2 }

exp:
  | fun_exp { $1 }
  | bin_exp { $1 }
  | unary_exp { $1 }
  | simple_exp { $1 }

exp_list:
  | rev_exp_list { Core.Std.List.rev $1 }

rev_exp_list:
  | exp { [$1] }
  | rev_exp_list COMMA exp { $3 :: $1 }

module_def:
  | MODULE LBRACE RBRACE { Ast.nop }
  | MODULE LBRACE stat_list RBRACE { Ast.nop }

struct_def:
  | STRUCT IDENT LBRACE field_def_list RBRACE
  { `Strdef { sdef_name = $2;
      sdef_fields = $4;
      sdef_type = None;
    }
  }

field_def_list:
  | rev_field_def_list { Core.Std.List.rev $1 }

rev_field_def_list:
  | field_def { [$1] }
  | rev_field_def_list field_def { $2 :: $1 }
  | rev_field_def_list COMMA field_def { $3 :: $1 }

field_def:
  | IDENT COLON type_exp
  { { sdef_field_name = $1;
      sdef_field_tyexp = $3;
      sdef_field_type = None;
    }
  }

enum_def:
  | ENUM IDENT LBRACE variant_def_list RBRACE { Ast.nop }

variant_def_list:
  | rev_variant_def_list { Core.Std.List.rev $1 }

rev_variant_def_list:
  | variant_def { [$1] }
  | rev_variant_def_list variant_def { $2 :: $1 }
  | rev_variant_def_list COMMA variant_def { $3 :: $1 }

variant_def:
  | IDENT { Ast.nop }
  | IDENT LPAREN variant_param_list RPAREN { Ast.nop }

variant_param_list:
  | rev_variant_param_list { Core.Std.List.rev $1 }

rev_variant_param_list:
  | type_exp { [$1] }
  | rev_variant_param_list COMMA type_exp { $3 :: $1 }

  vardef:
  | LET pattern EQ exp
  { `Vardef { vdef_pub = false; vdef_ptn = $2; vdef_exp = $4 } }
  | PUBLIC LET pattern EQ exp
  { `Vardef { vdef_pub = true; vdef_ptn = $3; vdef_exp = $5 } }

fundef:
  | FUNC var_name param_list EQ exp
  {
    `Fundef {
        fdef_name = $2;
        fdef_params = $3;
        fdef_block = [$5];
        fdef_type = None;
    }
  }
  | FUNC var_name param_list LBRACE block RBRACE
  {
    `Fundef {
        fdef_name = $2;
        fdef_params = $3;
        fdef_block = $5;
        fdef_type = None;
    }
  }

param_list:
  | LPAREN RPAREN { [] }
  | LPAREN param_list_body RPAREN { $2 }

param_list_body:
  | rev_param_list { Core.Std.List.rev $1 }

rev_param_list:
  | param { [$1] }
  | rev_param_list COMMA param { $3 :: $1 }

param:
  | IDENT { $1 }
  | IDENT COLON type_exp { $1 } (* TODO *)

if_stat:
  | IF exp LBRACE block RBRACE
  {
      `If {
          if_actions = [($2, $4)];
          if_else = [];
          if_type = None }
  }
  | IF exp LBRACE block elseif_block RBRACE
  {
      `If {
          if_actions = ($2, $4) :: $5;
          if_else = [];
          if_type = None }
  }
  | IF exp LBRACE block ELSE block RBRACE
  {
      `If {
          if_actions = [($2, $4)];
          if_else = $6;
          if_type = None }
  }
  | IF exp LBRACE block elseif_block ELSE block RBRACE
  {
      `If {
          if_actions = ($2, $4) :: $5;
          if_else = $7;
          if_type = None }
  }

elseif_block:
  | rev_elseif_block { Core.Std.List.rev $1 }

rev_elseif_block:
  | elseif_stat { [$1] }
  | rev_elseif_block elseif_stat { $2 :: $1 }

elseif_stat:
  | ELSEIF exp LBRACE block RBRACE { ($2, $4) }

switch_stat:
  | SWITCH exp LBRACE sw_clause_list RBRACE
  { `Switch {
        sw_val = $2;
        sw_cls = $4;
        sw_val_type = None;
        sw_cls_type = None; }
  }

sw_clause_list:
  | rev_sw_clause_list { Core.Std.List.rev $1 }

rev_sw_clause_list:
  | sw_clause { [$1] }
  | rev_sw_clause_list sw_clause { $2 :: $1 }

sw_clause:
  | CASE sw_pattern COLON block
  { { Ast.sw_cls_var = None;
      sw_cls_ptn = fst $2;
      sw_cls_guard = snd $2;
      sw_cls_action = $4;
      sw_cls_act_type = None } }
  | CASE IDENT EQ sw_pattern COLON block
  { { Ast.sw_cls_var = Some $2;
      sw_cls_ptn = fst $4;
      sw_cls_guard = snd $4;
      sw_cls_action = $6;
      sw_cls_act_type = None } }

sw_pattern:
  | pattern { ($1, None) }
  | pattern WHEN exp { ($1, Some $3) }

funcall:
  | var_name fun_exp
  { nop }
  | prefix_exp paren_arg_list
  {
    `Funcall { fc_fun = $1; fc_args = $2; fc_fun_type = None; fc_arg_types = None }
  }
  | LPAREN exp RPAREN paren_arg_list
  {
    `Funcall { fc_fun = $2; fc_args = $4; fc_fun_type = None; fc_arg_types = None }
  }
  | LPAREN exp COLON type_exp RPAREN paren_arg_list
  {
    (* TODO *)
    `Funcall { fc_fun = $2; fc_args = $6; fc_fun_type = None; fc_arg_types = None }
  }
  | funcall paren_arg_list
  {
    `Funcall { fc_fun = $1; fc_args = $2; fc_fun_type = None; fc_arg_types = None }
  }

paren_arg_list:
  | LPAREN RPAREN %prec trail { [] }
  | LPAREN RPAREN fun_exp { [] }
  | LPAREN arg_list RPAREN %prec trail { $2 }
  | LPAREN arg_list RPAREN fun_exp { $2 }

arg_list:
  | rev_arg_list { Core.Std.List.rev $1 }

rev_arg_list:
  | exp { [$1] }
  | rev_arg_list COMMA exp { $3 :: $1 }

fun_exp:
  | LBRACE block RBRACE { Ast.nop }
  | LBRACE BAR BAR block RBRACE { Ast.nop }
  | LBRACE BAR param_list_body BAR block RBRACE { Ast.nop }

bin_exp:
  | exp PLUS exp { create_binexp $1 $2 `Add $3 }
  | exp FPLUS exp { create_binexp $1 $2 `Fadd $3 }
  | exp MINUS exp { create_binexp $1 $2 `Sub $3 }
  | exp FMINUS exp { create_binexp $1 $2 `Fsub $3 }
  | exp AST exp { create_binexp $1 $2 `Mul $3 }
  | exp FAST exp { create_binexp $1 $2 `Fmul $3 }
  | exp AST2 exp { create_binexp $1 $2 `Pow $3 }
  | exp SLASH exp { create_binexp $1 $2 `Div $3 }
  | exp FSLASH exp { create_binexp $1 $2 `Fdiv $3 }
  | exp PCT exp { create_binexp $1 $2 `Mod $3 }
  | exp EQQ exp { create_binexp $1 $2 `Eq $3 }
  | exp NE exp { create_binexp $1 $2 `Ne $3 }
  | exp LT exp { create_binexp $1 $2 `Lt $3 }
  | exp LE exp { create_binexp $1 $2 `Le $3 }
  | exp GT exp { create_binexp $1 $2 `Gt $3 }
  | exp GE exp { create_binexp $1 $2 `Ge $3 }
  | exp AND exp { create_binexp $1 $2 `And $3 }
  | exp OR exp { create_binexp $1 $2 `Or $3 }
  | exp LPIPE exp { create_binexp $1 $2 `Lpipe $3 }
  | exp RPIPE exp { create_binexp $1 $2 `Rpipe $3 }

unary_exp:
  | LPAREN unary_body RPAREN { $2 }

unary_body:
  | PLUS simple_exp { create_unexp $1 `Pos $2 }
  | POS simple_exp { create_unexp $1 `Pos $2 }
  | FPOS simple_exp { create_unexp $1 `Fpos $2 }
  | MINUS simple_exp { create_unexp $1 `Neg $2 }
  | NEG simple_exp { create_unexp $1 `Neg $2 }
  | FNEG simple_exp { create_unexp $1 `Fneg $2 }

simple_exp:
  | prefix_exp %prec app { $1 }
  | literal { $1 }

prefix_exp:
  | var { $1 }
  | funcall %prec app { $1 }
  | LPAREN exp RPAREN %prec app { $2 }
  | LPAREN exp COLON type_exp RPAREN %prec app { $2 } (* TODO *)
  | LPAREN if_stat RPAREN { $2 }
  | LPAREN switch_stat RPAREN { $2 }
  | prefix_exp BANG { `Unwrap $1 }

directive:
  | AT IDENT paren_arg_list { `Directive ($2, $3) }

var:
  | var_name %prec trail
  { `Var {
        var_prefix = None;
        var_name = $1;
        var_type = None }
  }
  | prefix_exp DOT var_name
  { `Var {
        var_prefix = Some $1;
        var_name = $3;
        var_type = None }
  }
  | prefix_exp LBRACK exp RBRACK
  { `Index { idx_prefix = $1; idx_index = $3; idx_type = None } }

var_name:
  | IDENT { $1 }
  | IDENT Q { create $1.loc ($1.desc ^ "?") }

literal:
  | LPAREN RPAREN { `Unit $1 }
  | STRING { `String $1 }
  | INT { `Int $1 }
  | FLOAT { `Float $1 }
  | TRUE { `Bool (locate $1 true) }
  | FALSE { `Bool (locate $1 false) }
  | list_ { `List $1 }
  | tuple { `Tuple $1 }
  | INT DOT2 INT { `Range ($1, $3) }
  | struct_ { $1 }

list_:
  | LBRACK RBRACK { [] }
  | LBRACK elts RBRACK { $2 }

elts:
  | rev_elts { Core.Std.List.rev $1 }

rev_elts:
  | exp { [$1] }
  | rev_elts COMMA exp { $3 :: $1 }

tuple:
  | LPAREN exp COMMA rev_elts RPAREN { $2 :: (Core.Std.List.rev $4) }

struct_:
  | HASH_NEW namepath LBRACE key_value_pairs RBRACE
  { `Struct {
      str_namepath = $2;
      str_fields = $4;
      str_type = None }
  }

key_value_pairs:
  | rev_key_value_pairs { Core.Std.List.rev $1 }

rev_key_value_pairs:
  | key_value_pair { [$1] }
  | rev_key_value_pairs COMMA key_value_pair { $3 :: $1 }

key_value_pair:
  | IDENT { ($1, None) }
  | IDENT COLON exp { ($1, Some $3) }

pattern:
  | LPAREN pattern RPAREN { $2 }
  | pattern_clause { { ptn_cls = $1; ptn_type = None } }

pattern_clause:
  | LPAREN RPAREN { `Unit $1 }
  | STRING { `String $1 }
  | INT { `Int $1 }
  | FLOAT { `Float $1 }
  | TRUE { `Bool (locate $1 true) }
  | FALSE { `Bool (locate $1 false) }
  | IDENT { `Var $1 }
  | DOT IDENT { `Variant ($2, [])  }
  | DOT IDENT LPAREN elts_ptn RPAREN { `Variant ($2, $4)  }
  | CARET IDENT { `Pin $2 }
  | pattern COLON2 IDENT { `Cons ($1, $3) }
  | list_ptn { `List $1 }
  | tuple_ptn { `Tuple $1 }

list_ptn:
  | LBRACK RBRACK { [] }
  | LBRACK elts_ptn RBRACK { $2 }

elts_ptn:
  | rev_elts_ptn { Core.Std.List.rev $1 }

rev_elts_ptn:
  | pattern { [$1] }
  | rev_elts_ptn COMMA pattern { $3 :: $1 }

tuple_ptn:
  | LPAREN pattern COMMA rev_elts_ptn RPAREN { Core.Std.List.rev ($2 :: $4) }

type_exp:
  | simple_type_exp { $1 }
  | simple_type_exp Q { less @@ Ty_option $1 }
  | simple_type_exp LT type_stat_list GT
  { less @@ Ty_app ($1, $3) }
  | simple_type_exp LT type_stat_list GT Q
  { less @@ Ty_option (less @@ Ty_app ($1, $3)) }

type_stat_list:
  | rev_type_stat_list { Core.Std.List.rev $1 }

rev_type_stat_list:
  | type_exp { [$1] }
  | rev_type_stat_list COMMA type_exp { $3 :: $1 }

simple_type_exp:
  | LPAREN type_exp RPAREN { $2 }
  | SQUOTE IDENT { less @@ Ty_var $2 }
  | namepath { less @@ Ty_namepath $1  }
  | LBRACK type_exp RBRACK { less @@ Ty_list $2 }
  | LPAREN type_exp COMMA type_stat_list RPAREN
  { less @@ Ty_tuple ($2 :: $4) }

namepath:
  | IDENT { Namepath.create $1 }
  | namepath DOT IDENT { Namepath.create $3 ~prefix:(Some $1) }
