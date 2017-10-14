open Core.Std

include Ast_intf

let op_to_string op =
  match Located.(op.desc) with
  | `Pos -> "|+|"
  | `Neg -> "|-|"
  | `Eq -> "=="
  | `Ne -> "!="
  | `Lt -> "<"
  | `Le -> "<="
  | `Gt -> ">"
  | `Ge -> ">="
  | `Add -> "+"
  | `Sub -> "-"
  | `Mul -> "*"
  | `Div -> "/"
  | `Pow -> "**"
  | `Mod -> "%"
  | _ -> failwith "not supported operator"

let location = function
  | _ -> Location.zero (* TODO *)

let write_list (buf:Buffer.t) es ~f =
  let open Buffer in
  add_string buf "[";
  ignore @@ List.fold_left es
    ~f:(fun rest e ->
        f e;
        if rest > 0 then add_string buf " ";
        rest - 1)
    ~init:((List.length es) - 1);
  add_string buf "]"

let rec write (buf:Buffer.t) (node:Ast_intf.t) =
  let open Located in
  let open Buffer in
  let open Printf in
  let add_string = add_string buf in
  let add_space () = add_string " " in
  let add_lp () = add_string "(" in
  let add_rp () = add_string ")" in
  let write = write buf in
  let write_op op = add_string @@ op_to_string op in
  let write_nodes es = write_list buf es ~f:write in
  let write_text text = add_string ("\"" ^ text.desc ^ "\"") in
  let write_texts es = write_list buf es ~f:write_text in

  let write_fun_body buf (body:fun_body) =
    add_lp ();
    write_texts body.fbody_params;
    add_space ();
    Option.iter body.fbody_ret
      ~f:(fun ty ->
          write_tyexp buf ty;
          add_space ());
    write_nodes body.fbody_block;
    add_rp ()
  in

  match node with
  | `Nop _ -> add_string "nop"
  | `Chunk chunk ->
    add_string "(chunk ";
    write_nodes chunk.ch_attrs;
    write_nodes chunk.ch_stats;
    add_string ")"
  | `Package name ->
    add_string "(package ";
    write_text name;
    add_string ")"
  | `Vardef vdef ->
    add_string "(vardef ";
    write_ptn buf vdef.vdef_ptn;
    add_space ();
    write vdef.vdef_exp;
    add_string ")"
  | `Fundef fdef ->
    add_string "(fundef ";
    add_string fdef.fdef_name.desc;
    add_space ();
    write_texts fdef.fdef_params;
    add_space ();
    Option.iter fdef.fdef_ret
      ~f:(fun ty ->
          write_tyexp buf ty;
          add_space ());
    write_nodes fdef.fdef_block;
    add_string ")"
  | `Strdef sdef ->
    add_string "(strdef ";
    write_text sdef.sdef_name;
    add_string " {";
    List.iter sdef.sdef_fields ~f:(fun fld ->
        write_text fld.sdef_field_name;
        add_string ":";
        write_tyexp buf fld.sdef_field_tyexp;
        add_string ", ");
    add_string "})"
  | `Assign assign ->
    add_string "(assign ";
    write assign.asg_var;
    add_space ();
    write assign.asg_exp;
    add_string ")"
  | `Switch sw ->
    add_string "(switch ";
    write sw.sw_val;
    add_string " [";
    write_list buf sw.sw_cls
      ~f:(fun cls ->
          add_lp ();
          write_ptn buf cls.sw_cls_ptn;
          add_space ();
          begin match cls.sw_cls_guard with
            | None -> add_string "true"
            | Some guard -> write guard
          end;
          add_space ();
          write_nodes cls.sw_cls_action;
          add_rp ());
    add_string "]";
    add_rp ()
  | `Return exp ->
    add_string "(return ";
    Option.iter exp ~f:write;
    add_rp ()
  | `If if_ ->
    add_string "(if [";
    write_list buf if_.if_actions
      ~f:(fun (cond, action) ->
          add_lp ();
          write cond;
          add_space ();
          write_nodes action;
          add_rp ());
    add_string "] ";
    write_nodes if_.if_else;
    add_rp ()
  | `For for_ ->
    add_string "(for ";
    add_string for_.for_var.desc;
    add_space ();
    write for_.for_range;
    add_space ();
    write_nodes for_.for_block;
    add_rp ()
  | `Funcall fc ->
    add_string "(funcall ";
    write fc.fc_fun;
    add_space ();
    write_nodes fc.fc_args;
    add_string ")"
  | `Unexp exp ->
    add_string "(";
    write_op exp.unexp_op;
    add_space ();
    write exp.unexp_exp;
    add_string ")"
  | `Binexp exp ->
    add_string "(";
    write_op exp.binexp_op;
    add_space ();
    write exp.binexp_left;
    add_space ();
    write exp.binexp_right;
    add_rp ()
  | `Unwrap exp ->
    add_string "(unwrap ";
    write exp;
    add_rp()
  | `Directive (name, args) ->
    add_string "(directive ";
    add_string name.desc;
    add_space ();
    write_nodes args;
    add_rp ()
  | `Var var ->
    add_string "(var ";
    Option.iter var.var_prefix ~f:(fun node ->
        write node;
        add_string " ");
    add_string "\"";
    add_string var.var_name.desc;
    add_string "\"";
    add_rp ()
  | `Index idx ->
    add_string "(index ";
    write idx.idx_prefix;
    add_space ();
    write idx.idx_index;
    add_rp ()
  | `Void _ -> add_string "()"
  | `Bool { desc = true } -> add_string "true"
  | `Bool { desc = false } -> add_string "false"
  | `Int v -> add_string @@ sprintf "(int %d)" v.desc
  | `Float v -> add_string @@ sprintf "(float %f)" v.desc
  | `String s -> add_string @@ sprintf "\"%s\"" s.desc
  | `List exps ->
    add_string "(list ";
    write_nodes exps;
    add_rp ()
  | `Tuple exps ->
    add_string "(tuple ";
    write_nodes exps;
    add_rp ()
  | `Range range ->
    add_string "(range ";
    write range.range_begin;
    add_string @@ (match range.range_kind with
        | `Half_open -> "..<"
        | `Closed -> "...");
    write range.range_end;
    add_rp ()
  | `Struct str ->
    add_string "{";
    Namepath.iter str.str_namepath ~f:(fun name ->
        write_text name;
        add_string ".");
    add_string ": ";
    List.iter str.str_fields ~f:(fun (name, v_opt) ->
        write_text name;
        Option.iter v_opt ~f:(fun v ->
            add_string "=";
            write v);
        add_string ", ");
    add_string "}"
  | `Fun fn ->
    add_string "(fun ";
    write_fun_body buf fn.fun_body;
    add_string ")"
  | _ -> failwith "not supported"

and write_ptn buf ptn =
  let open Located in
  let open Buffer in
  let open Printf in
  let add_string = add_string buf in
  let add_space () = add_string " " in
  let add_lp () = add_string "(" in
  let add_rp () = add_string ")" in
  let write = write_ptn buf in
  let write_ptns es = write_list buf es ~f:write in
  let write_texts es = write_list buf es ~f:(fun e -> add_string e.desc) in
  match ptn.ptn_cls with
  | `Bool { desc = true } -> add_string "true"
  | `Bool { desc = false } -> add_string "false"
  | `Int v -> add_string @@ sprintf "%d" v.desc
  | `Float v -> add_string @@ sprintf "%f" v.desc
  | `String s -> add_string @@ sprintf "\"%s\"" s.desc
  | `List ptns ->
    add_string "[";
    write_ptns ptns;
    add_string "]"
  | `Tuple ptns ->
    add_string "(tuple ";
    write_ptns ptns;
    add_rp ()
  | `Var name ->
    add_string @@ "(var " ^ name.desc ^ ")"
  | _ -> failwith "not supported pattern"

and write_tyexp (buf:Buffer.t) tyexp =
  let open Located in
  let open Buffer in

  let write_list es =
    List.iter es ~f:(fun arg ->
        write_tyexp buf arg;
        add_string buf ", ")
  in

  match tyexp.desc with
  | Ty_var name ->
    add_string buf ("'" ^ name.desc)
  | Ty_namepath path ->
    Namepath.iter path ~f:(fun name -> add_string buf name.desc)
  | Ty_app (tycon, args) ->
    write_tyexp buf tycon;
    add_string buf "<";
    write_list args;
    add_string buf ">"
  | Ty_list e ->
    add_string buf "[";
    write_tyexp buf e;
    add_string buf "]"
  | Ty_tuple es ->
    add_string buf "(";
    write_list es;
    add_string buf ")"
  | Ty_option e ->
    write_tyexp buf e;
    add_string buf "?"

let to_string node =
  let buf = Buffer.create 16 in
  write buf node;
  Buffer.contents buf

let tyexp_to_string node =
  let buf = Buffer.create 16 in
  write_tyexp buf node;
  Buffer.contents buf

let print node =
  Printf.printf "%s\n" (to_string node)

let print_tyexp node =
  Printf.printf "%s\n" (tyexp_to_string node)

let rec type_ (node:t) =
  match node with
  | `Funcall call ->
    Some (Type.fun_return call.fc_fun_type)
  | `Binexp exp ->
    begin match exp.binexp_op.desc with
      | `Eq | `Ne | `Lt | `Le | `Gt | `Ge -> Some Type.bool
      | _ -> type_ exp.binexp_left
    end
  | `Var var -> Some (Option.value_exn var.var_value).type_
  | `Bool _ -> Some Type.bool
  | `Int _ -> Some Type.int
  | `String _ -> Some Type.string
  | _ -> failwith "not impl"

let type_exn node =
  Option.value_exn (type_ node)
