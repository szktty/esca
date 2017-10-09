(* high-level intermediate representation *)

open Core.Std

module Id = struct

  type t = {
    name : string;
    ty : Type.t;
  }

end

module Binop = struct

  type t =
    | Eq
    | Ne
    | Lt
    | Le
    | Gt
    | Ge
    | Add
    | Sub
    | Mul
    | Div

end

module Op = struct

  type t =
    | Nop
    | Fundef of fundef
    | Vardef of vardef
    | Assign of assign
    | Switch of switch
    | If of if_
    | For of for_
    | Return of return option
    | Block of t list
    | Call of call
    | Methcall of method_call
    | Binexp of binexp
    | Var of Id.t
    | Ref of Value.t
    | Poly of poly
    | Ref_fun of Id.t
    | Ref_prop of ref_prop
    | Prim of primitive
    | Null
    | Bool of bool
    | String of string
    | Int of int
    | Float of float
    | List of t list
    | Range of range
    | Fun of closure

  and fundef = {
    fdef_ty : Type.t;
    fdef_name : string;
    fdef_params : Id.t list;
    fdef_block : t list;
  }

  and vardef = {
    vdef_var : Id.t;
    vdef_ptn : pattern;
    vdef_exp : t;
  }

  and assign = {
    asn_var : t;
    asn_exp : t;
  }

  and switch = {
    sw_val : t;
    sw_clss : switch_clause list;
  }

  and switch_clause = {
    sw_cls_var : Id.t;
    sw_cls_ptn : pattern;
    sw_cls_guard : t option;
    sw_cls_action : t list;
  }

  and pattern =
    | Ptn_nop
    | Ptn_bool of bool
    | Ptn_string of string
    | Ptn_int of int
    | Ptn_float of float
    | Ptn_list of pattern list
    | Ptn_tuple of pattern list
    | Ptn_var of string

  and if_ = {
    if_actions : (t * t list) list;
    if_else : t list;
  }

  and for_ = {
    for_var : Id.t;
    for_range : t;
    for_block : t list;
  }

  and return = {
    ret_var : Id.t;
    ret_val : t;
  }

  and call = {
    call_fun : t;
    call_args : t list;
    call_ty : Type.t;
  }

  and method_call = {
    mcall_recv : t;
    mcall_name : string;
    mcall_args : t list;
    mcall_ty : Type.t;
  }

  and binexp = {
    binexp_left : t;
    binexp_op : Binop.t;
    binexp_right : t;
    binexp_ty : Type.t;
  }

  and block = {
    blk_vars : t list;
    blk_list : t list;
  }

  and primitive = {
    prim_id : string;
    prim_type : Type.t;
  }

  and poly = {
    poly_path : string Namepath.t;
    poly_type : Type.t
  }

  and ref_prop = {
    ref_prop_obj : t;
    ref_prop_name : string;
    ref_prop_ty : Type.t
  }

  and range = {
    range_begin : t;
    range_end : t;
    range_kind : [`Half_open | `Closed];
  }

  and closure = {
    clos_parent : closure option;
    clos_type : Type.t;
    clos_scope : [`Named of string | `Anon | `Main | `Init];
    clos_params : Id.t list;
    clos_block : t list;
  }

end

module Closure = struct

  type t = Op.closure

  let create ?parent ty ~scope ~params ~block =
    { Op.clos_parent = parent;
      clos_type = ty;
      clos_scope = scope;
      clos_params = params;
      clos_block = block }

  let of_fundef (def:Op.fundef) =
    create
      def.fdef_ty
      ~scope:(`Named def.fdef_name)
      ~params:def.fdef_params
      ~block: def.fdef_block

  let string_of_scope = function
    | `Main -> "main"
    | `Init -> "init"
    | `Named name -> Printf.sprintf "\"%s\"" name
    | `Anon -> "fun"

end

module Program = struct

  type t = {
    file : string;
    package : string option;
    funs : Closure.t list;
    used_mods : Module.t list;
  }

  let create ~file ~package ~funs ~used_mods =
    { file; package; funs; used_mods }

end

module Context = struct

  type t = {
    file : string;
    package : string option;
    stack : t list;
    id : int;
    vars : Id.t list;
    var_map : Id.t String.Map.t;
    mutable ret : Type.t;
    mutable used_mods : Module.t list;
    mutable prog : Program.t option;
  }

  let create file =
    { file;
      package = None;
      stack = [];
      id = 0;
      vars = [];
      var_map = String.Map.empty;
      ret = Type.void;
      used_mods = [];
      prog = None;
    }

  let create_sub ctx =
    { ctx with stack = ctx :: ctx.stack }

  let gen_id ctx =
    let id = ctx.id + 1 in
    { ctx with id = id }, Printf.sprintf "t%d" id

  let add_var ctx var =
    { ctx with vars = var :: ctx.vars }

  let new_var ?name ctx ty =
    let ctx, id = gen_id ctx in
    let var = { Id.name = id; ty = ty } in
    let ctx = match name with
      | None -> ctx
      | Some name -> { ctx with
                       var_map = String.Map.add ctx.var_map
                           ~key:name
                           ~data:var }
    in
    { ctx with vars = var :: ctx.vars }, var

  let new_ref ctx ty name =
    let ctx, var = new_var ctx ty in
    let ctx = { ctx with var_map = String.Map.add ctx.var_map
                             ~key:name
                             ~data:var } in
    ctx, var 

  let get_var ctx name =
    String.Map.find ctx.var_map name

  (* TODO: type *)
  let main_fun = { Id.name = "main"; ty = Type.void }

  let use_module ctx m =
    ctx.used_mods <- m :: ctx.used_mods

end

module Compiler = struct

  let rec compile_node (ctx:Context.t) (node:Ast.t) : Context.t * Op.t =
    let open Located in
    let open Op in
    let open Context in

    Ast.print node;
    match node with
    | `Chunk chunk ->
      let ctx, _ = compile_fold ctx chunk.ch_attrs in
      let ctx, funs =
        List.fold_left chunk.ch_stats
          ~init:(ctx, [])
          ~f:(fun (ctx, funs) node ->
              let ctx, op = compile_node ctx node in
              match op with
              | Fundef def ->
                let func = Closure.of_fundef def in
                ctx, func :: funs
              | _ ->
                Printf.printf "\n";
                Ast.print node;
                failwith "invalid node")
      in
      ctx.prog <- Some (Program.create
                          ~file:ctx.file
                          ~package:ctx.package
                          ~funs
                          ~used_mods:ctx.used_mods);
      ctx, Nop

    | `Package name ->
      { ctx with package = Some name.desc }, Nop

    | `Fundef def ->
      Printf.printf "HIR: compile fundef\n";
      Ast.print node;
      let fun_ctx = ctx in
      let fun_ctx, fun_var =
        match def.fdef_name.desc with
        | "main" -> fun_ctx, main_fun
        | _ -> new_var fun_ctx def.fdef_type
      in 

      (* parameters *)
      let param_tys = Type.fun_params def.fdef_type in
      let fun_ctx, rev_params =
        List.fold2_exn def.fdef_params param_tys
          ~init:(fun_ctx, [])
          ~f:(fun (ctx, vars) name ty ->
              let ctx, var = new_var ctx ty ~name:name.desc in
              ctx, var :: vars)
      in
      let params = List.rev rev_params in
      Printf.printf"HIR: fundef params = %d\n" (List.length params);

      let _, block = compile_fold fun_ctx def.fdef_block in
      let ctx' = add_var ctx fun_var in
      ctx', Fundef {
        fdef_ty = def.fdef_type;
        fdef_name = def.fdef_name.desc;
        fdef_params = params;
        fdef_block = block;
      }

    | `Vardef def ->
      let ctx, var = new_var ctx @@ Ast.type_exn def.vdef_exp in
      let exp_op = compile_node' ctx def.vdef_exp in
      let ctx, ptn_op = compile_ptn ctx def.vdef_ptn in
      let vdef = { vdef_var = var;
                   vdef_ptn = ptn_op;
                   vdef_exp = exp_op } in
      ctx, Vardef vdef

    | `Switch sw ->
      Printf.printf "HIR: compile switch\n";
      let val_ty = Ast.type_exn sw.sw_val in
      let val_op = compile_node' ctx sw.sw_val in
      let cls_ops = List.map sw.sw_cls
          ~f:(fun cls ->
              let ctx, var = new_var ctx val_ty in
              let ctx, ptn_op = compile_ptn ctx cls.sw_cls_ptn in
              let grd_op =
                Option.map cls.sw_cls_guard
                  ~f:(fun grd -> compile_node' ctx grd) in
              let act_ops = compile_fold' ctx cls.sw_cls_action in
              { sw_cls_var = var;
                sw_cls_ptn = ptn_op;
                sw_cls_guard = grd_op;
                sw_cls_action = act_ops;
              });
      in
      ctx, Switch {
        sw_val = val_op;
        sw_clss = cls_ops;
      }

    | `If if_ -> 
      Printf.printf "HIR: compile if\n";
      let claus = List.map if_.if_actions
          ~f:(fun (cond, action) ->
              let ctx, cond_op = compile_node ctx cond in
              let act_ops = compile_fold' ctx action in
              cond_op, act_ops)
      in
      let other = compile_fold' ctx if_.if_else in
      ctx, If { if_actions = claus; if_else = other }

    | `For for_ ->
      Printf.printf "HIR: compile for\n";
      let range_op = compile_node' ctx for_.for_range in
      let ctx', var = new_var ctx Type.int ~name:for_.for_var.desc in
      let block_ops = compile_fold' ctx' for_.for_block in
      ctx, For { for_var = var;
                 for_range = range_op;
                 for_block = block_ops }

    | `Return exp ->
      Printf.printf "HIR: compile return\n";
      let ctx, ret = match exp with
        | None -> ctx, None
        | Some exp ->
          let ctx, var = new_var ctx @@ Ast.type_exn exp in
          let op = compile_node' ctx exp in
          ctx, Some { ret_var = var; ret_val = op }
      in
      ctx, Return ret

    | `Funcall call -> 
      Printf.printf "HIR: compile funcall\n";
      let fun_op = compile_node' ctx call.fc_fun in
      let arg_ops =
        List.map call.fc_args
          ~f:(fun arg -> compile_node' ctx arg)
      in
      begin match fun_op with
        | Ref_prop ref ->
          ctx, Methcall { mcall_recv = ref.ref_prop_obj;
                          mcall_name = ref.ref_prop_name;
                          mcall_args = arg_ops;
                          mcall_ty = ref.ref_prop_ty }
        | _ ->
          ctx, Call { call_fun = fun_op;
                      call_args = arg_ops;
                      call_ty = Type.void } (* TODO *)
      end

    | `Binexp exp ->
      let left = compile_node' ctx exp.binexp_left in
      let right = compile_node' ctx exp.binexp_right in
      let op : Binop.t = match exp.binexp_op.desc with
        | `Eq -> Eq
        | `Ne -> Ne
        | `Lt -> Lt
        | `Le -> Le
        | `Gt -> Gt
        | `Ge -> Ge
        | `Add -> Add
        | `Sub -> Sub
        | `Mul -> Mul
        | `Div -> Div
        | _ -> failwith "not impl"
      in
      ctx, Binexp { binexp_left = left;
                    binexp_op = op;
                    binexp_right = right;
                    binexp_ty = Ast.type_exn node }

    | `Var var ->
      let name = var.var_name.desc in
      Printf.printf "HIR: compile var '%s'\n" name;
      Option.iter var.var_value
        ~f:(fun value ->
            Printf.printf "HIR: var = %s\n" (Value.to_string value));

      let desc = Option.value_exn var.var_value in
      begin match desc with
        | { scope = `Module path;
            type_ = { desc = `Poly { contents = `Unify ty } };
            name } ->
          (* TODO: namepath *)
          let m = Option.value_exn (Library.find_module path.name) in
          use_module ctx m;
          ctx, Poly {
            poly_path = Namepath.create name ~prefix:(Some path);
            poly_type = ty }

        | { scope = `Module _ } ->
          ctx, Ref desc
        | _ ->
          let ty = Type.unwrap var.var_type in
          begin match Type.prim_id ty with
            | Some id ->
              ctx, Prim { prim_id = id; prim_type = ty }
            | None ->
              match var.var_prefix with
              | None ->
                begin match get_var ctx name with
                  | None -> ctx, Ref_fun { Id.name; ty }
                  | Some var -> ctx, Var var
                end
              | Some prefix ->
                Printf.printf "HIR: property '%s'\n" name;
                let obj = compile_node' ctx prefix in
                ctx, Ref_prop { ref_prop_obj = obj;
                                ref_prop_name = name;
                                ref_prop_ty = ty }
          end
      end

    | `Range range ->
      let begin_op = compile_node' ctx range.range_begin in
      let end_op = compile_node' ctx range.range_end in
      ctx, Range { range_begin = begin_op;
                   range_end = end_op;
                   range_kind = range.range_kind }

    | `Fun fn ->
      Printf.printf "HIR: compile anon fun\n";
      Ast.print node;
      let body = fn.fun_body in
      let fun_ctx = ctx in

      (* parameters *)
      let fun_ctx, params = compile_params fun_ctx
          ~fun_type:body.fbody_type
          ~params:body.fbody_params in
      Printf.printf"HIR: anon fun params = %d\n" (List.length params);

      let _, block = compile_fold fun_ctx body.fbody_block in
      ctx, Fun (Closure.create body.fbody_type
                  ~scope:`Anon
                  ~params
                  ~block)

    | `Bool value -> ctx, Bool value.desc
    | `Int value -> ctx, Int value.desc
    | `String value -> ctx, String value.desc
    | _ -> ctx, Nop

  and compile_node' ctx es =
    snd @@ compile_node ctx es

  and compile_fold ctx es =
    let ctx', rev_ops = List.fold_left es
        ~init:(ctx, [])
        ~f:(fun (ctx, accu) e ->
            let ctx', op = compile_node ctx e in
            ctx', op :: accu)
    in
    ctx', List.rev rev_ops

  and compile_fold' ctx es =
    snd @@ compile_fold ctx es

  and compile_iter ctx es =
    List.iter es ~f:(fun e -> ignore @@ compile_node ctx e)

  and compile_params ctx ~fun_type ~params =
    let open Context in
    let param_tys = Type.fun_params fun_type in
    let ctx, rev_vars = List.fold2_exn params param_tys
        ~init:(ctx, [])
        ~f:(fun (ctx, vars) name ty ->
            let ctx, var = new_var ctx ty ~name:name.desc in
            ctx, var :: vars)
    in
    ctx, List.rev rev_vars

  and compile_ptn ctx (ptn:Ast.pattern) : Context.t * Op.pattern =
    let open Op in

    let fold ctx ptns ~f =
      let ctx, rev_ptns =
        List.fold_left ptns
          ~init:(ctx, [])
          ~f:(fun (ctx, accu) ptn ->
              let ctx, ptn' = compile_ptn ctx ptn in
              ctx, ptn' :: accu) in
      ctx, f @@ List.rev rev_ptns
    in

    match ptn.ptn_cls with
    | `Nop _ -> ctx, Ptn_nop
    | `Bool v -> ctx, Ptn_bool v.desc
    | `Int v -> ctx, Ptn_int v.desc
    | `String v -> ctx, Ptn_string v.desc
    | `Float v -> ctx, Ptn_float v.desc
    | `List ptns -> fold ctx ptns ~f:(fun ptns' -> Ptn_list ptns')
    | `Tuple ptns -> fold ctx ptns ~f:(fun ptns' -> Ptn_tuple ptns')
    | _ -> ctx, Ptn_nop

  and compile_ptn' ctx ptn =
    snd @@ compile_ptn ctx ptn

  let run file node =
    let ctx = Context.create file in
    let ctx, _ = compile_node ctx node in
    Option.value_exn ctx.prog

end
