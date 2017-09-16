(* high-level intermediate representation *)

open Core.Std

type id = string

module Var = struct

  type t = {
    id : id;
    ty : Type.t;
  }

end

module Op = struct

  type t =
    | Nop
    | Fundef of fundef
    | Vardef of vardef
    | Assign of assign
    | Switch of switch
    | If of if_
    | Return of return option
    | Block of t list
    | Call of call
    | Var of Var.t
    | Ref_fun of Var.t
    | Ref_var of Var.t
    | Prim of primitive
    | Null
    | Bool of bool
    | String of string
    | Int of int
    | Float of float
    | List of t list

  and fundef = {
    fdef_var : Var.t; (* TODO: need? *)
    fdef_ty : Type.t;
    fdef_name : string;
    fdef_params : Var.t list;
    fdef_block : t list;
  }

  and vardef = {
    vdef_var : Var.t;
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
    sw_cls_var : Var.t;
    sw_cls_ptn : pattern;
    sw_cls_guard : t option;
    sw_cls_action : t list;
  }

  and pattern =
    | Ptn_nop
    | Ptn_void
    | Ptn_bool of bool
    | Ptn_string of string
    | Ptn_int of int
    | Ptn_float of float
    | Ptn_list of pattern list
    | Ptn_tuple of pattern list
    | Ptn_var of id

  and if_ = {
    if_actions : (t * t list) list;
    if_else : t list;
  }

  and return = {
    ret_var : Var.t;
    ret_val : t;
  }

  and call = {
    call_fun : t;
    call_args : t list;
    call_ty : Type.t;
  }

  and block = {
    blk_vars : t list;
    blk_list : t list;
  }

  and primitive = {
    prim_name : string;
    prim_ty : Type.t;
  }

end

module Closure = struct

  type t = {
    parent : t option;
    ty : Type.t;
    var : Var.t;
    name : string;
    params : Var.t list;
    block : Op.t list;
  }

  let create ~parent ~ty ~var ~name ~params ~block =
    { parent; ty; var; name; params; block }

  let of_fundef (def:Op.fundef) =
    create
      ~parent:None
      ~ty:def.fdef_ty
      ~var:def.fdef_var
      ~name:def.fdef_name
      ~params:def.fdef_params
      ~block: def.fdef_block

end

module Program = struct

  type t = {
    file : string;
    package : string option;
    funs : Closure.t list;
  }

  let create ~file ~package ~funs =
    { file; package; funs }

end

module Context = struct

  type t = {
    file : string;
    package : string option;
    stack : t list;
    id : int;
    vars : Var.t list;
    var_map : Var.t String.Map.t;
    mutable ret : Type.t;
    mutable prog : Program.t option;
  }

  let create file =
    { file;
      package = None;
      stack = [];
      id = 0;
      vars = [];
      var_map = String.Map.empty;
      ret = Type.unit;
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
    let var = { Var.id = id; ty = ty } in
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
  let main_fun = { Var.id = "main"; ty = Type.unit }

end

module Compiler = struct

  let rec compile_node (ctx:Context.t) (node:Ast.t) : Context.t * Op.t =
    let open Located in
    let open Op in
    let open Context in

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
                          ~funs);
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
        | _ -> new_var fun_ctx @@ Option.value_exn def.fdef_type
      in 

      (* parameters *)
      let param_tys = Type.fun_params @@ Option.value_exn def.fdef_type in
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
        fdef_var = fun_var;
        fdef_ty = Option.value_exn def.fdef_type;
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
      let _, fun_op = compile_node ctx call.fc_fun in
      let arg_ops =
        List.map call.fc_args
          ~f:(fun arg -> compile_node' ctx arg)
      in
      ctx, Call { call_fun = fun_op;
                  call_args = arg_ops;
                  call_ty = Type.unit }

    | `Var var ->
      let name = var.var_name.desc in
      Printf.printf "HIR: compile var '%s'\n" name;
      let ty = Type.unwrap @@ Option.value_exn var.var_type in
      begin match Type.prim_name ty with
        | Some name ->
          ctx, Prim { prim_name = name; prim_ty = ty }
        | None ->
          (* TODO: namepath *)
          match get_var ctx name with
          | None -> ctx, Ref_fun { Var.id = name; ty }
          | Some var -> ctx, Ref_var var
      end

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
    | `Unit _ -> ctx, Ptn_void
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
