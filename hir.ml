(* high-level intermediate representation *)

open Core.Std

module Op = struct

  type id = string

  type t =
    | Nop
    | Vardef of vardef
    | Assign of assign
    | Block of t list
    | Call of call
    | Var of var
    | Prim of primitive
    | Null
    | Bool of bool
    | String of string
    | Int of int
    | Float of float
    | List of t list

  and vardef = {
    vdef_id : id;
    vdef_ptn : t;
    vdef_exp : t;
  }

  and assign = {
    asn_var : t;
    asn_exp : t;
  }

  and call = {
    call_fun : t;
    call_args : t list;
    (*call_ty : Type.t;*)
  }

  and block = {
    blk_vars : t list;
    blk_list : t list;
  }

  and var = {
    var_id : id;
    var_ty : Type.t;
  }

  and primitive = {
    prim_name : string;
    prim_ty : Type.t;
  }

end

module Closure = struct

  type var = {
    var_id : string;
    var_ty : Type.t;
  }

  type t = {
    parent : t option;
    name : string option;
    vars : var list;
    ops : Op.t list;
    (* ret : Type.t; *)
  }

  let create ~parent ~name ~vars ~ops =
    { parent; name; vars; ops }

end

module Program = struct

  type t = {
    file : string;
    package : string option;
    clos : Closure.t;
  }

  let create ~file ~package ~clos =
    { file; package; clos }

end

module Context = struct

  type var = {
    var_id : string;
    var_orig : string;
    (*var_ty : Type.t;*) (* TODO*)
    mutable var_share : bool;
  }

  type t = {
    file : string;
    package : string option;
    id : int;
    vars : var String.Map.t;
    mutable ret : Type.t;
    mutable prog : Program.t option;
  }

  let create file =
    { file;
      package = None;
      id = 0;
      vars = String.Map.empty;
      ret = Type.unit;
      prog = None;
    }

  let gen_id ctx =
    let id = ctx.id + 1 in
    { ctx with id = id }, Printf.sprintf "t%d" id

                            (*
  let add_var ctx name id =
    let var = { var_id = id; var_share = false } in
    { ctx with vars = String.Map.add ctx.vars
                   ~key:name
                   ~data:var }
                             *)

  let to_clos ctx ~parent ~name ~ops =
    (* TODO: vars *)
    Closure.create ~parent ~name ~vars:[] ~ops

end

module Compiler = struct

  let rec compile_node (ctx:Context.t) (node:Ast.t) =
    let open Located in
    let open Op in
    let open Context in

    match node with
    | `Chunk chunk ->
      let ctx, _ = compile_fold ctx chunk.ch_attrs in
      let ctx, clos = compile_block ctx
          ~parent:None
          ~name:None
          ~ops:chunk.ch_block in
      ctx.prog <- Some (Program.create
                          ~file:ctx.file
                          ~package:ctx.package
                          ~clos);
      ctx, Nop

    | `Package name ->
      { ctx with package = Some name.desc }, Nop

    | `Vardef def ->
      let ctx, id = gen_id ctx in
      let exp_op = compile_node' ctx def.vdef_exp in
      let ctx, ptn_op = compile_ptn ctx def.vdef_ptn in
      let vdef = { vdef_id = id;
                   vdef_ptn = ptn_op;
                   vdef_exp = exp_op } in
      ctx, Vardef vdef

    | `Funcall call -> 
      Printf.printf "HIR: compile funcall\n";
      let _, fun_op = compile_node ctx call.fc_fun in
      let arg_ops =
        List.map call.fc_args
          ~f:(fun arg -> compile_node' ctx arg)
      in
      ctx, Call { call_fun = fun_op; call_args = arg_ops }

    | `Var var ->
      Printf.printf "HIR: compile var\n";
      let ty = Type.unwrap @@ Option.value_exn var.var_type in
      begin match Type.prim_name ty with
        | Some name ->
          ctx, Prim { prim_name = name; prim_ty = ty }
        | None ->
          let ctx, var_id = gen_id ctx in
          ctx, Var { var_id; var_ty = ty }
      end

    | `Bool value -> ctx, Bool value.desc
    | `Int value -> ctx, Int value.desc
    | `String value -> ctx, String value.desc
    | _ -> ctx, Nop

  and compile_node' ctx es =
    snd @@ compile_node ctx es

  and compile_fold ctx es =
    let ctx, rev_ops = List.fold_left es
        ~init:(ctx, [])
        ~f:(fun (ctx, accu) e ->
            let ctx, op = compile_node ctx e in
            ctx, op :: accu)
    in
    ctx, List.rev rev_ops

  and compile_iter ctx es =
    List.iter es ~f:(fun e -> ignore @@ compile_node ctx e)

  and compile_ptn ctx (ptn:Ast.pattern) =
    let open Op in
    match ptn with
    | _ -> ctx, Nop

  and compile_ptn' ctx ptn =
    snd @@ compile_ptn ctx ptn

  and compile_block ctx ~parent ~name ~ops =
    let sub, ops = compile_fold ctx ops in
    let blk = Context.to_clos sub ~parent ~name ~ops in
    ctx, blk

  let run file node =
    let ctx = Context.create file in
    let ctx, _ = compile_node ctx node in
    Option.value_exn ctx.prog

end
