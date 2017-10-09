(* low-level intermediate representation *)

open Core

module Register = struct

  type id = string

  type name = string

  type t = {
    id : id;
    ty : Raw_type.t;
    scope : [`Param of name | `Temp | `Named of name];
  }

  let create ~id ~ty ~scope = { id; ty; scope }

  let ids regs =
    List.map regs ~f:(fun reg -> reg.id)

  let params regs =
    List.filter regs ~f:(fun reg ->
        match reg.scope with
        | `Param _ -> true
        | _ -> false)

  let locals regs =
    List.filter regs ~f:(fun reg ->
        match reg.scope with
        | `Param _ -> false
        | _ -> true)

end

module Binop = struct

  type t =
    | Add
    | Sub
    | Mul
    | Div

end

module Op = struct

  type label = string

  type t =
    | Nop
    | Comment of string list
    | Package of string
    | Import of string
    | Struct of (string * Type.t) list
    | Vardef of (Register.t * Type.t)
    | Defer of t

    | Move of move
    | Eq_void of Register.t
    | Eq_bool of Register.t * bool
    | Eq_int of Register.t * int
    | Eq_float of Register.t * float
    | Eq_string of Register.t * string
    | Eq of Register.t * Register.t
    | Ne of Register.t * Register.t
    | Lt of Register.t * Register.t
    | Le of Register.t * Register.t
    | Gt of Register.t * Register.t
    | Ge of Register.t * Register.t
    | Branch of branch
    | Jump of label
    | Return of Register.t
    | Void_return
    | Nothing_return
    | Label of label

    | For of Register.t * Register.t
    | End_for

    | Call of call
    | Methcall of method_call
    | Binexp of binexp
    | Block of t list
    | Terminal
    | Var of var
    | Ref of ref_
    | Poly of poly
    | Ref_fun of ref_fun
    | Ref_prop of ref_prop
    | Prim of primitive
    | Null
    | Void of Register.t
    | Bool of bool
    | Int of Register.t * int
    | Float of float
    | String of Register.t * string
    | Range of range
    | Fun of Register.t * closure

  and context = {
    ctx_src : string;
    ctx_ops : t list;
    ctx_locals : Register.t list;
    ctx_flag : Register.id;
  }

  and move = {
    mv_from : Register.t;
    mv_to : Register.t;
  }

  and branch = {
    br_cond : bool;
    br_dest : label;
  }

  and ref_ = {
    ref_path : string Namepath.t;
    ref_to : Register.t;
  }

  and ref_fun = {
    ref_fun_from : string;
    ref_fun_to : Register.t;
  }

  and var = {
    var_from : Register.t;
    var_to : Register.t;
  }

  and poly = {
    poly_path : string Namepath.t;
    poly_box : string;
    poly_to : Register.t
  }

  and ref_prop = {
    ref_prop_from : Register.t;
    ref_prop_to : Register.t;
    ref_prop_name : string;
  }

  and call = {
    call_rc : Register.t;
    call_fun : Register.t;
    call_args : Register.t list;
  }

  and method_call = {
    mcall_rc : Register.t;
    mcall_recv : Register.t;
    mcall_name : string;
    mcall_args : Register.t list;
  }

  and binexp = {
    binexp_rc : Register.t;
    binexp_left : Register.t;
    binexp_op : Hir.Binop.t;
    binexp_right : Register.t;
  }

  and primitive = {
    prim_rc : Register.t;
    prim_id : string;
  }

  and range = {
    range_to : Register.t;
    range_begin : Register.t;
    range_end : Register.t;
    range_kind : [`Half_open | `Closed];
  }

  and closure = {
    clos_ctx : context;
    clos_type : Raw_type.t;
    clos_scope : [`Named of string | `Anon | `Main | `Init];
    clos_params : Register.t list;
    clos_locals : Register.t list;
    clos_body : t list;
  }

end

module Closure = struct

  type t = Op.closure

  let create ctx ~type_ ~scope ~params ~locals ~body : t =
    { Op.clos_ctx = ctx;
      clos_type = type_;
      clos_scope = scope;
      clos_params = params;
      clos_locals = locals;
      clos_body = body;
    }

  let string_of_scope = function
    | `Main -> "main"
    | `Init -> "init"
    | `Named name -> Printf.sprintf "\"%s\"" name
    | `Anon -> "fun"

end

module Program = struct

  type poly = {
    poly_path : string Namepath.t;
    poly_type : Raw_type.t;
    poly_box : string;
  }

  type t = {
    src : string;
    out : string;
    pkg : string option;
    vars : string list; (* TODO: var type *)
    funs : Op.closure list;
    main : string option;
    polys : poly list;
    used_mods : Module.t list;
  }

  let basic_pkgs = [
  ]

  let create ~src ~out ~pkg ~vars ~funs ~main ~polys ~used_mods =
    { src; out; pkg; vars; funs; main; polys; used_mods }

  let ext = ".go"

  let bridge_prim_prefix = "EscaPrim"

  let bridge_prim_id name =
    bridge_prim_prefix ^ (Utils.camel_case name)

  let flag = "fr"

  let rec write (prog:t) =
    let open Buffer in
    let buf = Buffer.create 16 in
    write_pkg buf prog;
    write_import buf prog;
    Printf.printf "polys: %d\n" (List.length prog.polys);
    List.iter prog.polys ~f:(write_poly buf);
    List.iter prog.funs ~f:(write_clos buf);
    write_main buf prog;
    let code = contents buf in
    Printf.printf "golang = %s\n" (contents buf);
    Printf.printf "out = %s\n" prog.out;
    Out_channel.write_all prog.out ~data:code;
    prog.out

  and write_pkg buf prog =
    let open Buffer in
    Option.iter prog.main ~f:(fun _ ->
        add_string buf "package ";
        add_string buf @@ Option.value prog.pkg ~default:"main";
        add_string buf "\n\n")

  and write_import buf prog =
    let open Buffer in
    add_string buf "import (\n";
    List.iter basic_pkgs
      ~f:(fun pkg ->
          add_string buf @@ sprintf "\"%s/%s\"\n" !Config.runlib_path pkg);
    List.iter prog.used_mods
      ~f:(fun m ->
          add_string buf @@ sprintf "%s \"%s\"\n"
            (Go.Name.import_name @@ Module.namepath m)
            (Module.package m));
    add_string buf ")\n\n"

  and write_poly buf (poly:poly) =
    let open Buffer in
    add_string buf @@ sprintf "func %s" poly.poly_box;

    begin match poly.poly_type with
      | Fun (args, ret) ->
        let _, rev_args =
          List.fold_left args ~init:(0, [])
            ~f:(fun (id, temps) ty ->
                let temp = sprintf "t%d %s" id (Raw_type.to_decl ty) in
                id + 1, temp :: temps) in
        let args = List.rev rev_args in
        add_string buf @@ sprintf "(%s) %s {\n"
          (String.concat ~sep:", " args)
          (Raw_type.to_decl ret);

        (* call function *)
        let _, rev_call_args =
          List.fold_left args ~init:(0, [])
            ~f:(fun (id, temps) _ ->
                let temp = sprintf "t%d" id in
                id + 1, temp :: temps) in
        let call_args = List.rev rev_call_args in
        add_string buf @@ sprintf "return %s(%s)\n"
          (Go.Name.value_path poly.poly_path)
          (String.concat ~sep:", " call_args);
        add_string buf "}\n\n"

      | _ -> failwith "not function"
    end

  and write_main buf prog =
    let open Buffer in
    Option.iter prog.main ~f:(fun reg ->
        add_string buf @@ sprintf "func main() { %s() }" reg)

  and decl_var buf name ty =
    let open Buffer in
    add_string buf @@ sprintf "var %s %s\n" name (Raw_type.to_decl ty);
    add_string buf @@ sprintf "var _ = %s\n" name

  and with_exp buf (reg:Register.t) ~f =
    let open Buffer in
    begin match reg.ty with
      | Void -> ()
      | _ -> add_string buf @@ sprintf "%s = " reg.id
    end;
    f buf;
    add_string buf @@ sprintf "\n"

  and write_op buf (op:Op.t) =
    let open Buffer in
    let open Printf in
    let add s = add_string buf s in
    let ln n = add (String.make n '\n') in
    let addln s =
      add s;
      ln 1
    in

    match op with

    | Move mv ->
      addln @@ sprintf "%s = %s" mv.mv_to.id mv.mv_from.id

    | Branch br ->
      add @@ sprintf "if ";
      if not br.br_cond then begin
        add "!"
      end;
      addln @@ sprintf "%s {" flag;
      addln @@ sprintf "goto %s" br.br_dest;
      addln "}"

    | Jump label ->
      addln @@ sprintf "goto %s" label

    | Label label ->
      addln @@ sprintf "\n%s:" label

    | Eq_void reg ->
      addln @@ sprintf "%s = %s == Void{}" flag reg.id

    | Eq_bool (reg, v) ->
      addln @@ sprintf "%s = %s == %s" flag reg.id (Go.Repr.of_bool v)

    | Eq_int (reg, v) ->
      addln @@ sprintf "%s = %s == %d" flag reg.id v

    | Eq_float (reg, v) ->
      addln @@ sprintf "%s = %s == %f" flag reg.id v

    | Eq_string (reg, v) ->
      addln @@ sprintf "%s = %s == %s" flag reg.id (Go.Repr.of_string v)

    | Eq (left, right) ->
      addln @@ sprintf "%s = %s == %s" flag left.id right.id

    | Ne (left, right) ->
      addln @@ sprintf "%s = %s != %s" flag left.id right.id

    | Lt (left, right) ->
      addln @@ sprintf "%s = %s < %s" flag left.id right.id

    | Le (left, right) ->
      addln @@ sprintf "%s = %s <= %s" flag left.id right.id

    | Gt (left, right) ->
      addln @@ sprintf "%s = %s > %s" flag left.id right.id

    | Ge (left, right) ->
      addln @@ sprintf "%s = %s >= %s" flag left.id right.id

    | Return reg ->
      addln @@ sprintf "return %s" reg.id

    | Nothing_return ->
      addln @@ sprintf "return"

    | For (var, range) ->
      addln @@ sprintf "for %s := %s.Begin; %s < %s.End; %s++ {"
        var.id range.id var.id range.id var.id

    | End_for ->
      addln "} // for"

    | Call call ->
      Printf.printf "call\n";
      with_exp buf call.call_rc
        ~f:(fun _ ->
            add call.call_fun.id;
            add "(";
            add @@ String.concat ~sep:"," (Register.ids call.call_args);
            add ")")

    | Methcall call ->
      Printf.printf "method call\n";
      let args = call.mcall_recv :: call.mcall_args in
      with_exp buf call.mcall_rc
        ~f:(fun _ ->
            add call.mcall_name;
            add "(";
            add @@ String.concat ~sep:"," (Register.ids args);
            add ")")

    | Binexp exp ->
      Printf.printf "binexp\n";
      let op = match exp.binexp_op with
        | Add -> "+"
        | Sub -> "-"
        | _ -> failwith "not impl"
      in
      with_exp buf exp.binexp_rc
        ~f:(fun _ ->
            add @@ sprintf "%s %s %s" exp.binexp_left.id
              op exp.binexp_right.id)

    | Prim prim ->
      let bridge = bridge_prim_id prim.prim_id in
      with_exp buf prim.prim_rc ~f:(fun _ -> add bridge)

    | Var var ->
      addln @@ sprintf "%s = %s" var.var_to.id var.var_from.id

    | Ref ref ->
      addln @@ sprintf "%s = %s" ref.ref_to.id
        (Go.Name.value_path ref.ref_path)

    | Poly poly ->
      addln @@ sprintf "%s = %s" poly.poly_to.id poly.poly_box

    | Ref_fun ref ->
      addln @@ sprintf "%s = %s" ref.ref_fun_to.id ref.ref_fun_from

    | Ref_prop ref ->
      Printf.printf "write: ref_prop %s\n" (Raw_type.to_decl ref.ref_prop_from.ty);
      addln @@ sprintf "%s = %s.%s"
        ref.ref_prop_to.id
        ref.ref_prop_from.id
        ref.ref_prop_name

    | Null -> add "null"

    | Void reg ->
      with_exp buf reg ~f:(fun _ -> add "Void{}")

    | Int (reg, value) ->
      with_exp buf reg ~f:(fun _ -> add @@ sprintf "%d" value)

    | String (reg, value) ->
      with_exp buf reg ~f:(fun _ -> add @@ sprintf "\"%s\"" value)

    | Range range ->
      with_exp buf range.range_to ~f:(fun _ ->
          let fname =
            match range.range_kind with
            | `Closed -> "CreateClosedRange"
            | `Half_open -> "CreateHalfOpenRange"
          in
          add @@ sprintf "%s.%s(%s, %s)"
            Raw_type.kernel_decl
            fname
            range.range_begin.id
            range.range_end.id)

    | Fun (reg, clos) ->
      addln @@ sprintf "%s = " reg.id;
      write_clos buf clos

    | _ -> ()

  and write_ops buf ops =
    List.iter ops ~f:(write_op buf)

  and write_clos buf clos =
    let open Buffer in

    (* function name *)
    let ret_ty = Raw_type.return_ty_exn clos.clos_type in
    let name, ret_ty = match clos.clos_scope with
      | `Main -> "main", Raw_type.Void
      | `Init -> "init", Raw_type.Void
      | `Named name -> name, ret_ty
      | `Anon -> "", ret_ty
    in
    add_string buf @@ sprintf "func %s(" name;

    (* parameters *)
    let params =
      List.map clos.clos_params
        ~f:(fun var ->
            sprintf "%s %s" var.id (Raw_type.to_decl var.ty))
      |> String.concat ~sep:", "
    in
    add_string buf @@ sprintf "%s) " params;

    (* return value type *)
    add_string buf @@ sprintf " %s {\n" (Raw_type.to_decl ret_ty);

    (* define flag variable *)
    decl_var buf flag Raw_type.Bool;

    (* declare variables to avoid "goto" error ("jumps over declaration") *)
    List.iter clos.clos_locals
      ~f:(fun local ->
          match local.scope with
          | `Temp ->
            begin match local.ty with
              | Void -> ()
              | _ ->
                add_string buf @@ Printf.sprintf "var %s %s\nvar _ = %s\n"
                  local.id (Raw_type.to_decl local.ty) local.id
            end
          | _ -> ());
    add_string buf "\n";

    (* block *)
    write_ops buf clos.clos_body;

    (* suppress return type error after last label *)
    begin match clos.clos_scope with
      | `Main | `Init -> ()
      | _ -> add_string buf "panic(\"Unreachable code\")\n"
    end;

    (* end *)
    add_string buf "}\n\n";

end

module Context = struct

  type t = {
    src : string;
    ops : Op.t list;
    locals : Register.t list;
    rc : Register.t;
    flag : Register.id;
    labels : Op.label list;
    main : Register.t option; (* TODO: unused? *)
    polys : Program.poly list;
  }

  let create src =
    { src;
      ops = [];
      locals = [];
      rc = Register.create ~id:"r0" ~ty:Raw_type.Void ~scope:`Temp (* dummy *);
      flag = "fr";
      labels = [];
      main = None;
      polys = [];
    }

  let add_local ctx reg =
    { ctx with locals = reg :: ctx.locals }

  let new_local ctx ~ty ~prefix ~scope =
    let n = List.length ctx.locals in
    let id = Printf.sprintf "%s%d" prefix n in
    let reg = Register.create ~id ~ty ~scope in
    let ctx = add_local ctx reg in
    { ctx with rc = reg }, reg

  let new_param ctx ~ty ~name =
    new_local ctx ~ty ~prefix:"p" ~scope:(`Param name)

  let new_temp ctx ~ty =
    new_local ctx ~ty ~prefix:"t" ~scope:`Temp

  let new_named ctx ~ty ~name =
    new_local ctx ~ty ~prefix:"t" ~scope:(`Named name)

  let find_named ctx name =
    Printf.printf "find named %s %d\n" name (List.length ctx.locals);
    List.find_exn ctx.locals ~f:(fun local ->
        match local.scope with
        | `Named name'
        | `Param name' -> name = name'
        | _ -> false)

  let set_main ctx reg =
    { ctx with main = Some reg }

  let new_label ctx =
    let n = List.length ctx.labels in
    let label = Printf.sprintf "L%d" n in
    { ctx with labels = label :: ctx.labels }, label

  let add_poly ctx (path:string Namepath.t) (ty:Raw_type.t) =
    let n = List.length ctx.polys in
    let box = Printf.sprintf "__EscaPoly_%d" n in
    let poly = { Program.poly_path = path; poly_box = box; poly_type = ty } in
    { ctx with polys = poly :: ctx.polys }, box

  let add_op ctx op =
    { ctx with ops = op :: ctx.ops }

  let add_temp_op ctx ty ~f =
    let ctx, reg = new_temp ctx ~ty in
    add_op ctx @@ f reg

  let move ctx ~from ~to_ =
    add_op ctx @@ Move { mv_from = from; mv_to = to_ }

  let finish ctx =
    { ctx with ops = List.rev ctx.ops;
               locals = List.rev ctx.locals }

  let to_op_clos_ctx ctx : Op.context =
    { ctx_src = ctx.src;
      ctx_ops = ctx.ops;
      ctx_locals = List.rev ctx.locals;
      ctx_flag = ctx.flag }

  let map ctx elts ~f =
    let ctx, rev_accu =
      List.fold_left elts
        ~init:(ctx, [])
        ~f:(fun (ctx, accu) elt ->
            let ctx, elt = f ctx elt in
            ctx, elt :: accu)
    in
    ctx, List.rev rev_accu

end

module Compiler = struct

  let rec compile_clos ctx (clos:Hir.Closure.t) : Context.t * Closure.t =
    Printf.printf "LIR: compile closure '%s'\n"
      (Hir.Closure.string_of_scope clos.clos_scope);

    let open Context in

    (* parameters *)
    let clos_ctx, rev_params = List.fold_left clos.clos_params
        ~init:(ctx, [])
        ~f:(fun (ctx, params) var ->
            let ctx, param = new_param ctx
                ~ty:(Raw_type.of_type var.ty)
                ~name:var.name
            in
            ctx, param :: params)
    in
    let params = List.rev rev_params in

    let clos_ctx = compile_block clos_ctx clos.clos_block in

    (* return value *)
    let clos_ctx =
      match clos.clos_scope with
      | `Anon | `Named _ -> clos_ctx
      | `Main | `Init -> add_op clos_ctx @@ Nothing_return
    in

    (* finish *)
    let clos_ctx = finish clos_ctx in
    let l_clos = Closure.create
        (to_op_clos_ctx clos_ctx)
        ~type_:(Raw_type.of_type clos.clos_type)
        ~scope:clos.clos_scope
        ~params:params
        ~locals:(Register.locals clos_ctx.locals)
        ~body:clos_ctx.ops in
    { ctx with polys = List.append ctx.polys clos_ctx.polys }, l_clos

  and compile_op (ctx:Context.t) (op:Hir.Op.t) : Context.t =
    let open Context in
    Printf.printf "LIR: poly %d\n" (List.length ctx.polys);
    match op with

    | Switch sw ->
      Printf.printf "LIR: compile switch\n";
      let ctx = compile_op ctx sw.sw_val in
      let val_reg = ctx.rc in
      List.fold_left sw.sw_clss
        ~init:ctx
        ~f:(fun ctx cls ->
            let ctx = compile_ptn ctx val_reg cls.sw_cls_ptn in
            let ctx, dest = new_label ctx in
            let ctx = add_op ctx @@
              Branch { br_cond = false; br_dest = dest } in
            let ctx = compile_block ctx cls.sw_cls_action in
            add_op ctx @@ Label dest)

    | If if_ ->
      Printf.printf "LIR: compile if\n";
      let ctx, end_ = new_label ctx in

      (* actions *)
      let ctx = List.fold if_.if_actions
          ~init:ctx
          ~f:(fun ctx (cond, act) ->
              Printf.printf "LIR: compile if-action\n";
              let ctx, dest = new_label ctx in
              let ctx = compile_op ctx cond in
              let ctx = add_op ctx @@
                Branch { br_cond = false; br_dest = dest } in
              let ctx = compile_block ctx act in
              let ctx = add_op ctx @@ Jump end_ in
              add_op ctx @@ Label dest)
      in

      (* else-action *)
      let ctx = compile_block ctx if_.if_else in
      add_op ctx @@ Label end_

    | For for_ ->
      Printf.printf "LIR: compile for\n";
      let ctx, var_reg = new_named ctx
          ~ty:Raw_type.Int
          ~name:for_.for_var.name in
      let ctx = compile_op ctx for_.for_range in
      let range_reg = ctx.rc in
      let ctx = add_op ctx @@ For (var_reg, range_reg) in
      let ctx = compile_block ctx for_.for_block in
      add_op ctx @@ End_for

    | Return ret ->
      Printf.printf "LIR: compile return\n";
      begin match ret with
        | None ->
          add_op ctx Void_return
        | Some ret ->
          let ctx = compile_op ctx ret.ret_val in
          add_op ctx @@ Return ctx.rc
      end

    | Call call ->
      Printf.printf "LIR: compile call\n";
      let ctx = compile_op ctx call.call_fun in
      Printf.printf "LIR: poly %d\n" (List.length ctx.polys);
      let fun_reg = ctx.rc in
      Printf.printf "LIR: call fun %s\n" fun_reg.id;
      let ctx, arg_regs = compile_exps ctx call.call_args in
      add_temp_op ctx (Raw_type.return_ty_exn fun_reg.ty)
        ~f:(fun reg -> Call { call_rc = reg;
                              call_fun = fun_reg;
                              call_args = arg_regs })

    | Methcall call ->
      Printf.printf "LIR: compile primitive method call\n";
      let ctx = compile_op ctx call.mcall_recv in
      let fun_ty = Raw_type.of_type call.mcall_ty in
      let recv_reg = ctx.rc in
      let ctx, arg_regs = compile_exps ctx call.mcall_args in
      let name = Raw_type.symbol_method recv_reg.ty call.mcall_name in
      add_temp_op ctx (Raw_type.return_ty_exn fun_ty)
        ~f:(fun reg -> Methcall { mcall_rc = reg;
                                  mcall_recv = recv_reg;
                                  mcall_name = name;
                                  mcall_args = arg_regs })

    | Binexp exp ->
      Printf.printf "LIR: compile binexp\n";
      let ctx = compile_op ctx exp.binexp_left in
      let left_op = ctx.rc in
      let ctx = compile_op ctx exp.binexp_right in
      let right_op = ctx.rc in
      begin match exp.binexp_op with
        | Eq -> add_op ctx @@ Eq (left_op, right_op)
        | Ne -> add_op ctx @@ Ne (left_op, right_op)
        | Lt -> add_op ctx @@ Lt (left_op, right_op)
        | Le -> add_op ctx @@ Le (left_op, right_op)
        | Gt -> add_op ctx @@ Gt (left_op, right_op)
        | Ge -> add_op ctx @@ Ge (left_op, right_op)
        | _ ->
          add_temp_op ctx (Raw_type.of_type exp.binexp_ty)
            ~f:(fun reg -> Binexp { binexp_rc = reg;
                                    binexp_left = left_op;
                                    binexp_op = exp.binexp_op;
                                    binexp_right = right_op })
      end

    | Prim prim ->
      Printf.printf "LIR: compile prim: %s\n" prim.prim_id;
      let ctx, reg = new_temp ctx ~ty:(Raw_type.of_type prim.prim_type) in
      add_op ctx @@ Prim {
        prim_rc = reg;
        prim_id = prim.prim_id;
      }

    | Var var ->
      add_temp_op ctx (Raw_type.of_type var.ty)
        ~f:(fun reg -> Var {
            var_from = find_named ctx var.name;
            var_to = reg })

    | Ref var ->
      Printf.printf "LIR: compile ref: %s\n" var.name;
      add_temp_op ctx (Raw_type.of_type var.type_)
        ~f:(fun reg -> Ref {
            ref_path = Value.path var;
            ref_to = reg })

    | Poly fn ->
      Printf.printf "LIR: compile poly: %s\n" (Namepath.to_string fn.poly_path);
      let raw_ty = Raw_type.of_type fn.poly_type in
      let ctx, box = add_poly ctx fn.poly_path raw_ty in
      add_temp_op ctx raw_ty 
        ~f:(fun reg -> Poly {
            poly_path = fn.poly_path;
            poly_box = box;
            poly_to = reg })

    | Ref_fun var ->
      Printf.printf "LIR: compile ref fun: %s\n" var.name;
      Printf.printf "type = %s\n" (Type.to_string var.ty);
      add_temp_op ctx (Raw_type.of_type var.ty)
        ~f:(fun reg -> Ref_fun {
            ref_fun_from = var.name;
            ref_fun_to = reg;
          })

    | Ref_prop prop ->
      Printf.printf "LIR: compile ref prop: %s: %s\n"
        prop.ref_prop_name (Type.to_string prop.ref_prop_ty);
      let ctx = compile_op ctx prop.ref_prop_obj in
      let obj_reg = ctx.rc in
      add_temp_op ctx (Raw_type.of_type prop.ref_prop_ty)
        ~f:(fun reg -> Ref_prop {
            ref_prop_from = obj_reg;
            ref_prop_name = prop.ref_prop_name;
            ref_prop_to = reg })

    | Int value ->
      Printf.printf "LIR: compile int\n";
      add_temp_op ctx Raw_type.Int
        ~f:(fun reg -> Int (reg, value))

    | String value ->
      Printf.printf "LIR: compile string\n";
      add_temp_op ctx Raw_type.String
        ~f:(fun reg -> String (reg, value))

    | Range range ->
      Printf.printf "LIR: compile range\n";
      let ctx = compile_op ctx range.range_begin in
      let begin_op = ctx.rc in
      let ctx = compile_op ctx range.range_end in
      let end_op = ctx.rc in
      add_temp_op ctx Raw_type.Range
        ~f:(fun reg -> Range {
            range_to = reg;
            range_begin = begin_op;
            range_end = end_op;
            range_kind = range.range_kind })

    | Fun clos ->
      Printf.printf "LIR: compile anon fun\n";
      let ctx, l_clos = compile_clos ctx clos in
      add_temp_op ctx l_clos.clos_type
        ~f:(fun reg -> Fun (reg, l_clos))

    | _ -> ctx

  and compile_block (ctx:Context.t) (ops:Hir.Op.t list) =
    List.fold_left ops
      ~init:ctx
      ~f:(fun ctx op -> compile_op ctx op)

  and compile_exps (ctx:Context.t) (ops:Hir.Op.t list) =
    let ctx, locals =
      List.fold_left ops
        ~init:(ctx, [])
        ~f:(fun (ctx, locals) op ->
            let ctx = compile_op ctx op in
            ctx, ctx.rc :: locals)
    in
    ctx, List.rev locals

  and compile_ptn ctx reg (ptn:Hir.Op.pattern) =
    Printf.printf "LIR: compile pattern\n";
    let open Context in
    match ptn with
    | Ptn_nop -> ctx
    | Ptn_bool v -> add_op ctx @@ Eq_bool (reg, v)
    | Ptn_int v -> add_op ctx @@ Eq_int (reg, v)
    | Ptn_float v -> add_op ctx @@ Eq_float (reg, v)
    | Ptn_string v -> add_op ctx @@ Eq_string (reg, v)
    | _ -> failwith "pattern not yet supported"

  let run (prog:Hir.Program.t) =
    let ctx = Context.create prog.file in
    let ctx, funs = List.fold_left prog.funs
        ~init:(ctx, [])
        ~f:(fun (ctx, accu) clos ->
            let ctx, clos' = compile_clos ctx clos in
            ctx, clos' :: accu) in
    let prog = Program.create
        ~src:prog.file
        ~out:((Filename.chop_extension prog.file) ^".go")
        ~pkg:None
        ~vars:[]
        ~funs
        ~polys:ctx.polys
        ~main:(Option.map ctx.main ~f:(fun reg -> reg.id))
        ~used_mods:prog.used_mods
    in
    Program.write prog

end
