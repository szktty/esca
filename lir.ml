(* low-level intermediate representation *)

open Core

type register = string

type label = string

module Op = struct

  type t =
    | Nop
    | Comment of string list
    | Package of string
    | Import of string
    | Struct of (string * Type.t) list
    | Fundef of fundef
    | Vardef of (register * Type.t)
    | Defer of t

    | Move of move
    | Comp of register * register
    | Comp_int of register * int
    | Branch of branch
    | Jump of label
    | Return of register list
    | Label of label

    | Call of call
    | Block of t list
    | Terminal
    | Var of var
    | Prim of primitive
    | Null
    | Bool of bool
    | Int of register * int
    | Float of float
    | String of register * string

  and move = {
    mv_from : register;
    mv_to : register;
  }

  and branch = {
    br_cond : bool;
    br_dest : label;
  }

  and fundef = {
    fdef_name : string;
    fdef_params : var list;
    fdef_vars : var list;
    fdef_body : t list;
    fdef_ret : Raw_type.t;
  }

  and var = {
    var_reg : register;
    var_ty : Raw_type.t;
  }

  and call = {
    call_rc : register;
    call_ty : Raw_type.t;
    call_fun : register;
    call_args : register list;
  }

  and primitive = {
    prim_rc : register;
    prim_name : string;
    prim_ty : Raw_type.t;
  }

end

module Program = struct

  type t = {
    src : string;
    out : string;
    pkg : string option;
    vars : string list; (* TODO: var type *)
    top : Op.fundef;
    subs : Op.fundef list; (* TODO *)
  }

  let basic_pkgs = [
    "base";
    "lib/kernel";
  ]

  let create ~src ~out ~pkg ~vars ~top ~subs =
    { src; out; pkg; vars; top; subs }

  let ext = ".go"

  let bridge_prim_prefix = "EscaPrim"

  let bridge_prim_name name =
    bridge_prim_prefix ^ (Utils.camel_case name)

  let flag = "fr"

  let rec write (prog:t) =
    let open Buffer in
    let buf = Buffer.create 16 in
    write_pkg buf prog;
    write_import buf prog;
    write_fun buf prog.top;
    let code = contents buf in
    Printf.printf "golang = %s\n" (contents buf);
    Printf.printf "out = %s\n" prog.out;
    Out_channel.write_all prog.out ~data:code;
    ()

  and write_pkg buf prog =
    let open Buffer in
    add_string buf "package ";
    add_string buf @@ Option.value prog.pkg ~default:"main";
    add_string buf "\n\n";

  and write_import buf prog =
    let open Buffer in
    add_string buf "import (\n";
    List.iter basic_pkgs
      ~f:(fun pkg ->
          add_string buf @@ sprintf ". \"%s/%s\"\n" !Config.runlib_path pkg);
    add_string buf ")\n\n"

  and with_var buf name ty ~f =
    let open Buffer in
    add_string buf @@ sprintf "var %s %s = " name (Raw_type.to_string ty);
    f buf;
    add_string buf @@ sprintf "; var _ = %s\n" name

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
      addln @@ sprintf "%s = %s" mv.mv_to mv.mv_from

    | Branch br ->
      add @@ sprintf "if ";
      if not br.br_cond then begin
        add "!"
      end;
      addln @@ sprintf "fr {";
      addln @@ sprintf "goto %s" br.br_dest;
      addln "}"

    | Label label ->
      addln @@ sprintf "%s:" label

    | Comp_int (r, v) ->
      addln @@ sprintf "%s = %s == %d" flag r v;

      ()

    | Call call ->
      Printf.printf "call\n";
      with_var buf call.call_rc call.call_ty
        ~f:(fun _ ->
            add call.call_fun;
            add "(";
            add @@ String.concat ~sep:"," call.call_args;
            add ")")

    | Prim prim ->
      let bridge = bridge_prim_name prim.prim_name in
      with_var buf prim.prim_rc prim.prim_ty
        ~f:(fun _ -> add bridge)

    | Null -> add "null"

    | Int (rc, value) ->
      with_var buf rc Raw_type.Int
        ~f:(fun _ -> add @@ sprintf "%d" value)

    | String (rc, value) ->
      with_var buf rc Raw_type.String
        ~f:(fun _ -> add @@ sprintf "\"%s\"" value)

    | _ -> ()

  and write_ops buf ops =
    List.iter ops ~f:(write_op buf)

  and write_fun buf func =
    let open Buffer in
    add_string buf "func ";
    add_string buf func.fdef_name;
    add_string buf "() ";
    if func.fdef_name <> "main" then begin
      add_string buf @@ Raw_type.to_string func.fdef_ret;
    end;
    add_string buf " {\n";
    (* flag register *)
    with_var buf flag Raw_type.Bool
      ~f:(fun buf -> add_string buf "true");
    write_ops buf func.fdef_body;
    add_string buf "}\n";

end

module Context = struct

  type t = {
    src : string;
    mutable rev_ops : Op.t list;
    mutable reg_n : int;
    mutable rc_n : int;
    mutable rc : register;
    flag : register;
    vars : Op.var String.Map.t;
    mutable label_n : int;
    mutable label : label;
  }

  let create src =
    (*
    let out_file, exec_file =
      match String.rsplit2 src ~on:'.' with
      | None -> src ^ ext, in_file
      | Some (base, _) -> base ^ ext, base
    in
     *)
    { src;
      rev_ops = [];
      reg_n = 0;
      rc_n = 0;
      rc = "rc0";
      flag = "fr";
      vars = String.Map.empty;
      label_n = 0;
      label = "L0";
    }

  let new_reg ctx =
    let reg_n = ctx.reg_n in
    ctx.reg_n <- reg_n + 1;
    Printf.sprintf "r%d" reg_n

  let new_rc ctx =
    let rc_n = ctx.rc_n in
    ctx.rc_n <- rc_n + 1;
    ctx.rc <- Printf.sprintf "rc%d" rc_n;
    ctx.rc

  let new_label ctx =
    let label_n = ctx.label_n in
    ctx.label_n <- label_n + 1;
    ctx.label <- Printf.sprintf "L%d" label_n;
    ctx.label

  let add ctx op =
    ctx.rev_ops <- op :: ctx.rev_ops

  let ops ctx =
    List.rev ctx.rev_ops

  let move ctx from to_ =
    add ctx @@ Move { mv_from = from; mv_to = to_ }

  let add_var ctx (var:Hir.Closure.var) =
    let reg = new_reg ctx in
    let l_var = { Op.var_reg = var.var_id;
                  var_ty = Raw_type.of_type var.var_ty } in
    { ctx with vars = String.Map.add ctx.vars
                   ~key:var.var_id
                   ~data:l_var }, l_var

end

module Compiler = struct

  let rec compile_clos ctx (clos:Hir.Closure.t) =
    Printf.printf "LIR: compile closure\n";
    let open Context in
    let clos_ctx, vars = List.fold_left clos.vars
        ~init:(ctx, [])
        ~f:(fun (clos_ctx, vars) var ->
            let ctx, var = add_var clos_ctx var in
            ctx, var :: vars)
    in
    compile_ops clos_ctx clos.ops;
    (* TODO *)
    { Op.fdef_name = "main";
      fdef_params = [];
      fdef_vars = vars;
      fdef_body = Context.ops clos_ctx;
      fdef_ret = Raw_type.Void }

  and compile_op (ctx:Context.t) (op:Hir.Op.t) : unit =
    let open Context in
    match op with

    | Switch sw ->
      Printf.printf "LIR: compile switch\n";
      compile_op ctx sw.sw_val;
      let val_reg = ctx.rc in
      List.iter sw.sw_clss
        ~f:(fun cls ->
            compile_ptn ctx val_reg cls.sw_cls_ptn;
            let dest = new_label ctx in
            add ctx @@ Branch { br_cond = false; br_dest = dest };
            compile_ops ctx cls.sw_cls_action;
            add ctx @@ Label dest;
            ());
      ()

    | Call call ->
      Printf.printf "LIR: compile call\n";
      compile_op ctx call.call_fun;
      let f_rc = ctx.rc in
      let rev_rcs = compile_fold ctx
          call.call_args
          ~init:[]
          ~f:(fun rcs _ ->
              Printf.printf "call arg: %s\n" ctx.rc;
              ctx.rc :: rcs)
      in
      add ctx @@ Call { call_rc = new_rc ctx;
                        call_ty = Raw_type.of_type call.call_ty;
                        call_fun = f_rc;
                        call_args = List.rev rev_rcs }

    | Prim prim ->
      add ctx @@ Prim {
        prim_rc = new_rc ctx;
        prim_name = prim.prim_name;
        prim_ty = Raw_type.of_type prim.prim_ty;
      }

    | Var var ->
      Printf.printf "compile var\n";
      add ctx @@ Var {
        var_reg = new_rc ctx;
        var_ty = Raw_type.of_type var.var_ty;
      }

    | Int value ->
      add ctx @@ Int (new_rc ctx, value)

    | String value ->
      add ctx @@ String (new_rc ctx, value)

    | _ -> ()

  and compile_iter (ctx:Context.t) (ops:Hir.Op.t list) ~f =
    List.iter ops ~f:(fun op ->
        compile_op ctx op;
        f op)

  and compile_ops (ctx:Context.t) (ops:Hir.Op.t list) =
    compile_iter ctx ops ~f:ignore

  and compile_fold (ctx:Context.t)
      (ops:Hir.Op.t list)
      ~(init:'a)
      ~(f:('a -> Hir.Op.t -> 'a)) : 'a =
    List.fold_left ops
      ~init
      ~f:(fun accu op ->
          compile_op ctx op;
          f accu op)

  and compile_ptn ctx reg (ptn:Hir.Op.pattern) =
    Printf.printf "LIR: compile pattern\n";
    let open Context in
    match ptn with
    | Ptn_nop -> ()
    | Ptn_int v -> add ctx @@ Comp_int (reg, v)
    | _ -> failwith "pattern not yet supported"

  let run (prog:Hir.Program.t) =
    let ctx = Context.create prog.file in

    (* top-level *)
    let top = compile_clos ctx prog.clos in
    let prog = Program.create
        ~src:prog.file
        ~out:((Filename.chop_extension prog.file) ^".go")
        ~pkg:None
        ~vars:[]
        ~top
        ~subs:[]
    in
    Program.write prog;

    (* compile block *)
(*
    compile_op ctx op;
    let buf = Buffer.create 1000 in
    write_ops buf (ops ctx);
    Printf.printf "write = %s\n" (Buffer.contents buf);
    Out_channel.with_file ctx.out_file
      ~f:(fun chan -> Out_channel.output_string chan (Buffer.contents buf));
     *)
    ()

end
