(* low-level intermediate representation *)

open Core

type register = string

module Op = struct

  type t =
    | Nop
    | Comment of string list
    | Package of string
    | Import of string
    | Struct of (string * Type.t) list
    | Fundef of fundef
    | Vardef of (register * Type.t)
    | Return of register list
    | Defer of t
    | Get of register
    | Put of (register * register)
    | Call of call
    | Block of t list
    | Terminal
    | Var of var
    | Prim of primitive
    | Null
    | Bool of bool
    | Int of int
    | Float of float
    | String of register * string

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
    | Call call ->
      Printf.printf "call\n";
      add @@ sprintf "var %s %s = %s("
        call.call_rc
        (Raw_type.to_string call.call_ty)
        call.call_fun;
      add @@ String.concat ~sep:"," call.call_args;
      addln ")";

    | Prim prim ->
      let bridge = bridge_prim_name prim.prim_name in
      addln @@ sprintf "var %s %s = %s"
        prim.prim_rc
        (Raw_type.to_string prim.prim_ty)
        bridge

    | Null -> add "null"
    | String (rc, value) ->
      addln @@ sprintf "var %s string = \"%s\"" rc value

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
    vars : Op.var String.Map.t;
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
      vars = String.Map.empty;
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

  let add ctx op =
    ctx.rev_ops <- op :: ctx.rev_ops

  let ops ctx =
    List.rev ctx.rev_ops

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
    compile_iter clos_ctx clos.ops ~f:ignore;
    (* TODO *)
    { Op.fdef_name = "main";
      fdef_params = [];
      fdef_vars = vars;
      fdef_body = Context.ops clos_ctx;
      fdef_ret = Raw_type.Void }

  and compile_op (ctx:Context.t) (op:Hir.Op.t) : unit =
    let open Context in
    match op with
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

    | String value ->
      add ctx @@ String (new_rc ctx, value)

    | _ -> ()

  and compile_iter (ctx:Context.t)
      (ops:Hir.Op.t list)
      ~f =
    List.iter ops ~f:(fun op -> compile_op ctx op; f op)

  and compile_fold (ctx:Context.t)
      (ops:Hir.Op.t list)
      ~(init:'a)
      ~(f:('a -> Hir.Op.t -> 'a)) : 'a =
    List.fold_left ops
      ~init
      ~f:(fun accu op ->
          compile_op ctx op;
          f accu op)

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
