(* low-level intermediate representation *)

open Core

module Register = struct

  type id = string

  type t = {
    id : string;
    ty : Raw_type.t;
  }

  let create id ty = { id; ty }

  let ids regs =
    List.map regs ~f:(fun reg -> reg.id)

end

type label = string

module Op = struct

  type t =
    | Nop
    | Comment of string list
    | Package of string
    | Import of string
    | Struct of (string * Type.t) list
    | Fundef of fundef
    | Vardef of (Register.t * Type.t)
    | Defer of t

    | Move of move
    | Eq of Register.t * Register.t
    | Eq_void of Register.t
    | Eq_bool of Register.t * bool
    | Eq_int of Register.t * int
    | Eq_float of Register.t * float
    | Eq_string of Register.t * string
    | Branch of branch
    | Jump of label
    | Return of Register.t list
    | Label of label

    | Call of call
    | Block of t list
    | Terminal
    | Var of var
    | Prim of primitive
    | Null
    | Bool of bool
    | Int of Register.t * int
    | Float of float
    | String of Register.t * string

  and move = {
    mv_from : Register.t;
    mv_to : Register.t;
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
    var_reg : Register.t;
  }

  and call = {
    call_rc : Register.t;
    call_fun : Register.t;
    call_args : Register.t list;
  }

  and primitive = {
    prim_rc : Register.t;
    prim_name : string;
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

  and decl_var buf name ty =
    let open Buffer in
    add_string buf @@ sprintf "var %s %s\n" name (Raw_type.to_string ty);
    add_string buf @@ sprintf "var _ = %s\n" name

  and with_exp buf name ~f =
    let open Buffer in
    add_string buf @@ sprintf "%s = " name;
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
      addln @@ sprintf "fr {";
      addln @@ sprintf "goto %s" br.br_dest;
      addln "}"

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

    | Call call ->
      Printf.printf "call\n";
      with_exp buf call.call_rc.id
        ~f:(fun _ ->
            add call.call_fun.id;
            add "(";
            add @@ String.concat ~sep:"," (Register.ids call.call_args);
            add ")")

    | Prim prim ->
      let bridge = bridge_prim_name prim.prim_name in
      with_exp buf prim.prim_rc.id ~f:(fun _ -> add bridge)

    | Null -> add "null"

    | Int (reg, value) ->
      with_exp buf reg.id ~f:(fun _ -> add @@ sprintf "%d" value)

    | String (reg, value) ->
      with_exp buf reg.id ~f:(fun _ -> add @@ sprintf "\"%s\"" value)

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

    (* flag Register.id *)
    decl_var buf flag Raw_type.Bool;

    (* declare variables to avoid "goto" error ("jumps over declaration") *)
    List.iter func.fdef_vars
      ~f:(fun var ->
          add_string buf @@ Printf.sprintf "var %s %s\nvar _ = %s\n"
            var.var_reg.id
            (Raw_type.to_string var.var_reg.ty)
            var.var_reg.id);
    add_string buf "\n";

    write_ops buf func.fdef_body;
    add_string buf "}\n";

end

module Context = struct

  type t = {
    src : string;
    mutable rev_ops : Op.t list;
    mutable regs : Register.t list;
    mutable rc : Register.t;
    flag : Register.id;
    mutable labels : label list;
  }

  let create src =
    let r0 = Register.create "r0" Raw_type.Void in
    let l0 = "L0" in
    { src;
      rev_ops = [];
      regs = [r0];
      rc = r0;
      flag = "fr";
      labels = [l0];
    }

  let add_reg ctx reg =
    ctx.regs <- reg :: ctx.regs

  let new_reg ctx ty =
    let n = List.length ctx.regs in
    let id = Printf.sprintf "r%d" n in
    let reg = Register.create id ty in
    ctx.regs <- reg :: ctx.regs;
    ctx.rc <- reg;
    reg

  let new_label ctx =
    let n = List.length ctx.labels in
    let label = Printf.sprintf "L%d" n in
    ctx.labels <- label :: ctx.labels;
    label

  let add ctx op =
    ctx.rev_ops <- op :: ctx.rev_ops

  let ops ctx =
    List.rev ctx.rev_ops

  let move ctx from to_ =
    add ctx @@ Move { mv_from = from; mv_to = to_ }

end

module Compiler = struct

  let rec compile_clos ctx (clos:Hir.Closure.t) =
    Printf.printf "LIR: compile closure\n";
    let open Context in
    compile_ops ctx clos.ops;
    let vars = List.rev_map ctx.regs
        ~f:(fun reg -> { Op.var_reg = reg }) in
    { Op.fdef_name = "main";
      fdef_params = [];
      fdef_vars = vars;
      fdef_body = Context.ops ctx;
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
              Printf.printf "call arg: %s\n" ctx.rc.id;
              ctx.rc :: rcs)
      in
      let raw_ty = Raw_type.of_type call.call_ty in
      add ctx @@ Call { call_rc = new_reg ctx raw_ty;
                        call_fun = f_rc;
                        call_args = List.rev rev_rcs }

    | Prim prim ->
      add ctx @@ Prim {
        prim_rc = new_reg ctx (Raw_type.of_type prim.prim_ty);
        prim_name = prim.prim_name;
      }

    | Var var ->
      Printf.printf "compile var\n";
      add ctx @@ Var {
        var_reg = new_reg ctx (Raw_type.of_type var.var_ty);
      }

    | Int value ->
      add ctx @@ Int (new_reg ctx Raw_type.Int, value)

    | String value ->
      add ctx @@ String (new_reg ctx Raw_type.String, value)

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
    | Ptn_void -> add ctx @@ Eq_void reg
    | Ptn_bool v -> add ctx @@ Eq_bool (reg, v)
    | Ptn_int v -> add ctx @@ Eq_int (reg, v)
    | Ptn_float v -> add ctx @@ Eq_float (reg, v)
    | Ptn_string v -> add ctx @@ Eq_string (reg, v)
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
