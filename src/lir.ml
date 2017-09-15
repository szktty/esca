(* low-level intermediate representation *)

open Core

module Register = struct

  type id = string

  type t = {
    id : id;
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
    | Ref of ref_var
    | Prim of primitive
    | Null
    | Bool of bool
    | Int of Register.t * int
    | Float of float
    | String of Register.t * string

  and context = {
    ctx_src : string;
    ctx_ops : t list;
    ctx_regs : Register.t list;
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

  and fundef = {
    fdef_ctx : context;
    fdef_name : string;
    fdef_params : var list;
    fdef_vars : var list;
    fdef_body : t list;
    fdef_ret : Raw_type.t;
  }

  and var = {
    var_reg : Register.t;
  }

  and ref_var = {
    ref_var : Register.t;
    ref_name : string; (* TODO: namepath *)
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
    funs : Op.fundef list;
    main : string option;
  }

  let basic_pkgs = [
    "base";
    "lib/kernel";
  ]

  let create ~src ~out ~pkg ~vars ~funs ~main =
    { src; out; pkg; vars; funs; main }

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
    List.iter prog.funs ~f:(write_fun buf);
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

  and write_import buf _prog =
    let open Buffer in
    add_string buf "import (\n";
    List.iter basic_pkgs
      ~f:(fun pkg ->
          add_string buf @@ sprintf ". \"%s/%s\"\n" !Config.runlib_path pkg);
    add_string buf ")\n\n"

  and write_main buf prog =
    let open Buffer in
    Option.iter prog.main ~f:(fun reg ->
        add_string buf @@ sprintf "func main() { %s() }" reg)

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

    | Ref ref ->
      addln @@ sprintf "%s = %s" ref.ref_var.id ref.ref_name

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
    add_string buf "}\n\n";

end

module Context = struct

  type t = {
    src : string;
    ops : Op.t list;
    regs : Register.t list;
    rc : Register.t;
    flag : Register.id;
    labels : label list;
    main : Register.t option; (* TODO: unused? *)
  }

  let create src =
    (* dummy *)
    let r0 = Register.create "r0" Raw_type.Void in
    { src;
      ops = [];
      regs = [];
      rc = r0;
      flag = "fr";
      labels = [];
      main = None;
    }

  let add_reg ctx reg =
    { ctx with regs = reg :: ctx.regs }

  let new_reg ~prefix ctx ty =
    let n = List.length ctx.regs in
    let id = Printf.sprintf "%s%d" prefix n in
    let reg = Register.create id ty in
    let ctx = add_reg ctx reg in
    { ctx with rc = reg }, reg

  let new_var_reg ctx ty =
    new_reg ~prefix:"t" ctx ty

  let set_main ctx reg =
    { ctx with main = Some reg }

  let new_label ctx =
    let n = List.length ctx.labels in
    let label = Printf.sprintf "L%d" n in
    { ctx with labels = label :: ctx.labels }, label

  let add_op ctx op =
    { ctx with ops = op :: ctx.ops }

  let add_op_with_reg ctx ty ~f =
    let ctx, reg = new_var_reg ctx ty in
    add_op ctx @@ f reg

  let move ctx from to_ =
    add_op ctx @@ Move { mv_from = from; mv_to = to_ }

  let finish ctx =
    { ctx with ops = List.rev ctx.ops }

  let to_clos_ctx ctx : Op.context =
    { ctx_src = ctx.src;
      ctx_ops = ctx.ops;
      ctx_regs = ctx.regs;
      ctx_flag = ctx.flag }

end

module Compiler = struct

  let rec compile_clos ctx (clos:Hir.Closure.t) : Context.t * Op.fundef =
    Printf.printf "LIR: compile closure '%s'\n" clos.var.id;
    let open Context in
    let clos_ctx = compile_block ctx clos.block in
    Printf.printf "ops: %d\n" (List.length clos_ctx.ops);
    let vars = List.rev_map clos_ctx.regs
        ~f:(fun reg -> { Op.var_reg = reg }) in
    let clos_ctx = finish clos_ctx in
    let fdef = { Op.fdef_ctx = to_clos_ctx clos_ctx;
                 fdef_name = clos.name;
                 fdef_params = [];
                 fdef_vars = vars;
                 fdef_body = clos_ctx.ops;
                 fdef_ret = Raw_type.Void } in
    ctx, fdef

  and compile_op (ctx:Context.t) (op:Hir.Op.t) : Context.t =
    let open Context in
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

    | Call call ->
      Printf.printf "LIR: compile call\n";
      let ctx = compile_op ctx call.call_fun in
      let fun_reg = ctx.rc in
      Printf.printf "LIR: call fun %s\n" fun_reg.id;
      let ctx, arg_regs = compile_exps ctx call.call_args in
      add_op_with_reg ctx fun_reg.ty
        ~f:(fun reg -> Call { call_rc = reg;
                              call_fun = fun_reg;
                              call_args = arg_regs })

    | Prim prim ->
      let ctx, reg = new_var_reg ctx (Raw_type.of_type prim.prim_ty) in
      add_op ctx @@ Prim {
        prim_rc = reg;
        prim_name = prim.prim_name;
      }

    | Var var ->
      Printf.printf "LIR: compile var: %s\n" var.id;
      add_op_with_reg ctx (Raw_type.of_type var.ty)
        ~f:(fun reg -> Var { var_reg = reg })

    | Ref ref ->
      Printf.printf "LIR: compile ref: %s\n" ref.ref_name;
      add_op_with_reg ctx (Raw_type.of_type ref.ref_var.ty)
        ~f:(fun reg -> Ref { ref_var = reg; ref_name = ref.ref_name })

    | Int value ->
      add_op_with_reg ctx Raw_type.Int
        ~f:(fun reg -> Int (reg, value))

    | String value ->
      Printf.printf "LIR: compile string\n";
      add_op_with_reg ctx Raw_type.String
        ~f:(fun reg -> String (reg, value))

    | _ -> ctx

  and compile_block (ctx:Context.t) (ops:Hir.Op.t list) =
    List.fold_left ops
      ~init:ctx
      ~f:(fun ctx op -> compile_op ctx op)

  and compile_exps (ctx:Context.t) (ops:Hir.Op.t list) =
    let ctx, regs =
      List.fold_left ops
        ~init:(ctx, [])
        ~f:(fun (ctx, regs) op ->
            let ctx = compile_op ctx op in
            ctx, ctx.rc :: regs)
    in
    ctx, List.rev regs

  and compile_ptn ctx reg (ptn:Hir.Op.pattern) =
    Printf.printf "LIR: compile pattern\n";
    let open Context in
    match ptn with
    | Ptn_nop -> ctx
    | Ptn_void -> add_op ctx @@ Eq_void reg
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
        ~main:(Option.map ctx.main ~f:(fun reg -> reg.id))
    in
    Program.write prog

end
