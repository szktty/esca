open Core.Std
open Located
open Type
open Logging

type unity_exn = {
  uniexn_ex : Type.t;
  uniexn_ac : Type.t;
}

type mismatch = {
  mismatch_node: Ast.t;
  mismatch_ex : Type.t;
  mismatch_ac : Type.t;
}

exception Unify_error of unity_exn
exception Type_mismatch of mismatch
exception Deref_error of Type.t * string

let fail_unify ex ac = 
  raise (Unify_error { uniexn_ex = ex; uniexn_ac = ac })

let tyvar_names = [|
  "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m"; "n";
  "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"
|]

module Closure = struct

  type t = {
    mutable ret_ty: Type.t option;
  }

  let create () =
    { ret_ty = None }

end

module Tyexp = struct

        (*
and tyexp_desc =
  | Ty_var of text
  | Ty_namepath of text Namepath.t
  | Ty_app of tyexp * tyexp list
  | Ty_list of tyexp
  | Ty_tuple of tyexp list
  | Ty_option of tyexp
         *)

  (* TODO: attr kind *)
  let find_type env (path:Ast.text Namepath.t) =
    (* TODO: prefix *)
    match Env.find env path.name.desc with
    | None -> None
    | Some attr -> match attr.kind with
      | `Type -> Some attr.type_
      | _ -> failwith "not type"

  let to_type (env:Env.t) (node:Ast.tyexp) : Type.t option =
    match node.desc with
    | Ty_namepath path -> find_type env path
    | _ -> Some Type.void

end

let rec generalize (ty:Type.t) : Type.t =
  let tyvars : (metavar * tyvar) list ref = Ref.create [] in

  let new_tyvar () =
    Array.get tyvar_names @@ List.length !tyvars
  in

  let rec walk ty : t =
    let gen = match ty.desc with
      | App (tycon, args) ->
        let gen_tycon = match tycon with
          | Tycon_tyfun (tyvars, ty) -> Tycon_tyfun (tyvars, generalize ty)
          | tycon -> tycon
        in
        App (gen_tycon, List.map args ~f:walk)
      | Var tyvar -> Var tyvar
      | Meta ({ contents = None } as ref) ->
        let tyvar = match List.Assoc.find !tyvars ref ~equal:phys_equal with
          | Some tyvar -> tyvar
          | None ->
            let tyvar = new_tyvar () in
            tyvars := (ref, tyvar) :: !tyvars;
            tyvar
        in
        Var tyvar
      | Meta ({ contents = Some ty }) -> (walk ty).desc
      | Poly ({ contents = `Preunify (tyvars, ty) } as ref) ->
        ref := `Preunify (tyvars, walk ty);
        Poly ref
      | Poly _ -> ty.desc
      | Prim prim ->
        Prim { prim with prim_type = walk prim.prim_type }
      | Unique (ty, path) -> Unique (walk ty, path)
    in
    Located.create ty.loc gen
  in

  let gen = walk ty in
  if List.length !tyvars = 0 then
    gen
  else begin
    let tyvars = List.rev_map !tyvars ~f:(fun (_, tyvar) -> tyvar) in
    Located.create ty.loc @@ Type.poly tyvars gen
  end

let rec subst (ty:Type.t) (env:(tyvar * t) list) =
  match ty.desc with
  | Var tyvar ->
    Option.value (List.Assoc.find env ~equal:(=) tyvar) ~default:ty

  | App (Tycon_tyfun (tyvars, ty'), args) ->
    let env' = List.map2_exn tyvars args
        ~f:(fun tyvar ty -> (tyvar, ty)) in
    subst (subst ty' env') env

  | App (tycon, args) ->
    let args' = List.map args
        ~f:(fun ty' -> subst ty' env) in
    Located.create ty.loc @@ App (tycon, args')

  | Poly ({ contents = `Preunify (tyvars, ty') } as ref) ->
    let tyvars' = List.mapi tyvars
        ~f:(fun i _tyvar ->
            Array.get Type.var_names (i + List.length env))
    in
    let ty'' = subst ty' @@ List.map2_exn tyvars tyvars'
        ~f:(fun tyvar tyvar' ->
            (tyvar, Located.create ty.loc @@ Var tyvar'))
    in
    ref := `Unify (subst ty'' env);
    ty

  | Poly _ -> ty

  | Prim prim ->
    Located.create ty.loc @@
    Prim { prim with prim_type = subst prim.prim_type env }

  | Unique (ty, path) ->
    Located.create ty.loc @@ Unique (subst ty env, path)

  | Meta { contents = Some ty' } ->
    Option.value_map (List.find env ~f:(fun (_, ty) -> ty = ty'))
      ~f:(fun (_, ty') -> subst ty' env)
      ~default:ty

  | Meta { contents = None } -> ty

let instantiate (ty:Type.t) =
  match ty.desc with
  | Poly ({ contents = `Preunify (tyvars, ty') } as ref) ->
    let env = List.map tyvars
        ~f:(fun tyvar ->
            (tyvar, metavar ty.loc)) in
    ref := `Unify (subst ty' env);
    ty
  | _ -> ty

let rec occur (ref:t option ref) (ty:Type.t) : bool =
  match ty.desc with
  | App (tycon, args) ->
    begin match tycon with
      | Tycon_void
      | Tycon_bool
      | Tycon_int
      | Tycon_float
      | Tycon_string
      | Tycon_range
      | Tycon_stream ->
        false
      | Tycon_list
      | Tycon_tuple
      | Tycon_option
      | Tycon_ref
      | Tycon_fun ->
        List.exists args ~f:(occur ref)
      | Tycon_method recv ->
        List.exists (recv :: args) ~f:(occur ref)
      | Tycon_tyfun (_, ty) ->
        occur ref ty
      | _ -> failwith "not impl"
    end
  | Prim { prim_type } -> occur ref prim_type
  | Meta ref2 when phys_equal ref ref2 -> true
  | Meta { contents = None } -> false
  | Meta { contents = Some t2 } -> occur ref t2
  | _ -> failwith "not impl"

let rec unify ~(ex:Type.t) ~(ac:Type.t) : unit =
  Printf.printf "unify %s and %s\n" (Type.to_string ex) (Type.to_string ac);
  match ex.desc, ac.desc with
  | App (Tycon_void, []), App (Tycon_void, [])
  | App (Tycon_bool, []), App (Tycon_bool, [])
  | App (Tycon_int, []), App (Tycon_int, [])
  | App (Tycon_float, []), App (Tycon_float, [])
  | App (Tycon_string, []), App (Tycon_string, [])
  | App (Tycon_range, []), App (Tycon_range, []) -> ()

  | App (Tycon_list, [ex]), App (Tycon_list, [ac])
  | App (Tycon_option, [ex]), App (Tycon_option, [ac])
  | App (Tycon_ref, [ex]), App (Tycon_ref, [ac]) -> unify ~ex ~ac

  | App (Tycon_tuple, exs), App (Tycon_tuple, acs)
  | App (Tycon_fun, exs), App (Tycon_fun, acs)
    when List.length exs = List.length acs ->
    List.iter2_exn exs acs ~f:(fun ex ac -> unify ~ex ~ac)

  (* TODO *)
  | App (Tycon_printf, []), App (Tycon_printf, [])
  | App (Tycon_printf, []), App (Tycon_fun, _) -> ()

  | App (Tycon_method _, exs), App (Tycon_method _, acs)
  | App (Tycon_fun, exs), App (Tycon_method _, acs)
  | App (Tycon_method _, exs), App (Tycon_fun, acs)
    when List.length exs = List.length acs ->
    List.iter2_exn exs acs ~f:(fun ex ac -> unify ~ex ~ac)

  | Poly { contents = `Preunify _ }, _ -> unify ~ex:(instantiate ex) ~ac;
  | _, Poly { contents = `Preunify _ } -> unify ~ex ~ac:(instantiate ac)
  | Poly { contents = `Unify ex }, _ -> unify ~ex ~ac
  | _, Poly { contents = `Unify ac } -> unify ~ex ~ac

  | Prim { prim_type }, _ -> unify ~ex:prim_type ~ac
  | _, Prim { prim_type } -> unify ~ex ~ac:prim_type

  | Meta ex, Meta ac when phys_equal ex ac -> ()
  | Meta { contents = Some ex }, _ -> unify ~ex ~ac
  | _, Meta { contents = Some ac } -> unify ~ex ~ac

  | Meta ({ contents = None } as ref), _ ->
    if occur ref ac then
      fail_unify ex ac
    else
      ref := Some ac

  | _, Meta ({ contents = None } as ref) ->
    if occur ref ex then
      fail_unify ex ac
    else
      ref := Some ex

  | _, _ when ex = ac -> ()
  | _, _ ->
    raise (Unify_error { uniexn_ex = ex; uniexn_ac = ac })

let owner = Ref.create (Module.create "dummy")

let rec infer (e:Ast.t) 
    ~(clos:Closure.t)
    ~(env:Env.t) : (Env.t * Type.t) =
  Printf.printf "infer e: ";
  Ast.print e;
  let loc = Ast.location e in
  try
    let env, desc = match e with
      | `Nop _ -> (env, desc_void)

      | `Chunk chunk -> 
        let env = List.fold_left chunk.ch_stats
            ~init:env
            ~f:(fun env e -> fst @@ infer e ~clos ~env)
        in
        (env, desc_void)

      | `Import_attr _ -> env, void.desc

      | `Return e ->
        let ty = Option.value_map e
            ~default:Type.void
            ~f:(fun e -> easy_infer e ~clos ~env)
        in
        begin match clos.ret_ty with
          | None -> clos.ret_ty <- Some ty
          | Some ret -> unify ~ex:ret ~ac:ty
        end;
        env, void.desc

      | `If if_ -> 
        let unify_action block =
          List.iter block ~f:(fun stat ->
              unify ~ex:Type.void ~ac:(easy_infer stat ~clos ~env))
        in

        List.iter if_.if_actions
          ~f:(fun (cond, block) ->
              unify ~ex:Type.bool ~ac:(easy_infer cond ~clos ~env);
              unify_action block);
        unify_action if_.if_else;
        (env, desc_void)

      | `For for_ ->
        unify ~ex:Type.range ~ac:(easy_infer ~clos ~env for_.for_range);
        let var = Value.local for_.for_var.desc ~kind:`Value ~type_:Type.int in
        let env = Env.add env var in
        let _, block_ty = infer_block ~clos ~env for_.for_block in
        unify ~ex:Type.void ~ac:block_ty;
        (env, Type.void.desc)

      | `Void _ -> (env, desc_void)
      | `Bool _ -> (env, desc_bool)
      | `Int _ -> (env, desc_int)
      | `Float _ -> (env, desc_float)
      | `String _ -> (env, desc_string)

      | `List es ->
        begin match es with
          | [] -> (env, desc_list @@ metavar_some loc)
          | e :: es ->
            let base_ty = easy_infer ~clos ~env e in
            List.iter es ~f:(fun e ->
                unify ~ex:base_ty ~ac:(easy_infer ~clos ~env e));
            (env, desc_list @@ base_ty)
        end

      | `Tuple es ->
        (env, desc_tuple (List.map es ~f:(fun e -> easy_infer ~clos ~env e)))

      | `Range range ->
        unify ~ex:Type.int ~ac:(easy_infer range.range_begin ~clos ~env);
        unify ~ex:Type.int ~ac:(easy_infer range.range_end ~clos ~env);
        (env, desc_range)

      | `Ref (e, ty) ->
        unify ~ex:ty ~ac:(Type.ref (easy_infer e ~clos ~env));
        (env, ty.desc)

      | `Vardef vdef ->
        let exp_ty = easy_infer ~clos ~env vdef.vdef_exp in
        let env, ptn_ty = infer_ptn ~clos ~env vdef.vdef_ptn in
        unify ~ex:ptn_ty ~ac:exp_ty;
        (env, ptn_ty.desc)

      | `Fundef fdef ->
        let params = List.map fdef.fdef_params
            ~f:(fun param -> Type.metavar param.loc)
        in

        (* TODO: return type declaration *)
        let ret = match fdef.fdef_ret with
          | None -> Type.void
          | Some exp ->
            Printf.printf "find type: ";
            Ast.print_tyexp exp;
            match Tyexp.to_type env exp with
            | None -> failwith "type not found"
            | Some ty -> ty
        in
        let clos' = Closure.create () in
        clos'.ret_ty <- Some ret;

        let fun_ty = Type.fun_ (Some loc) params ret in
        unify ~ex:fdef.fdef_type ~ac:fun_ty;

        (* for recursive call *)
        let fun_var = Value.local fdef.fdef_name.desc
            ~kind:`Value
            ~type_:fun_ty in
        let env = Env.add env fun_var in
        let fenv = List.fold2_exn fdef.fdef_params params ~init:env
            ~f:(fun env name type_ ->
                Env.add env (Value.local_value name.desc ~type_))
        in
        let _, ret' = infer_block fdef.fdef_block ~clos:clos' ~env:fenv in
        (*unify ~ex:ret ~ac:ret';*)
        let clos_ret = Option.value clos'.ret_ty ~default:Type.void in
        unify ~ex:ret ~ac:clos_ret;
        (env, (generalize fun_ty).desc)

      | `Funcall call ->
        Printf.printf "# funcall ";
        Ast.print e;
        let ex_fun = easy_infer ~clos ~env call.fc_fun in
        unify ~ex:call.fc_fun_type ~ac:ex_fun;

        Printf.printf "# funcall infer ex: %s\n" (Type.to_string ex_fun);

        if Type.equal ex_fun Type.fun_printf then begin
          Printf.printf "# typing printf\n";
          match call.fc_args with
          | [] -> failwith "no format string"
          | fmt_s :: args ->
            match fmt_s with
            | `String fmt_s ->
              let args = List.map args ~f:(fun e -> easy_infer ~clos ~env e) in
              let params = Type.parse_format fmt_s.desc in
              if List.length args <> List.length params then
                failwith "numbers of printf arguments are not matched"
              else begin
                List.iter2_exn params args
                  ~f:(fun param arg -> unify ~ex:param ~ac:arg);
                Printf.printf "# printf ok\n";
                (env, Type.desc_void)
              end
            | _ -> (env, Type.desc_void)

        end else begin
          let args = List.map call.fc_args ~f:(fun e -> easy_infer ~clos ~env e) in
          let ret = Type.metavar_some loc in
          let ac_fun = Type.create (Some loc) (desc_fun args ret) in
          unify ~ex:ex_fun ~ac:ac_fun;
          Printf.printf "# end funcall infer\n";
          (env, (Type.fun_return ex_fun).desc)
        end

      | `Switch sw ->
        let match_ty = easy_infer ~clos ~env sw.sw_val in
        let val_ty = Type.metavar_some loc in
        List.iter sw.sw_cls ~f:(fun cls ->
            infer_sw_cls ~clos ~env match_ty val_ty cls);
        (env, val_ty.desc)

      | `Var var ->
        let name = var.var_name.desc in
        let ty = match Ast.(var.var_prefix) with
          | Some prefix ->
            let ty = easy_infer ~clos ~env prefix in
            begin match (Type.unwrap ty).desc with
              | App (Tycon_module mname, _) ->
                begin match Library.find_module mname with
                  | None -> failwith (sprintf "unknown module %s" mname)
                  | Some m ->
                    let aname = var.var_name.desc in
                    match Module.find_attr m aname with
                    | None -> failwith ("module attribute is not found: " ^ aname)
                    | Some attr ->
                      Module.use m;
                      var.var_value <- Some attr;
                      attr.type_
                end
              | _ ->
                match Property.find ty name with
                | None -> failwith @@ Printf.sprintf "property %s not found" name
                | Some ty -> ty
            end
          | None ->
            match Env.find env name with
            | None -> failwith ("variable is not found: " ^ name)
            | Some attr ->
              var.var_value <- Some attr;
              attr.type_
        in
        unify ~ex:var.var_type ~ac:ty;
        env, ty.desc

      | `Unexp exp ->
        let op_ty, val_ty = match exp.unexp_op.desc with
          | `Pos | `Neg -> (Type.int, Type.int)
          | _ -> failwith "not yet supported"
        in
        unify ~ex:op_ty ~ac:(easy_infer ~clos ~env e);
        (env, val_ty.desc)

      | `Binexp { binexp_left = e1; binexp_op = op; binexp_right = e2 } ->
        let op_ty, val_ty = match op.desc with
          | `Eq | `Ne ->
            (easy_infer ~clos ~env e1, Type.bool)
          | `And | `Or ->
            (Type.bool, Type.bool)
          | `Lt | `Le | `Gt | `Ge ->
            (Type.int, Type.bool)
          | `Add | `Sub | `Mul | `Div
          | `Pow | `Mod | `Lcomp | `Rcomp ->
            (Type.int, Type.int)
          | _ -> failwith "not yet supported"
        in
        unify ~ex:op_ty ~ac:(easy_infer ~clos ~env e1);
        unify ~ex:op_ty ~ac:(easy_infer ~clos ~env e2);
        (env, val_ty.desc)

      | `Fun fn ->
        infer_fun_body fn.fun_body ~env ~loc ~block_type:Type.void

      | _ -> Ast.print e; failwith "TODO"

    in
    let ty = Type.create (Some loc) desc in
    Printf.printf "inferred node: ";
    Ast.print e;
    Printf.printf "inferred type: %s\n" (Type.to_string ty);
    (env, ty)
  with
  | Unify_error { uniexn_ex = ex; uniexn_ac = ac } ->
    raise @@ Type_mismatch {
      mismatch_node = e;
      mismatch_ex = generalize ex;
      mismatch_ac = generalize ac;
    }

and easy_infer (e:Ast.t) ~clos ~env : Type.t =
  snd @@ infer e ~clos ~env

and infer_block es ~clos ~env =
  List.fold_left es ~init:(env, Type.void)
    ~f:(fun (env, _) e -> infer e ~clos ~env)

and infer_fun_body
    ?(name:string option)
    (body:Ast.fun_body)
    ~env
    ~loc
    ~block_type
  : Env.t * Type.desc =
  (* parameters *)
  let fenv, param_tys =
    List.fold_right body.fbody_params
      ~init:(env, [])
      ~f:(fun param (env, tys) ->
          let ty = Type.metavar param.loc in
          Env.add env (Value.local_value param.desc ~type_:ty), ty :: tys)
  in

  (* return type *)
  let ret_ty = match body.fbody_ret with
    | None -> block_type
    | Some exp ->
      Printf.printf "find type: ";
      Ast.print_tyexp exp;
      match Tyexp.to_type fenv exp with
      | None -> failwith "type not found"
      | Some ty -> ty
  in
  let clos' = Closure.create () in
  clos'.ret_ty <- Some ret_ty;

  (* generate function type *)
  let fun_ty = Type.fun_ (Some loc) param_tys ret_ty in
  unify ~ex:body.fbody_type ~ac:fun_ty;

  (* add the function to environment for recursive call *)
  let fenv = match name with
    | None -> fenv
    | Some name ->
      let fun_var = Value.local name
          ~kind:`Value
          ~type_:fun_ty in
      Env.add fenv fun_var
  in

  (* block *)
  let _, ret_ty' = infer_block body.fbody_block ~clos:clos' ~env:fenv in
  unify ~ex:ret_ty ~ac:ret_ty';

  (env, (generalize fun_ty).desc)

and infer_sw_cls ~clos ~env match_ty val_ty (cls:Ast.switch_cls) =
  (* pattern *)
  let env, ptn_ty = infer_ptn ~clos ~env cls.sw_cls_ptn in
  unify ~ex:match_ty ~ac:ptn_ty;

  (* guard *)
  Option.iter cls.sw_cls_guard ~f:(fun guard ->
      unify ~ex:Type.bool ~ac:(easy_infer ~clos ~env guard));

  (* var *)
  let env = Option.value_map cls.sw_cls_var
      ~default:env
      ~f:(fun name -> Env.add env (Value.local_value name.desc ~type_:match_ty))
  in

  (* action *)
  let _, action_ty = infer_block ~clos ~env cls.sw_cls_action in
  unify ~ex:val_ty ~ac:action_ty

and infer_ptn ~clos ~env (ptn:Ast.pattern) =
  match ptn.ptn_cls with
  | `Nop _ -> (env, Type.void)
  | `Bool _ -> (env, Type.bool)
  | `Int _ -> (env, Type.int)
  | `Float _ -> (env, Type.float)
  | `String _ -> (env, Type.string)

  | `Var name ->
    let ty = Type.metavar name.loc in
    Env.add env (Value.local_value name.desc ~type_:ty), ty

  | `List elts ->
    let ty = Type.metavar_some @@ Ast.location ptn.ptn_cls in
    let env = List.fold_left elts
        ~init:env
        ~f:(fun env elt ->
            let env, elt_ty = infer_ptn ~clos ~env elt in
            unify ~ex:ty ~ac:elt_ty;
            env)
    in
    (env, Type.list ty)

  | `Tuple elts ->
    let env, rev_tys = List.fold_left elts
        ~init:(env, [])
        ~f:(fun (env, accu) elt ->
            let env, elt_ty = infer_ptn ~clos ~env elt in
            (env, elt_ty :: accu))
    in
    (env, Type.tuple (List.rev rev_tys))

  | _ -> failwith "notimpl"

let run mod_ (node:Ast.t) : Module.t =
  verbose "begin typing";
  owner := mod_;
  ignore @@ infer node
    ~clos:(Closure.create ())
    ~env:(Library.root_env ());
  verbose "end typing";
  !owner
