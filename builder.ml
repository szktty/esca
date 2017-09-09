open Core

type stat =
  | Vardef of vardef

and vardef = {
  vdef_ty : string option;
  vdef_name : string;
  vdef_exp : string;
}

type func = {
  fun_name : string;
  mutable fun_block : stat list;
}

type t = {
  mutable pkg : string option;
  mutable funs : func list;
  mutable main_fun : func;
  mutable cur_fun : func;
}

let create () =
  let main_fun = { fun_name = "main"; fun_block = [] } in
  { pkg = None;
    funs = [];
    main_fun;
    cur_fun = main_fun;
  }

let set_pkg bld name =
  bld.pkg <- Some name

let add_vardef bld ?ty name exp =
  ()

let begin_fun bld name =
  let new_fun = { fun_name = name; fun_block = [] } in
  bld.cur_fun <- new_fun;
  ()

let end_fun bld name =
  bld.cur_fun <- bld.main_fun;
  ()

let string s =
  (* TOOD: escape *)
  "\"" ^ s ^ "\""

let contents bld =
  let open Buffer in
  let buf = create 256 in
  contents buf 
