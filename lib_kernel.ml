open Core.Std

let init () =
  Context.Spec.(define "kernel" ~init:true
                +> typ "unit" Type.unit
                +> typ "bool" Type.bool
                +> typ "int" Type.int
                +> typ "float" Type.float
                +> typ "string" Type.string
                +> typ "range" Type.range
                +> typ "list" Type.list_gen
                +> typ "option" Type.option_gen
                +> typ "box" Type.box_gen
                +> typ "stream" Type.stream
                +> fun_ "id" Type.Spec.(a @-> a)
                +> fun_ "print" Type.Spec.(string @-> string)
                +> fun_ "printf" Type.Spec.fun_printf
                +> string "version"
                |> end_);
  ()
