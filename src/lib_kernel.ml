open Core.Std

let init () =
  Context.Spec.(define "Kernel" ~init:true
                +> typ "Unit" Type.unit
                +> typ "Bool" Type.bool
                +> typ "Int" Type.int
                +> typ "Float" Type.float
                +> typ "String" Type.string
                +> typ "Range" Type.range
                +> typ "List" Type.list_gen
                +> typ "Option" Type.option_gen
                +> typ "Box" Type.box_gen
                +> typ "Stream" Type.stream
                +> fun_ "id" Type.Spec.(a @-> a)
                +> fun_ "print" Type.Spec.(string @-> unit)
                +> fun_ "printf" Type.Spec.fun_printf
                +> string "version"
                |> end_);
  ()