open Core.Std

let init () =
  Library.Spec.(define "Kernel" ~init:true
                +> typ "Void" Type.void
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
                +> fun_ "print" Type.Spec.(string @-> void)
                +> fun_ "printf" Type.Spec.fun_printf
                +> string "version"
                |> end_);

  let open Type in
  Property.add_prim string
    ~name:"length"
    ~id:("kernel", "StringLength")
    ~value:Spec.(string @-> int);
  ()
