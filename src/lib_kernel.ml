open Core.Std

let init () =
  Library.Spec.(define "Kernel" ~init:true
                  ~package:(Config.runtime_package "lib/kernel")
                +> typ "Void" Type.void
                +> typ "Bool" Type.bool
                +> typ "Int" Type.int
                +> typ "Float" Type.float
                +> typ "String" Type.string
                +> typ "Range" Type.range
                +> typ "List" Type.list_gen
                +> typ "Option" Type.option_gen
                +> typ "Ref" Type.ref_gen
                +> typ "Stream" Type.stream
                +> fun_ "id" Type.Spec.(a @-> a)
                +> fun_ "show" Type.Spec.(a @-> void)
                +> fun_ "print" Type.Spec.(string @-> void)
                +> fun_ "printf" Type.Spec.printf
                +> string "version"
                |> end_);

  Property.add_method Type.string
    ~name:"length"
    ~spec:Type.Spec.(string @-> int);
  ()
