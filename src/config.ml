let version = "0.1"

let debug_mode = ref false

let verbose_mode = ref false

let runlib_path = ref "github.com/szktty/esca/runtime"

let runtime_package name =
  Printf.sprintf "%s/%s" !runlib_path name
