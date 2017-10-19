open Core.Std
open Printf

let command =
  Command.basic
    ~summary: (sprintf "Esca type declaration file generator %s" Config.version)
    Command.Spec.(
      empty
      +> flag "-d" no_arg ~doc:" debug output"
      +> flag "-v" no_arg ~doc:" print verbose message"
      +> anon ("import" %: string)
      +> anon ("package" %: string)
    )
    (fun debug verbose path package () ->
       Config.debug_mode := debug;
       Config.verbose_mode := verbose;
       printf "Get package '%s' at '%s'\n" path package;
    )

let () =
  Command.run ~version:Config.version command
