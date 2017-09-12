open Core.Std
open Printf

let file_exists file =
  match Sys.file_exists ~follow_symlinks:true file with
  | `No | `Unknown ->
    Printf.printf "Error: No such file `%s'\n" file;
    exit (-1)
  | `Yes -> ()

let parse_file file =
  file_exists file;
  In_channel.with_file file
    ~f:(fun chan ->
        let buf = Lexing.from_channel chan in
        try begin
          Parser.prog Lexer.read buf
        end with
        | Lexer.Syntax_error (pos, msg) ->
          let open Position in
          printf "Line %d, column %d: Invalid syntax: %s\n" pos.line pos.col msg;
          exit (-1)
        | Parser.Error ->
          let pos = Lexing.lexeme_start_p buf in
          printf "Line %d, column %d: Invalid syntax\n"
            pos.pos_lnum pos.pos_bol;
          exit (-1)
        | e -> raise e)

let init () =
  Lib.init ()

let command =
  Command.basic
    ~summary: (sprintf "Esca %s" Config.version)
    Command.Spec.(
      empty
      +> flag "-d" no_arg ~doc:" debug output"
      +> flag "-v" no_arg ~doc:" print verbose message"
      +> flag "-syntax" no_arg ~doc:" check syntax only"
      +> flag "-debug-ast" no_arg ~doc:" print parse tree"
      +> flag "-runlib" (optional string) ~doc:" runtime library path"
      +> anon (maybe ("filename" %: string))
    )
    (fun debug verbose syntax debug_ast runlib file_opt () ->
       Config.debug_mode := debug;
       Config.verbose_mode := verbose;
       Option.iter runlib ~f:(fun path -> Config.runlib_path := path);

       try
         Printexc.record_backtrace true;
         match file_opt with
         | None ->
           Printf.printf "Error: No input files\n";
           exit 1
         | Some file ->
           init ();
           if syntax then
             ignore @@ parse_file file
           else if debug_ast then
             (* TODO: printing node *)
             let node = parse_file file in
             Ast.write Out_channel.stdout node
           else begin
             let node = parse_file file in
             ignore @@ Typing.run node;

             let out = Hir.Compiler.run file node
                       |> Lir.Compiler.run in
             (* TODO: error handling *)
             if (Sys.command @@ sprintf "goimports -w %s" out) = 0 then begin
               if (Sys.command @@ sprintf "go build -o %s %s"
                     (Filename.chop_extension out)
                     out) = 0 then begin

               end
             end
(*
             begin try Compiler.compile node ~file with
               | Compiler.Error e ->
                 Printf.printf "Error: %s: %s\n" e.err_file e.err_reason
               | _ as e -> raise e
             end
 *)
           end
       with
       | Typing.Type_mismatch e ->
         Printf.printf "Error: Type mismatch: expected: %s, actual: %s\n"
           (Type.to_string e.mismatch_ex)
           (Type.to_string e.mismatch_ac)
       | e -> raise e)

let () =
  Command.run ~version:Config.version command
