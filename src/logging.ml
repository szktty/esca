open Core.Std

let debug f =
  if !Config.debug_mode then
    printf ("# " ^^ f ^^ "\n")
  else
    Printf.ifprintf stderr f

let verbose f =
  if !Config.verbose_mode || !Config.debug_mode then
    printf ("# " ^^ f ^^ "\n")
  else
    Printf.ifprintf stderr f
