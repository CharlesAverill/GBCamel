(** main.ml - Driver code *)

open Argument_parsing
open Gbc

let main () =
  let args = parse_arguments () in
  let gbc = init_gbc args.rom_file in
  run gbc

let _ = main ()
