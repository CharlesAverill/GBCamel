(** main.ml - Driver code *)

open Argument_parsing
open Gbc

let main =
  let args = parse_arguments () in
  init_gbc args.rom_file

let _ = main
