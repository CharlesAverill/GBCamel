(** main.ml - Driver code *)

open Argument_parsing
open Gbc

let main () =
  let args = parse_arguments () in
  let gbc = init_gbc args.rom_file args.model in
  run gbc args.step

let _ = main ()
