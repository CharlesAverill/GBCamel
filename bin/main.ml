(** main.ml - Driver code *)

open Argument_parsing
open Gbc

let main () =
  let args = parse_arguments () in
  let gbc = init_gbc args.rom_file args.model in
  if args.print_bios then (
    print_bios gbc;
    exit 0)
  else run gbc args.step

let _ = main ()
