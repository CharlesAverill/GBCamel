(** argument_parsing.ml - Argument parsing logic *)

open Gbcamel.Globals
open Gbcamel.Logging

type arguments = {
  rom_file : string;
  print_bios : bool;
  model : string;
  step : bool;
}

let parse_arguments () =
  let rom_file = ref ""
  and log_level = _GLOBAL_LOG_LEVEL
  and print_bios = ref false
  and model = ref "DMG"
  and step = ref false in

  let speclist =
    [
      ( "-log-level",
        Arg.Int (fun n -> log_level := n),
        "Minimum severity of logs to print \
         [1=Debug|2=Info|3=Warning|4=Error|5=Critical]" );
      ( "-print-bios",
        Arg.Set print_bios,
        "Prints out the contents of the loaded BIOS in hex" );
      ( "-model",
        Arg.String (fun n -> model := n),
        "Forces a model of Game Boy to emulate [DMG|MGB|CGB|SameBoyCGB]" );
      ( "-step",
        Arg.Set step,
        "Waits for keyboard input after each instruction is executed" );
    ]
  in

  let usage_msg = "Usage: gbcamel [OPTION...] ROM" in

  Arg.parse speclist (fun s -> rom_file := s) usage_msg;

  let _ =
    if !rom_file = "" then
      fatal rc_ArgError
        (Printf.sprintf "Please provide a ROM file\n%s" usage_msg)
    else if !log_level < 1 || !log_level > 5 then
      fatal rc_ArgError
        (Printf.sprintf "-log-level must be in [1:5]\n%s" usage_msg)
  in

  {
    rom_file = !rom_file;
    print_bios = !print_bios;
    model = !model;
    step = !step;
  }
