(** gbc.ml - Wrapper definitions for individual hardware emulators *)

open Cpu.Processor
open Memory.Rom
open Memory.Vmem
open Gbcamel.Logging

type gbc = { processor : processor }

let init_gbc rom_path =
  let loaded_rom = load_rom rom_path in
  let _ = _log Log_Info (Printf.sprintf "ROM loaded from '%s'" rom_path) in
  let memory = _init_mem_ctrl loaded_rom in
  let out = { processor = _init_processor memory } in
  out
