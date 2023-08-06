(** gbc.ml - Wrapper definitions for individual hardware emulators *)

open Cpu.Processor
open Gbcamel.Logging
open Memory.Limits
open Memory.Rom
open Memory.Vmem

type gbc = { processor : processor }

let init_gbc rom_path =
  let loaded_rom = load_rom rom_path in
  let _ = _log Log_Info (Printf.sprintf "ROM loaded from '%s'" rom_path) in
  let memory = _init_mem_ctrl loaded_rom in
  let out = { processor = _init_processor memory } in
  out

let step gbc =
  let cycles = de_step gbc.processor * 4 in
  cycles

let run gbc =
  let break_loop = ref false in
  while not !break_loop do
    let start_time = Core_unix.gettimeofday () in
    (* 16.6 ms *)
    let frame_time = 0.0166 in
    let cycles_per_frame = _CLOCK_SPEED / _FRAME_RATE in
    let emulated_cycles = ref 0 in
    while !emulated_cycles < cycles_per_frame do
      emulated_cycles := !emulated_cycles + step gbc
    done;
    let elapsed_time = Core_unix.gettimeofday () -. start_time in
    if not (elapsed_time > frame_time) then
      Core_thread.delay (frame_time -. elapsed_time)
  done
