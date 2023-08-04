open Memory.Ram
open Memory.Rom
open Cpu.Registers
(* open Registers *)

let _ramtest () =
  let ram = get_ram_bank 8 in
  ram_write ram 0 'h';
  ram_write ram 1 'i';
  ram_write ram 2 '!';
  print_endline (string_of_bank (get_sub_bank ram 0 1))

let _romloadtest () = load_rom "roms/pokecrystal.gbc"

let _regprinttest () =
  let regs = _init_registers () in
  (* set_af regs 0x1234; *)
  print_regs regs

let _ = _regprinttest ()
