(** vmem.ml - Virtual Memory implementation *)

open Memmap
open Limits
open Gbcamel.Logging
open Ram
open Rom

type mem_ctrl = { rom : rom; wram : ram_bank; hram : ram_bank }

let new_mem_ctrl rom =
  { rom; wram = get_ram_bank wram_bank_size; hram = get_ram_bank hram_size }

let read_mem mem address =
  match address with
  | address when address >= _ROM_START && address <= _ROM_BANK_END ->
      rom_read mem.rom address
  (* | address when address >= _VRAM_START && address <= _VRAM_END ->
      gpu_read mem.gpu address *)
  | address when address >= _ERAM_START && address <= _ERAM_END ->
      ram_read mem.rom address
  | address when address >= _WRAM_START && address <= _WRAM_END ->
      ram_read mem.wram address
  | address when address >= _ECHO_START && address <= _ECHO_END ->
      ram_read mem.wram address
  (* | address when address >= _OAM_START && address <= _OAM_END ->
      gpu_read mem.gpu address *)
  | address when address >= _HRAM_START && address <= _HRAM_END ->
      ram_read mem.hram address
  | _ ->
      fatal rc_MemError
        (Printf.sprintf "Invalid virtual memory read at address %d" address)
