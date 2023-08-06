(** vmem.ml - Virtual Memory implementation *)

open Memmap
open Limits
open Gbcamel.Logging
open Gbc_boot_rom
open Ram
open Rom

type mem_ctrl = {
  boot_rom_enabled : bool ref;
  boot_rom : bytes;
  rom : rom;
  vram : ram_bank;
  wram : ram_bank;
  oam : ram_bank;
  hram : ram_bank;
}

let _init_mem_ctrl rom =
  let _ = _log Log_Debug "Memory Controller Initialized" in
  {
    boot_rom_enabled = ref true;
    boot_rom = gbc_boot_rom ();
    rom;
    vram = get_ram_bank vram_size;
    wram = get_ram_bank wram_size;
    oam = get_ram_bank oam_size;
    hram = get_ram_bank hram_size;
  }

let read mem address =
  let _ = _log Log_Debug (Printf.sprintf "read %d" address) in
  if !(mem.boot_rom_enabled) then ram_read mem.boot_rom address
  else
    match address with
    | address when address >= _ROM_START && address <= _ROM_BANK_END ->
        rom_read mem.rom address
    | address when address >= _VRAM_START && address <= _VRAM_END ->
        ram_read mem.vram (address - _VRAM_START)
    | address when address >= _ERAM_START && address <= _ERAM_END ->
        fatal rc_MemError "ERAM read not implemented"
    | address when address >= _WRAM_START && address <= _WRAM_END ->
        ram_read mem.wram (address - _WRAM_START)
    | address when address >= _ECHO_START && address <= _ECHO_END ->
        ram_read mem.wram (address - _ECHO_START)
    | address when address >= _OAM_START && address <= _OAM_END ->
        ram_read mem.oam (address - _OAM_START)
    | address when address >= _HRAM_START && address <= _HRAM_END ->
        ram_read mem.hram (address - _HRAM_START)
    | _ ->
        fatal rc_MemError
          (Printf.sprintf "Invalid virtual memory read at address %x" address)

let write mem address data =
  _log Log_Debug (Printf.sprintf "write %d %c" address data);
  if !(mem.boot_rom_enabled) && address = 0xFF50 then (
    mem.boot_rom_enabled := false;
    _log Log_Debug "Boot sequence succeeded";
    exit 0)
  else
    match address with
    | address when address >= _ROM_START && address <= _ROM_BANK_END ->
        rom_write mem.rom address data
    | address when address >= _VRAM_START && address <= _VRAM_END ->
        ram_write mem.vram (address - _VRAM_START) data
    | address when address >= _ERAM_START && address <= _ERAM_END ->
        rom_write mem.rom (address - _ERAM_START) data
    | address when address >= _WRAM_START && address <= _WRAM_END ->
        ram_write mem.wram (address - _WRAM_START) data
    | address when address >= _ECHO_START && address <= _ECHO_END ->
        fatal rc_MemError
          (Printf.sprintf "Illegal ECHO memory write attempt at address %x"
             address)
    | address when address >= _OAM_START && address <= _OAM_END ->
        ram_write mem.oam (address - _OAM_START) data
    | address when address >= _HRAM_START && address <= _HRAM_END ->
        ram_write mem.hram (address - _HRAM_START) data
    | _ ->
        fatal rc_MemError
          (Printf.sprintf
             "Invalid virtual memory write at address %x with data %x" address
             (int_of_char data))
