(** mbc.ml - Definitions for the memory-bank controller type *)

(** {{: https://gbdev.gg8.se/wiki/articles/Memory_Bank_Controllers } https://gbdev.gg8.se/wiki/articles/Memory_Bank_Controllers} *)

open Utils.U16

type byte = char

(** Constructors for each MBC to implement *)
type mbc =
  | Mbc0 of mbc_funcs
  | Mbc1 of {
      title : string;
      rom_bank : byte;
      ram_bank : byte;
      ram_enabled : bool;
      ram_selected : bool;
      eram : bytes;
    }
  | Mbc2 of { rom_bank : byte }
  | Mbc3 of {
      title : string;
      rom_bank : byte;
      ram_bank : byte;
      rtc_register : byte;
      ram_timer_enabled : bool;
      ram_selected : bool;
      eram : bytes;
    }

and mbc_funcs = {
  read : mbc -> bytes -> u16 -> byte;
  write : mbc -> u16 -> byte -> unit;
  set_title : mbc -> string -> unit;
  load : mbc -> unit;
}
(** Common functions shared by all MBCs *)
