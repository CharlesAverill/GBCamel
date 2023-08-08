(** mbc.ml - Definitions for the memory-bank controller type *)

(** {{: https://gbdev.gg8.se/wiki/articles/Memory_Bank_Controllers } https://gbdev.gg8.se/wiki/articles/Memory_Bank_Controllers} *)

open Utils.U16
open Utils.U8

(** Constructors for each MBC to implement *)
type mbc =
  | Mbc0 of mbc_funcs
  | Mbc1 of {
      title : string;
      rom_bank : u8;
      ram_bank : u8;
      ram_enabled : bool;
      ram_selected : bool;
      eram : bytes;
    }
  | Mbc2 of { rom_bank : u8 }
  | Mbc3 of {
      title : string;
      rom_bank : u8;
      ram_bank : u8;
      rtc_register : u8;
      ram_timer_enabled : bool;
      ram_selected : bool;
      eram : bytes;
    }

and mbc_funcs = {
  read : mbc -> bytes -> u16 -> u8;
  write : mbc -> u16 -> u8 -> unit;
  set_title : mbc -> string -> unit;
  load : mbc -> unit;
}
(** Common functions shared by all MBCs *)
