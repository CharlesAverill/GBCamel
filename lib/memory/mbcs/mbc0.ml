(** mbc0.ml - Definitions for MBC0, not really an MBC *)

(** {{: https://gbdev.gg8.se/wiki/articles/Memory_Bank_Controllers#None_.2832KByte_ROM_only.29} https://gbdev.gg8.se/wiki/articles/Memory_Bank_Controllers#None_.2832KByte_ROM_only.29 } *)

open Mbc
open Utils.U16
open Utils.U8

let read_mbc0 (_ : mbc) memory address = Bytes.get memory address
let write_mbc0 (_ : mbc) (_ : u16) (_ : u8) = ()
let set_title_mbc0 (_ : mbc) (_ : string) = ()
let load_mbc0 (_ : mbc) = ()

let mbc0 () =
  Mbc0
    {
      read = read_mbc0;
      write = write_mbc0;
      set_title = set_title_mbc0;
      load = load_mbc0;
    }
