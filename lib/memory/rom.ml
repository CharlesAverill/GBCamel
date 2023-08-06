(** rom.ml - Definitions for read-only memory *)

open Gbcamel.Logging
open Mbcs.Mbc
open Mbcs.Mbc0

type rom = bytes * mbc
(** Type defining a ROM *)

let rom_size (x : rom) = Bytes.length (fst x)

let load_rom path =
  if not (Sys.file_exists path) then
    fatal rc_FileError (Printf.sprintf "ROM path \"%s\" does not exist" path)
  else
    let ic = open_in_bin path in
    let file_length = in_channel_length ic in
    let _bytes = Bytes.create file_length in
    really_input ic _bytes 0 file_length;
    close_in ic;
    (_bytes, mbc0 ())

let rom_read rom address =
  let _ = _log Log_Debug (Printf.sprintf "read %d" address) in
  if 0 <= address && address < rom_size rom then Bytes.get (fst rom) address
  else
    fatal rc_MemError
      (Printf.sprintf "Tried to read from position %d of ROM of size %d" address
         (rom_size rom))

let rom_write rom address data =
  let _, _, _ = (rom, address, data) in
  ()
