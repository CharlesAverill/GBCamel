(** rom.ml - Definitions for read-only memory *)

open Gbcamel.Logging

type rom = bytes
(** Type defining a ROM *)

let rom_size = Bytes.length

let load_rom path =
  if not (Sys.file_exists path) then
    fatal rc_FileError (Printf.sprintf "ROM path \"%s\" does not exist" path)
  else
    let ic = open_in_bin path in
    let file_length = in_channel_length ic in
    let bytes = Bytes.create file_length in
    really_input ic bytes 0 file_length;
    close_in ic;
    bytes

let rom_read rom address =
  let _ = _log Log_Debug (Printf.sprintf "read %d" address) in
  if 0 <= address && address < rom_size rom then Bytes.get rom address
  else
    fatal rc_MemError
      (Printf.sprintf "Tried to read from position %d of ROM of size %d" address
         (rom_size rom))
