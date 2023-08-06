(** ram.ml - Definitions for random-access memory *)

open Gbcamel.Logging
open Limits

type ram_bank = bytes
(** Type defining a RAM bank *)

let ram_size = Bytes.length
let address_in_bounds bank address = 0 <= address && address < ram_size bank

(** Initializes a RAM bank of size n *)
let get_ram_bank n : ram_bank =
  if n <= 0 || n > address_max + 1 then
    fatal rc_MemError
      (Printf.sprintf
         "Tried to create RAM bank of invalid size %d, range is [1:%d]" n
         (address_max + 1))
  else Bytes.make n '\000'

(** Gets a subsection of a RAM bank from [start:stop] *)
let get_sub_bank ram start stop =
  if address_in_bounds ram start && address_in_bounds ram stop then
    Bytes.sub ram start (stop + 1)
  else
    fatal rc_MemError
      (Printf.sprintf
         "Tried to get a subbank of range [%d:%d] but the provided bank has \
          size %d"
         start stop (ram_size ram))

(** Gets a string representation of a RAM bank *)
let string_of_bank = Bytes.to_string

(** Reads ram[address] *)
let ram_read ram address =
  if address_in_bounds ram address then Bytes.get ram address
  else
    fatal rc_MemError
      (Printf.sprintf "Tried to read from position %d of bank of size %d"
         address (ram_size ram))

(** Performs the assignment ram[address] := data *)
let ram_write ram address data =
  if address_in_bounds ram address then Bytes.set ram address data
  else
    fatal rc_MemError
      (Printf.sprintf "Tried to write to position %d of bank of size %d" address
         (ram_size ram))
