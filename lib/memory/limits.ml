(** limits.ml - Information about space limits, such as bank sizes *)

(** Address space bit width *)
let address_width = 16

(** Address space boundary *)
let address_max = int_of_float (2.0 ** float_of_int address_width) - 1

(** 4KB *)
let wram_bank_size = 4096

(** 32 KB of Working RAM - 2 GB banks and 6 GBC banks *)
let wram_size = 4096 * 8

(* 352 bytes of High Memory *)
let hram_size = 352
