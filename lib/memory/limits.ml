(** limits.ml - Information about space limits, such as bank sizes *)

(** Address space bit width *)
let address_width = 16

(** Address space boundary *)
let address_max = int_of_float (2.0 ** float_of_int address_width) - 1

(** 352 bytes of High Memory *)
let hram_size = 352

(** 160B of sprite table *)
let oam_size = 160

(** 16KB - 8 for GB and all 16 for CGB *)
let vram_size = 16384

(** 4KB *)
let wram_bank_size = 4096

(** 32 KB of Working RAM - 2 GB banks and 6 GBC banks *)
let wram_size = 4096 * 8
