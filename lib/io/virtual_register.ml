(** virtual_register.ml - Definitions for "virtual" registers in RAM that deal with I/O *)

type virtual_register = int ref

let vr_max = 256
let _get vr = !vr mod vr_max
let get vr = char_of_int (!vr mod vr_max)
let set vr d = vr := d mod vr_max
let clear vr = set vr 0
let set_nth_bit vr n = set vr (_get vr lor (1 lsl n))
let clear_nth_bit vr n = set vr (_get vr land ((1 lsl n) lxor -1))
let is_nth_bit_set vr n = _get vr land (1 lsl n) <> 0
let add vr n = set vr ((_get vr + n) mod vr_max)
