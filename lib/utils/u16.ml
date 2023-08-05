(** u16.ml - Definition for u16 type *)

type u16 = int

let u16_max = 65535
let u16_add a b = (a + b) land u16_max
let u16_sub a b = (a - b) land u16_max
let u16_mul a b = a * b land u16_max
let u16_div a b = a / b land u16_max
let u16_lshift n b = Int.shift_left n b land u16_max
let u16_rshift n b = Int.shift_right_logical n b land u16_max
