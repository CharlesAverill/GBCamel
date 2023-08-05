(** u16.ml - Definition for u16 type *)

type u16 = int

let u16_max = 65535
let u16_add a b = (a + b) mod u16_max

let u16_sub a b =
  let v = (a - b) mod u16_max in
  if v < 0 then v + u16_max + 1 else v

let u16_mul a b = a * b mod u16_max
let u16_div a b = a / b mod u16_max
let u16_lshift n b = Int.shift_left n b mod u16_max
let u16_rshift n b = Int.shift_right_logical n b mod u16_max

let combine a b =
  let high, low = (int_of_char a, int_of_char b) in
  (high lsl 8) lor low
