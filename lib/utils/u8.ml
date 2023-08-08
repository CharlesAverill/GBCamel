(** u8.ml - Definition for u8 type *)

open U16

type u8 = char
(** 8-bit integer type *)

let u8_max = 256

(** Converts an integer to an u8 *)
let u8_of_int n = Char.chr (abs n mod 256)

(** Interprets an u8 as a signed integer *)
let signed_int_of_u8 r =
  let value = u16_of_u8 r in
  if value >= 128 then value - 256 else value

(** Modular addition *)
let u8_add a b =
  let a', b' = (int_of_char a, int_of_char b) in
  char_of_int ((a' + b') mod 255)

(** Modular subtraction *)
let u8_sub a b =
  let a', b' = (int_of_char a, int_of_char b) in
  let v = (a' - b') mod 255 in
  if v < 0 then char_of_int (v + 256) else char_of_int v

(** Modular addition on list elements *)
let u8_add_multi l = List.fold_left u8_add '\000' l

(** Modular subtraction on list elements*)
let u8_sub_multi l = List.fold_left u8_sub '\000' l

(** Logical AND *)
let u8_land a' b' =
  let a, b = (u16_of_u8 a', u16_of_u8 b') in
  u8_of_int (a land b)

(** Logical OR *)
let u8_lor a' b' =
  let a, b = (u16_of_u8 a', u16_of_u8 b') in
  u8_of_int (a lor b)

(** Logical XOR *)
let u8_lxor a' b' =
  let a, b = (u16_of_u8 a', u16_of_u8 b') in
  u8_of_int (a lxor b)

let u8_negate a' = u8_of_int (u16_of_u8 a' lxor -1)
