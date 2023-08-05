(** conditions.ml - Condition codes for instructions *)

open Gbcamel.Logging

let ccNZ = 0b00
let ccZ = 0b01
let ccNC = 0b10
let ccC = 0b11

let cond_decode_error cond =
  fatal rc_DecodeError
    (Printf.sprintf "Tried to decode condition %d but range is [0:4]" cond)
