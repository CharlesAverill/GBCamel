(** registers.ml - Definitions for CPU registers and byte arithmetic *)

open Utils.U16
open Utils.U8
open Gbcamel.Logging

type registers = {
  af : u16 ref;
  bc : u16 ref;
  de : u16 ref;
  hl : u16 ref;
  sp : u16 ref;
  pc : u16 ref;
}
(** Struct for CPU registers - 8-bit registers are an abstraction *)

(** Get the highest 8 bits of an u16 *)
let high x : u8 = char_of_int ((x lsr 8) land 0xFF)

(** Get the lowest 8 bits of an u16*)
let low x : u8 = char_of_int (x land 0xFF)

(* Getter functions to minimize interactions with refs *)
let a regs = high !(regs.af)
let f regs = low !(regs.af)
let b regs = high !(regs.bc)
let c regs = low !(regs.bc)
let d regs = high !(regs.de)
let e regs = low !(regs.de)
let h regs = high !(regs.hl)
let l regs = low !(regs.hl)
let af regs = !(regs.af)
let bc regs = !(regs.bc)
let de regs = !(regs.de)
let hl regs = !(regs.hl)
let sp regs = !(regs.sp)
let pc regs = !(regs.pc)

(* Setter functions to minimize interactions with refs*)
let set_high r v' =
  let v = match v' with `R8 c -> u16_of_u8 c | `R16 i -> i in
  r := !r land 0xFF lor ((v lsl 8) land 0xFF00)

let set_low r (v' : [> `R16 of int | `R8 of char ]) =
  let v = match v' with `R8 c -> u16_of_u8 c | `R16 i -> i in
  r := !r land 0xFF00 lor (v land 0xFF)

let set_a regs a = set_high regs.af a
let set_f regs f = set_low regs.af f
let set_b regs b = set_high regs.bc b
let set_c regs c = set_low regs.bc c
let set_d regs d = set_high regs.de d
let set_e regs e = set_low regs.de e
let set_h regs h = set_high regs.hl h
let set_l regs l = set_low regs.hl l

let set_af regs af =
  _log Log_Debug (Printf.sprintf "Set AF = %x" af);
  regs.af := af mod u16_max

let set_bc regs bc =
  _log Log_Debug (Printf.sprintf "Set BC = %x" bc);
  regs.bc := bc mod u16_max

let set_de regs de =
  _log Log_Debug (Printf.sprintf "Set DE = %x" de);
  regs.de := de mod u16_max

let set_hl regs hl =
  _log Log_Debug (Printf.sprintf "Set HL = %x" hl);
  regs.hl := hl mod u16_max

let hli regs = set_hl regs (u16_add !(regs.hl) 1)
let hld regs = set_hl regs (u16_sub !(regs.hl) 1)

let set_sp regs sp =
  _log Log_Debug (Printf.sprintf "Set SP = %x" sp);
  regs.sp := sp mod u16_max

let set_pc regs pc =
  _log Log_Debug (Printf.sprintf "Set PC = %x" pc);
  regs.pc := pc mod u16_max

(** Formats a register set as a string *)
let string_of_regs regs =
  Printf.sprintf
    "AF: 0x%04x\n\
     \tA: 0x%02x\n\
     \tF: 0x%02x\n\
     BC: 0x%04x\n\
     \tB: 0x%02x\n\
     \tC: 0x%02x\n\
     DE: 0x%04x\n\
     \tD: 0x%02x\n\
     \tE: 0x%02x\n\
     HL: 0x%04x\n\
     \tH: 0x%02x\n\
     \tL: 0x%02x\n\
     SP: 0x%04x\n\
     PC: 0x%04x" (af regs)
    (u16_of_u8 (a regs))
    (u16_of_u8 (f regs))
    (bc regs)
    (u16_of_u8 (b regs))
    (u16_of_u8 (c regs))
    (de regs)
    (u16_of_u8 (d regs))
    (u16_of_u8 (e regs))
    (hl regs)
    (u16_of_u8 (h regs))
    (u16_of_u8 (l regs))
    (sp regs) (pc regs)

(** Prints a string representation of a set of registers *)
let print_regs regs = print_endline (string_of_regs regs)

(** Default-initialized register contents after GB BIOS runs *)
let _init_registers () =
  let out =
    { af = ref 0; bc = ref 0; de = ref 0; hl = ref 0; sp = ref 0; pc = ref 0 }
  in
  out
