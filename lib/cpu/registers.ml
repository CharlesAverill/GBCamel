(** registers.ml - Definitions for CPU registers and byte arithmetic *)

open Utils.U16

type r8 = char
(** 8-bit register type *)

type r16 = u16
(** 16-bit register type *)

(** Converts an integer to an r8 *)
let r8_of_int n = Char.chr (abs n mod 256)

(** Converts an r8 into an r16 *)
let r16_of_r8 x = int_of_char x mod u16_max

(** Interprets an r8 as a signed integer *)
let signed_int_of_r8 r =
  let value = r16_of_r8 r in
  if value >= 128 then value - 256 else value

(** Modular addition *)
let r8_add a b =
  let a', b' = (int_of_char a, int_of_char b) in
  char_of_int ((a' + b') mod 255)

(** Modular subtraction *)
let r8_sub a b =
  let a', b' = (int_of_char a, int_of_char b) in
  let v = (a' - b') mod 255 in
  if v < 0 then char_of_int (v + 256) else char_of_int v

(** Modular addition on list elements *)
let r8_add_multi l = List.fold_left r8_add '\000' l

(** Modular subtraction on list elements*)
let r8_sub_multi l = List.fold_left r8_sub '\000' l

(** Logical AND *)
let r8_land a' b' =
  let a, b = (r16_of_r8 a', r16_of_r8 b') in
  r8_of_int (a land b)

(** Logical OR *)
let r8_lor a' b' =
  let a, b = (r16_of_r8 a', r16_of_r8 b') in
  r8_of_int (a lor b)

(** Logical XOR *)
let r8_lxor a' b' =
  let a, b = (r16_of_r8 a', r16_of_r8 b') in
  r8_of_int (a lxor b)

type registers = {
  af : r16 ref;
  bc : r16 ref;
  de : r16 ref;
  hl : r16 ref;
  sp : r16 ref;
  pc : r16 ref;
}
(** Struct for CPU registers - 8-bit registers are an abstraction *)

(** Get the highest 8 bits of an r16 *)
let high x : r8 = char_of_int ((x lsr 8) land 0xFF)

(** Get the lowest 8 bits of an r16*)
let low x : r8 = char_of_int (x land 0xFF)

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
  let v = match v' with `R8 c -> r16_of_r8 c | `R16 i -> i in
  r := u16_lshift (v land 0xFF) 8 lor (!r land 0xFF) land u16_max

let set_low r (v' : [> `R16 of int | `R8 of char ]) =
  let v = match v' with `R8 c -> r16_of_r8 c | `R16 i -> i in
  r := !r land 0xFF00 lor (v land 0xFF) land u16_max

let set_a regs a = set_high regs.af a
let set_f regs f = set_low regs.af f
let set_b regs b = set_high regs.bc b
let set_c regs c = set_low regs.bc c
let set_d regs d = set_high regs.de d
let set_e regs e = set_low regs.de e
let set_h regs h = set_high regs.hl h
let set_l regs l = set_low regs.hl l
let set_af regs af = regs.af := af mod u16_max
let set_bc regs bc = regs.bc := bc mod u16_max
let set_de regs de = regs.de := de mod u16_max
let set_hl regs hl = regs.hl := hl mod u16_max
let hli regs = set_hl regs (u16_add !(regs.hl) 1)
let hld regs = set_hl regs (u16_sub !(regs.hl) 1)
let set_sp regs sp = regs.sp := sp mod u16_max
let set_pc regs pc = regs.pc := pc mod u16_max

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
    (r16_of_r8 (a regs))
    (r16_of_r8 (f regs))
    (bc regs)
    (r16_of_r8 (b regs))
    (r16_of_r8 (c regs))
    (de regs)
    (r16_of_r8 (d regs))
    (r16_of_r8 (e regs))
    (hl regs)
    (r16_of_r8 (h regs))
    (r16_of_r8 (l regs))
    (sp regs) (pc regs)

(** Prints a string representation of a set of registers *)
let print_regs regs = print_endline (string_of_regs regs)

(** Default-initialized register contents after GB BIOS runs *)
let _init_registers () =
  let out =
    { af = ref 0; bc = ref 0; de = ref 0; hl = ref 0; sp = ref 0; pc = ref 0 }
  in
  let _set_defaults =
    set_a out (`R16 0x01);
    set_f out (`R16 0xB0);
    set_b out (`R16 0x00);
    set_c out (`R16 0x13);
    set_d out (`R16 0x00);
    set_e out (`R16 0xD8);
    set_h out (`R16 0x01);
    set_l out (`R16 0x4D);
    set_sp out 0xFFFE;
    set_pc out 0x0150
  in
  out
