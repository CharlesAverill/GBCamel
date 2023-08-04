(** registers.ml - Definitions for CPU registers *)

open Utils.U16

type r8 = char
type r16 = u16

let r8_of_int : int -> r8 = Char.chr

type registers = {
  a : r8 ref;
  b : r8 ref;
  c : r8 ref;
  d : r8 ref;
  e : r8 ref;
  f : r8 ref;
  h : r8 ref;
  l : r8 ref;
  sp : r16 ref;
  pc : r16 ref;
}

let a regs = !(regs.a)
let b regs = !(regs.b)
let c regs = !(regs.c)
let d regs = !(regs.d)
let e regs = !(regs.e)
let f regs = !(regs.f)
let h regs = !(regs.h)
let l regs = !(regs.l)
let sp regs = !(regs.sp)
let pc regs = !(regs.pc)
let combine x y = (Char.code x lsl 8) lor Char.code y
let af regs = combine (a regs) (f regs)
let bc regs = combine (b regs) (c regs)
let de regs = combine (d regs) (e regs)
let hl regs = combine (h regs) (l regs)
let high x : r8 = char_of_int ((x lsr 8) land 0xFF)
let low x : r8 = char_of_int (x land 0xFF)
let set_a regs a = regs.a := a
let set_b regs b = regs.b := b
let set_c regs c = regs.c := c
let set_d regs d = regs.d := d
let set_e regs e = regs.e := e
let set_f regs f = regs.f := f
let set_h regs h = regs.h := h
let set_l regs l = regs.l := l
let set_sp regs sp = regs.sp := sp
let set_pc regs pc = regs.pc := pc

let set_af regs af =
  regs.a := high af;
  regs.f := low af

let set_bc regs bc =
  regs.b := high bc;
  regs.c := low bc

let set_de regs de =
  regs.d := high de;
  regs.e := low de

let set_hl regs hl =
  regs.h := high hl;
  regs.l := low hl

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
    (int_of_char (a regs))
    (int_of_char (f regs))
    (bc regs)
    (int_of_char (b regs))
    (int_of_char (c regs))
    (de regs)
    (int_of_char (d regs))
    (int_of_char (e regs))
    (hl regs)
    (int_of_char (h regs))
    (int_of_char (l regs))
    (sp regs) (pc regs)

let print_regs regs = print_endline (string_of_regs regs)

let _init_registers () =
  {
    a = ref (r8_of_int 0x01);
    b = ref (r8_of_int 0x00);
    c = ref (r8_of_int 0x13);
    d = ref (r8_of_int 0x00);
    e = ref (r8_of_int 0xD8);
    f = ref (r8_of_int 0xB0);
    h = ref (r8_of_int 0x01);
    l = ref (r8_of_int 0x4D);
    sp = ref 0xFFFE;
    pc = ref 0x0150;
  }
