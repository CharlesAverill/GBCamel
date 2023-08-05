(** processor.ml - CPU logic *)

open Flags
open Conditions
open Registers
open Memory.Vmem
open Utils.U16
open Gbcamel.Logging

type processor = { regs : registers }

let _init_processor = { regs = _init_registers () }

let read_byte cpu mem =
  let instr = read mem (pc cpu.regs) in
  let _ = set_pc cpu.regs (u16_add (pc cpu.regs) 1) in
  instr

let read_word cpu mem =
  let low = read_byte cpu mem in
  let high = read_byte cpu mem in
  combine high low

let is_flag_set cpu flag =
  if r16_of_r8 (f cpu.regs) land flag = 1 then true else false

let is_flag_set_b cpu flag = if is_flag_set cpu flag then 1 else 0

let set_flag cpu flag value =
  if value then set_f cpu.regs (`R16 (r16_of_r8 (f cpu.regs) lor flag))
  else set_f cpu.regs (`R16 (r16_of_r8 (f cpu.regs) land (flag lxor -1)))

let rp_table_write rp =
  match rp with
  | 0 -> set_bc
  | 1 -> set_de
  | 2 -> set_hl
  | 3 -> set_sp
  | _ ->
      fatal rc_DecodeError
        (Printf.sprintf "Tried to decode rp[%d] for writing, range is [0:4]" rp)

let rp_table_read rp =
  match rp with
  | 0 -> bc
  | 1 -> de
  | 2 -> hl
  | 3 -> sp
  | _ ->
      fatal rc_DecodeError
        (Printf.sprintf "Tried to decode rp[%d] for reading, range is [0:4]" rp)

let r_table_read mem r =
  match r with
  | 0 -> b
  | 1 -> c
  | 2 -> d
  | 3 -> e
  | 4 -> h
  | 5 -> l
  | 6 -> fun x -> read mem (hl x)
  | 7 -> a
  | _ ->
      fatal rc_DecodeError
        (Printf.sprintf "Tried to decode r[%d] for reading, range is [0:7]" r)

let r_table_write regs mem r data =
  match r with
  | 0 -> set_b regs (`R8 data)
  | 1 -> set_c regs (`R8 data)
  | 2 -> set_d regs (`R8 data)
  | 3 -> set_e regs (`R8 data)
  | 4 -> set_h regs (`R8 data)
  | 5 -> set_l regs (`R8 data)
  | 6 -> write mem (hl regs) data
  | 7 -> set_a regs (`R8 data)
  | _ ->
      fatal rc_DecodeError
        (Printf.sprintf "Tried to decode r[%d] for reading, range is [0:7]" r)

let nop () = 1

(* TODO : Figure out how to implement STOP *)
let stop _cpu _mem = 1
let j cpu address = cpu.regs.pc := address
let jr cpu offset = j cpu (u16_add (pc cpu.regs) offset)

let j_cond cpu cond address =
  let cond_met = ref false in
  let _ =
    match cond land 0b11 with
    | cond when cond = cNZ ->
        if not (is_flag_set cpu fZER) then j cpu address;
        cond_met := true
    | cond when cond = cZR ->
        if is_flag_set cpu fZER then j cpu address;
        cond_met := true
    | cond when cond = cNC ->
        if not (is_flag_set cpu fCAR) then j cpu address;
        cond_met := true
    | cond when cond = cCR ->
        if is_flag_set cpu fCAR then j cpu address;
        cond_met := true
    | _ -> ()
  in
  if !cond_met then 4 else 3

let jr_cond cpu cond offset = j_cond cpu cond (u16_add (pc cpu.regs) offset)
let add dest src = dest := u16_add !dest src

let rotate_left cpu n' include_carry update_zero =
  let n = r16_of_r8 n' in
  let msb = n lsr 7 in
  let result =
    if include_carry then (n lsl 1) lor is_flag_set_b cpu fCAR
    else (n lsl 1) lor (n lsr 7) land 0xFF
  in
  set_flag cpu fCAR (msb = 1);
  set_flag cpu fHCR false;
  set_flag cpu fSUB false;
  set_flag cpu fZER (result = 0 && update_zero);
  r8_of_int result

let rotate_right cpu n' include_carry update_zero =
  let n = r16_of_r8 n' in
  let lsb = n land 1 in
  let result =
    if include_carry then (n lsr 1) lor (is_flag_set_b cpu fCAR lsl 7)
    else (n lsl 1) lor (n lsr 7) land 0xFF
  in
  set_flag cpu fCAR (lsb = 1);
  set_flag cpu fHCR false;
  set_flag cpu fSUB false;
  set_flag cpu fZER (result = 0 && update_zero);
  r8_of_int result

let decode_execute cpu mem op' =
  let op = r16_of_r8 op' in
  let regs = cpu.regs in

  let x = (0b11000000 land op) lsr 6
  and y = (0b00111000 land op) lsr 3
  and z = 0b00000111 land op in
  let p = (0b110 land y) lsr 1 and q = 0b1 land y in

  let _ = (x, y, z, p, q, cpu, mem) in

  let decode_error_x () =
    fatal rc_DecodeError (Printf.sprintf "Decoded x as %d, range is [0:4]" x)
  in
  let decode_error_y () =
    fatal rc_DecodeError (Printf.sprintf "Decoded y as %d, range is [0:7]" y)
  in
  let decode_error_z () =
    fatal rc_DecodeError (Printf.sprintf "Decoded z as %d, range is [0:7]" z)
  in
  let decode_error_p () =
    fatal rc_DecodeError (Printf.sprintf "Decoded p as %d, range is [0:4]" p)
  in
  let decode_error_q () =
    fatal rc_DecodeError (Printf.sprintf "Decoded q as %d, range is [0:1]" q)
  in

  match x with
  | 0 -> (
      match z with
      | 0 -> (
          (* Relative jumps and assorted ops *)
          match y with
          (* NOP *)
          | 0 -> nop ()
          (* 	LD (nn), SP *)
          | 1 ->
              let nn = read_word cpu mem in
              let src = sp regs in
              let _ =
                write mem nn (low src);
                write mem (nn + 1) (high src)
              in
              5
          (* STOP *)
          | 2 -> stop cpu mem
          (* JR d *)
          | 3 ->
              jr cpu (signed_int_of_r8 (read_byte cpu mem));
              3
          (* JR cc[y-4], d *)
          | y when y >= 4 && y <= 7 ->
              jr_cond cpu (y - 4) (signed_int_of_r8 (read_byte cpu mem))
          | _ -> decode_error_y ())
      | 1 -> (
          (* 16-bit load immediate/add *)
          match q with
          (* LD rp[p], nn *)
          | 0 ->
              (rp_table_write p) regs (read_word cpu mem);
              3
          (* ADD HL, rp[p] *)
          | 1 ->
              add regs.hl ((rp_table_read p) regs);
              2
          | _ -> decode_error_q ())
      | 2 -> (
          (* Indirect loading *)
          match q with
          | 0 -> (
              match p with
              (* LD (BC), A *)
              | 0 ->
                  write mem (bc regs) (a regs);
                  2
              (* LD (DE), A *)
              | 1 ->
                  write mem (de regs) (a regs);
                  2
              (* LD (HL+), A *)
              | 2 ->
                  write mem (hl regs) (a regs);
                  hli regs;
                  2
              (* LD (HL-), A *)
              | 3 ->
                  write mem (hl regs) (a regs);
                  hld regs;
                  2
              | _ -> decode_error_p ())
          | 1 -> (
              match p with
              (* LD A, (BC) *)
              | 0 ->
                  set_a regs (`R8 (read mem (bc regs)));
                  2
              (* LD A, (DE) *)
              | 1 ->
                  set_a regs (`R8 (read mem (de regs)));
                  2
              (* LD A, (HL+) *)
              | 2 ->
                  set_a regs (`R8 (read mem (hl regs)));
                  hli regs;
                  2
              (* LD A, (HL-) *)
              | 3 ->
                  set_a regs (`R8 (read mem (hl regs)));
                  hld regs;
                  2
              | _ -> decode_error_p ())
          | _ -> decode_error_q ())
      | 3 -> (
          (* 16-bit INC/DEC *)
          match q with
          (* INC rp[p] *)
          | 0 ->
              (rp_table_write p) regs (u16_add 1 ((rp_table_read p) regs));
              1
          (* DEC rp[p] *)
          | 1 ->
              (rp_table_write p) regs (u16_sub 1 ((rp_table_read p) regs));
              1
          | _ -> decode_error_q ())
      | 4 ->
          (* 8-bit INC *)
          r_table_write regs mem y (r8_add ((r_table_read mem y) regs) '\001');
          1
      | 5 ->
          (* 8-bit DEC *)
          r_table_write regs mem y (r8_sub ((r_table_read mem y) regs) '\001');
          1
      | 6 ->
          (* LD r[y], n *)
          r_table_write regs mem y (read_byte cpu mem);
          2
      | 7 -> (
          (* Assorted operations on accumulator/flags *)
          match y with
          | 0 ->
              (* RLCA *)
              set_a regs (`R8 (rotate_left cpu (a regs) false false));
              1
          | 1 ->
              (* RRCA *)
              set_a regs (`R8 (rotate_right cpu (a regs) false false));
              1
          | 2 ->
              (* RLA  *)
              set_a regs (`R8 (rotate_left cpu (a regs) true false));
              1
          | 3 ->
              (* RRA  *)
              set_a regs (`R8 (rotate_right cpu (a regs) true false));
              1
          | 4 -> (* DAA  *) fatal rc_DecodeError "DAA not yet implemented"
          | 5 ->
              (* CPL  *)
              set_a regs (`R16 (r16_of_r8 (a regs) lxor -1));
              set_flag cpu fHCR true;
              set_flag cpu fSUB true;
              1
          | 6 ->
              (* SCF  *)
              set_flag cpu fSUB false;
              set_flag cpu fHCR false;
              set_flag cpu fCAR true;
              1
          | 7 ->
              (* CCF  *)
              set_flag cpu fSUB false;
              set_flag cpu fHCR false;
              set_flag cpu fCAR (is_flag_set_b cpu fCAR lxor 1 = 1);
              1
          | _ -> decode_error_y ())
      | _ -> decode_error_z ())
  | _ -> decode_error_x ()

(* let decode_execute cpu mem op =
   match int_of_char op with
   | 0x00 -> nop ()
   | 0x
   | _ -> fatal rc_DecodeError (Printf.sprintf "Failed to decode instruction %x" (int_of_char op)) *)
let step cpu mem =
  let opcode = read_byte cpu mem in
  decode_execute cpu mem opcode
