(** processor.ml - CPU logic *)

open Flags
open Conditions
open Registers
open Memory.Vmem
open Utils.U16
open Gbcamel.Logging

type processor = { regs : registers; mem : mem_ctrl; halted : bool ref }
(** Type for storing information about CPU state *)

(** Returns an initialized CPU state *)
let _init_processor mem =
  let _ = _log Log_Info "CPU Initialized" in
  { regs = _init_registers (); mem; halted = ref false }

(** Read the next byte at position (PC) and increment PC *)
let read_byte cpu =
  let instr = read cpu.mem (pc cpu.regs) in
  let _ = set_pc cpu.regs (u16_add (pc cpu.regs) 1) in
  instr

(** Read the next word (2 bytes) at position (PC) and increment PC twice *)
let read_word cpu =
  let low = read_byte cpu in
  let high = read_byte cpu in
  combine high low

(** Check if a specific flag is set in the F register *)
let is_flag_set cpu flag =
  if r16_of_r8 (f cpu.regs) land flag = 1 then true else false

(** Check if a specific flag is set in the F register - returns an int *)
let is_flag_set_b cpu flag = if is_flag_set cpu flag then 1 else 0

(** Set a flag in the F register *)
let set_flag cpu flag value =
  if value then set_f cpu.regs (`R16 (r16_of_r8 (f cpu.regs) lor flag))
  else set_f cpu.regs (`R16 (r16_of_r8 (f cpu.regs) land (flag lxor -1)))

(** Get the register read function corresponding to rp[n] *)
let rp_table_read n =
  match n with
  | 0 -> bc
  | 1 -> de
  | 2 -> hl
  | 3 -> sp
  | _ ->
      fatal rc_DecodeError
        (Printf.sprintf "Tried to decode rp[%d] for reading, range is [0:4]" n)

(** Get the register write function corresponding to rp[n] *)
let rp_table_write n =
  match n with
  | 0 -> set_bc
  | 1 -> set_de
  | 2 -> set_hl
  | 3 -> set_sp
  | _ ->
      fatal rc_DecodeError
        (Printf.sprintf "Tried to decode rp[%d] for writing, range is [0:4]" n)

(** Get the register read function corresponding to r\[n\] *)
let r_table_read cpu r =
  match r with
  | 0 -> (0, b)
  | 1 -> (0, c)
  | 2 -> (0, d)
  | 3 -> (0, e)
  | 4 -> (0, h)
  | 5 -> (0, l)
  | 6 -> (1, fun x -> read cpu.mem (hl x))
  | 7 -> (0, a)
  | _ ->
      fatal rc_DecodeError
        (Printf.sprintf "Tried to decode r[%d] for reading, range is [0:7]" r)

(** Get the register write function corresponding to r[n] *)
let r_table_write cpu r data =
  match r with
  | 0 -> set_b cpu.regs (`R8 data)
  | 1 -> set_c cpu.regs (`R8 data)
  | 2 -> set_d cpu.regs (`R8 data)
  | 3 -> set_e cpu.regs (`R8 data)
  | 4 -> set_h cpu.regs (`R8 data)
  | 5 -> set_l cpu.regs (`R8 data)
  | 6 -> write cpu.mem (hl cpu.regs) data
  | 7 -> set_a cpu.regs (`R8 data)
  | _ ->
      fatal rc_DecodeError
        (Printf.sprintf "Tried to decode r[%d] for reading, range is [0:7]" r)

(** Push 16 bits onto the stack *)
let push cpu data =
  set_sp cpu.regs (u16_sub (sp cpu.regs) 1);
  write cpu.mem (sp cpu.regs) (high data);
  set_sp cpu.regs (u16_sub (sp cpu.regs) 1);
  write cpu.mem (sp cpu.regs) (low data)

(** Push 16 bits onto the stack from a set register in the RP2 table *)
let push_rp2 cpu idx =
  match idx with
  | 0 -> push cpu (bc cpu.regs)
  | 1 -> push cpu (de cpu.regs)
  | 2 -> push cpu (hl cpu.regs)
  | 3 -> push cpu (af cpu.regs)
  | _ ->
      fatal rc_DecodeError
        (Printf.sprintf "Tried to decode rp2[%d], range is [0:3]" idx)

(** Pop 16 bits from the stack *)
let pop cpu =
  let low = read cpu.mem (sp cpu.regs) in
  set_sp cpu.regs (u16_add (sp cpu.regs) 1);
  let high = read cpu.mem (sp cpu.regs) in
  set_sp cpu.regs (u16_add (sp cpu.regs) 1);
  combine high low

(** Pop 16 bits from the stack and load them into a set register in the RP2 table *)
let pop_rp2 cpu idx =
  let value = pop cpu in
  match idx with
  | 0 -> set_bc cpu.regs value
  | 1 -> set_de cpu.regs value
  | 2 -> set_hl cpu.regs value
  | 3 -> set_af cpu.regs (value land 0xFFF0)
  | _ ->
      fatal rc_DecodeError
        (Printf.sprintf "Tried to decode rp2[%d], range is [0:3]" idx)

(** No-operation *)
let nop () = 1

(* TODO : Figure out how to implement STOP *)
let stop _cpu = 1

(** Halts the CPU *)
let halt cpu =
  cpu.halted := true;
  1

(** Unconditional jump *)
let jp cpu address = set_pc cpu.regs address

(** Unconditional relative jump*)
let jr cpu offset = jp cpu (u16_add (pc cpu.regs) offset)

(** Conditional jump 
    
    jp_cond [cpu] [cond] [address]
*)
let jp_cond cpu cond address =
  let cond_met = ref false in
  let _ =
    match cond land 0b11 with
    | cond when cond = ccNZ ->
        if not (is_flag_set cpu fZER) then jp cpu address;
        cond_met := true
    | cond when cond = ccZ ->
        if is_flag_set cpu fZER then jp cpu address;
        cond_met := true
    | cond when cond = ccNC ->
        if not (is_flag_set cpu fCAR) then jp cpu address;
        cond_met := true
    | cond when cond = ccC ->
        if is_flag_set cpu fCAR then jp cpu address;
        cond_met := true
    | _ -> cond_decode_error cond
  in
  if !cond_met then 4 else 3

(** Conditional relative jump 
    
    jr_cond [cpu] [cond] [offset]
*)
let jr_cond cpu cond offset = jp_cond cpu cond (u16_add (pc cpu.regs) offset)

(** Call 
    
    call [cpu] [new_pc]
*)
let call cpu new_pc =
  push cpu (pc cpu.regs);
  set_pc cpu.regs new_pc

(** Conditional call
    
    call [cpu] [new_pc] [cond]
*)
let call_cond cpu new_pc cond =
  let cond_met = ref false in
  let _ =
    match cond land 0b11 with
    | cond when cond = ccNZ ->
        if not (is_flag_set cpu fZER) then call cpu new_pc;
        cond_met := true
    | cond when cond = ccZ ->
        if is_flag_set cpu fZER then call cpu new_pc;
        cond_met := true
    | cond when cond = ccNC ->
        if not (is_flag_set cpu fCAR) then call cpu new_pc;
        cond_met := true
    | cond when cond = ccC ->
        if is_flag_set cpu fCAR then call cpu new_pc;
        cond_met := true
    | _ -> cond_decode_error cond
  in
  if !cond_met then 6 else 3

(** Return *)
let ret cpu = set_pc cpu.regs (pop cpu)

(** Conditional return *)
let ret_cond cpu cond =
  match cond with
  | cond when cond = ccNZ ->
      if not (is_flag_set cpu fZER) then (
        ret cpu;
        5)
      else 2
  | cond when cond = ccZ ->
      if is_flag_set cpu fZER then (
        ret cpu;
        5)
      else 2
  | cond when cond = ccNC ->
      if not (is_flag_set cpu fCAR) then (
        ret cpu;
        5)
      else 2
  | cond when cond = ccC ->
      if is_flag_set cpu fCAR then (
        ret cpu;
        5)
      else 2
  | _ -> cond_decode_error cond

(** Restart *)
let rst cpu addr =
  if not (addr >= 0 && addr <= 7) then
    fatal rc_DecodeError
      (Printf.sprintf "Tried to restart with address %d, bounds are [0:7]" addr)
  else call cpu (addr * 8);
  4

(** (dest += src)%r16 *)
let add16 dest src = dest := u16_add !dest src

(** ALU table 
    
    alu [cpu] [table_idx] [operand]
*)
let alu cpu idx operand =
  let add_a addval use_carry =
    let octal_xF = '\017' in
    let aval, cval =
      (a cpu.regs, if use_carry && is_flag_set cpu fCAR then '\001' else '\000')
    in
    let rawval = r8_add_multi [ aval; addval; cval ] in
    set_flag cpu fCAR (r16_of_r8 aval + r16_of_r8 addval + r16_of_r8 cval > 0xFF);
    set_flag cpu fHCR
      (r8_add_multi [ r8_land aval octal_xF; r8_land addval octal_xF; cval ]
      > octal_xF);
    set_flag cpu fSUB false;
    set_flag cpu fZER (rawval = '\000');
    set_a cpu.regs (`R8 rawval)
  and sub_a addval use_carry =
    let octal_xF = '\017' in
    let aval, cval =
      (a cpu.regs, if use_carry && is_flag_set cpu fCAR then '\001' else '\000')
    in
    let rawval = r8_sub_multi [ aval; addval; cval ] in
    set_flag cpu fCAR (r16_of_r8 aval - r16_of_r8 addval - r16_of_r8 cval < 0);
    set_flag cpu fHCR
      (r16_of_r8 (r8_land aval octal_xF)
       - r16_of_r8 (r8_land addval octal_xF)
       - r16_of_r8 (r8_land cval octal_xF)
      < 0);
    set_flag cpu fSUB true;
    set_flag cpu fZER (rawval = '\000');
    set_a cpu.regs (`R8 rawval)
  and and_a andval =
    let rawval = r8_land (a cpu.regs) andval in
    set_flag cpu fCAR false;
    set_flag cpu fHCR true;
    set_flag cpu fSUB false;
    set_flag cpu fZER (rawval = '\000');
    set_a cpu.regs (`R8 rawval)
  and or_a orval =
    let rawval = r8_lor (a cpu.regs) orval in
    set_flag cpu fCAR false;
    set_flag cpu fHCR false;
    set_flag cpu fSUB false;
    set_flag cpu fZER (rawval = '\000');
    set_a cpu.regs (`R8 rawval)
  and xor_a xorval =
    let rawval = r8_lxor (a cpu.regs) xorval in
    set_flag cpu fCAR false;
    set_flag cpu fHCR false;
    set_flag cpu fSUB false;
    set_flag cpu fZER (rawval = '\000');
    set_a cpu.regs (`R8 rawval)
  in
  let cp_a cpval =
    let original_aval = a cpu.regs in
    let _ = sub_a cpval false in
    set_a cpu.regs (`R8 original_aval)
  in
  match idx with
  (* ADD A, Operand *)
  | 0 ->
      add_a operand false;
      1
  (* ADC A, Operand *)
  | 1 ->
      add_a operand true;
      1
  (* SUB A, Operand *)
  | 2 ->
      sub_a operand false;
      1
  (* SBC A, Operand *)
  | 3 ->
      sub_a operand true;
      1
  (* AND A, Operand *)
  | 4 ->
      and_a operand;
      1
  (* XOR A, Operand *)
  | 5 ->
      xor_a operand;
      1
  (* OR A, Operand *)
  | 6 ->
      or_a operand;
      1
  (* CP A, Operand *)
  | 7 ->
      cp_a operand;
      1
  | _ ->
      fatal rc_DecodeError
        (Printf.sprintf "Tried to decode ALU[%d] but range is [0:7]" idx)

(** Leftward bit-rotation operation 
    
    rotate_left [cpu] [to_rotate] [include_carry] [update_zero]
*)
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

(** Rightward bit-rotation operation 
    
    rotate_right [cpu] [to_rotate] [include_carry] [update_zero]
*)
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

(** Left bitshift 
    
    shift_left [cpu] [to_shift] *)
let shift_left cpu n' =
  let n = r16_of_r8 n' in
  let result = n lsl 1 in
  let msb = n lsr 7 in
  set_flag cpu fCAR (msb = 1);
  set_flag cpu fHCR false;
  set_flag cpu fSUB false;
  set_flag cpu fZER (result = 0);
  r8_of_int result

(** Right bitshift 
    
    shift_left [cpu] [to_shift] [keep_msb] *)
let shift_right cpu n' keep_msb =
  let n = r16_of_r8 n' in
  let result = if keep_msb then (n lsr 1) lor (n land 0x80) else n lsr 1 in
  set_flag cpu fCAR (n land 1 = 1);
  set_flag cpu fHCR false;
  set_flag cpu fSUB false;
  set_flag cpu fZER (result = 0);
  r8_of_int result

(** Swaps the highest MSBs and lowest MSBs
    
    swap [cpu] [to_swap]
*)
let swap cpu n' =
  let n = r16_of_r8 n' in
  let result = (n lsl 4) lor (n lsr 4) in
  set_flag cpu fCAR false;
  set_flag cpu fHCR false;
  set_flag cpu fSUB false;
  set_flag cpu fZER (result = 0);
  r8_of_int result

(** Test a bit 
    bit [cpu] [value] [bit_to_test]
*)
let bit cpu n bit =
  set_flag cpu fZER (r8_land bit (r8_negate n) <> '\000');
  set_flag cpu fHCR true;
  set_flag cpu fSUB false

(** Set a bit (just [value | bit])
   
   set [cpu] [value] [bit]
*)
let set = r8_lor

(** Reset a bit 
    
    res [cpu] [value] [bit]
*)
let res n bit = r8_land (r8_negate bit) n

(** Rotation table ROT *)
let rot cpu idx operand =
  let read_hl, readf = r_table_read cpu operand in
  let opvalue = readf cpu.regs in
  (match idx with
  | 0 ->
      (* RLC *)
      r_table_write cpu idx (rotate_left cpu opvalue false true)
  | 1 ->
      (* RRC *)
      r_table_write cpu idx (rotate_right cpu opvalue false true)
  | 2 ->
      (* RL *)
      r_table_write cpu idx (rotate_left cpu opvalue true true)
  | 3 ->
      (* RR *)
      r_table_write cpu idx (rotate_right cpu opvalue true true)
  | 4 -> (* SLA *) r_table_write cpu idx (shift_left cpu opvalue)
  | 5 -> (* SRA *) r_table_write cpu idx (shift_right cpu opvalue true)
  | 6 -> (* SWAP *) r_table_write cpu idx (swap cpu opvalue)
  | 7 -> (* SRL *) r_table_write cpu idx (shift_right cpu opvalue false)
  | _ -> fatal rc_DecodeError "Tried to decode ROT[%d] but range is [0:7]");
  (2 * read_hl) + 2

(** General decoder

    Layout is based on this table - https://gb-archive.github.io/salvage/decoding_gbz80_opcodes/Decoding%20Gamboy%20Z80%20Opcodes.html 

    Returns the number of cycles taken to execute the provided instruction
*)
let decode_execute cpu op' =
  let op = r16_of_r8 op' in
  let regs = cpu.regs in

  let x = (0b11000000 land op) lsr 6
  and y = (0b00111000 land op) lsr 3
  and z = 0b00000111 land op in
  let p = (0b110 land y) lsr 1 and q = 0b1 land y in

  let _ = (x, y, z, p, q, cpu) in

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

  let rec main_decoder () =
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
                let nn = read_word cpu in
                let src = sp regs in
                let _ =
                  write cpu.mem nn (low src);
                  write cpu.mem (nn + 1) (high src)
                in
                5
            (* STOP *)
            | 2 -> stop cpu
            (* JR d *)
            | 3 ->
                jr cpu (signed_int_of_r8 (read_byte cpu));
                3
            (* JR cc[y-4], d *)
            | y when y >= 4 && y <= 7 ->
                jr_cond cpu (y - 4) (signed_int_of_r8 (read_byte cpu))
            | _ -> decode_error_y ())
        | 1 -> (
            (* 16-bit load immediate/add *)
            match q with
            (* LD rp[p], nn *)
            | 0 ->
                (rp_table_write p) regs (read_word cpu);
                3
            (* ADD HL, rp[p] *)
            | 1 ->
                add16 regs.hl ((rp_table_read p) regs);
                2
            | _ -> decode_error_q ())
        | 2 -> (
            (* Indirect loading *)
            match q with
            | 0 -> (
                match p with
                (* LD (BC), A *)
                | 0 ->
                    write cpu.mem (bc regs) (a regs);
                    2
                (* LD (DE), A *)
                | 1 ->
                    write cpu.mem (de regs) (a regs);
                    2
                (* LD (HL+), A *)
                | 2 ->
                    write cpu.mem (hl regs) (a regs);
                    hli regs;
                    2
                (* LD (HL-), A *)
                | 3 ->
                    write cpu.mem (hl regs) (a regs);
                    hld regs;
                    2
                | _ -> decode_error_p ())
            | 1 -> (
                match p with
                (* LD A, (BC) *)
                | 0 ->
                    set_a regs (`R8 (read cpu.mem (bc regs)));
                    2
                (* LD A, (DE) *)
                | 1 ->
                    set_a regs (`R8 (read cpu.mem (de regs)));
                    2
                (* LD A, (HL+) *)
                | 2 ->
                    set_a regs (`R8 (read cpu.mem (hl regs)));
                    hli regs;
                    2
                (* LD A, (HL-) *)
                | 3 ->
                    set_a regs (`R8 (read cpu.mem (hl regs)));
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
            let read_hl, readf = r_table_read cpu y in
            r_table_write cpu y (r8_add (readf regs) '\001');
            if read_hl = 1 then 3 else 1
        | 5 ->
            (* 8-bit DEC *)
            let read_hl, readf = r_table_read cpu y in
            r_table_write cpu y (r8_sub (readf regs) '\001');
            if read_hl = 1 then 3 else 1
        | 6 ->
            (* LD r[y], n *)
            r_table_write cpu y (read_byte cpu);
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
            | 4 ->
                (* DAA  *)
                fatal rc_ImplementationError "DAA not yet implemented"
            | 5 ->
                (* CPL  *)
                set_a regs (`R16 (r16_of_r8 (r8_negate (a regs))));
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
    | 1 -> (
        (* 8-bit loading *)
        match z with
        (* LD r, r *)
        | z when z = y -> nop ()
        (* LD r[y], r[z] *)
        | z when z <> 6 ->
            let read_hl, readf = r_table_read cpu z in
            r_table_write cpu y (readf regs);
            read_hl + 1
        (* Exception (replaces LD (HL), (HL)) *)
        | 6 -> halt cpu
        | _ -> decode_error_z ())
    | 2 ->
        (* Operate on accumulator and register/memory location *)
        let read_hl, readf = r_table_read cpu z in
        read_hl + alu cpu y (readf regs)
    | 3 -> (
        match z with
        | 0 -> (
            (* Conditional return, mem-mapped register loads and stack operations *)
            match y with
            (* RET cc[y] *)
            | y when y >= 0 && y <= 3 -> ret_cond cpu y
            (* LD (0xFF00 + n), A *)
            | 4 ->
                write cpu.mem
                  (0xFF00 lor r16_of_r8 (read_byte cpu))
                  (a cpu.regs);
                3
            (* ADD SP, d *)
            | 5 ->
                let d = signed_int_of_r8 (read_byte cpu) in
                let result = u16_add (sp regs) d in
                set_flag cpu fCAR (result land 0xFF < sp cpu.regs land 0xFF);
                set_flag cpu fHCR (result land 0xF < sp cpu.regs land 0xF);
                set_flag cpu fZER false;
                set_flag cpu fSUB false;
                set_sp regs result;
                4
            (* LD A, (0xFF00 + n) *)
            | 6 ->
                set_a regs
                  (`R8 (read cpu.mem (0xFF00 land r16_of_r8 (read_byte cpu))));
                3
            (* LD HL, SP + d *)
            | 7 ->
                let d = signed_int_of_r8 (read_byte cpu) in
                let result = u16_add (sp regs) d in
                set_flag cpu fCAR (result land 0xFF < sp cpu.regs land 0xFF);
                set_flag cpu fHCR (result land 0xF < sp cpu.regs land 0xF);
                set_flag cpu fZER false;
                set_flag cpu fSUB false;
                set_hl regs result;
                3
            | _ -> decode_error_y ())
        | 1 -> (
            if (* POP & various ops *)
               q = 0 then (
              (* POP rp2[p] *)
              pop_rp2 cpu p;
              3)
            else
              match q with
              (* RET *)
              | 0 ->
                  ret cpu;
                  4
              (* RETI *)
              | 1 -> fatal rc_ImplementationError "RETI not yet implemented"
              (* JP HL *)
              | 2 ->
                  jp cpu (hl cpu.regs);
                  1
              (* LD SP, HL *)
              | 3 ->
                  set_sp regs (hl cpu.regs);
                  2
              | _ -> decode_error_q ())
        | 2 -> (
            (* Conditional jump *)
            match y with
            (* JP cc[y], nn *)
            | y when y >= 0 && y <= 3 -> jp_cond cpu y (read_word cpu)
            (* LD (0xFF00+C), A *)
            | 4 ->
                write cpu.mem (0xFF00 land r16_of_r8 (c regs)) (a regs);
                2
            (* LD (nn), A *)
            | 5 ->
                write cpu.mem (read_word cpu) (a regs);
                4
            (* LD A, (0xFF00+C) *)
            | 6 ->
                set_a regs (`R8 (read cpu.mem (0xFF00 land r16_of_r8 (c regs))));
                2
            (* LD A, (nn) *)
            | 7 ->
                set_a regs (`R8 (read cpu.mem (read_word cpu)));
                4
            | _ -> decode_error_y ())
        | 3 -> (
            (* Assorted operations *)
            match y with
            (* JP nn *)
            | 0 ->
                jp cpu (read_word cpu);
                4
            (* CB prefix *)
            | y when y >= 1 && y <= 5 -> _CB_decoder ()
            (* DI *)
            | 6 -> fatal rc_ImplementationError "DI not yet implemented"
            (* EI *)
            | 7 -> fatal rc_ImplementationError "EI not yet implemented"
            | _ -> decode_error_y ())
        | 4 -> (
            (* Conditional call *)
            match y with
            (* CALL cc[y], nn *)
            | y when y >= 0 && y <= 3 -> call_cond cpu (read_word cpu) y
            (* CB prefix *)
            | y when y >= 4 && y <= 7 -> _CB_decoder ()
            | _ -> decode_error_y ())
        | 5 -> (
            if (* PUSH & various ops *)
               q = 0 then (
              (* PUSH rp2[p] *)
              push_rp2 cpu p;
              4)
            else
              match p with
              (* CALL nn *)
              | 0 ->
                  call cpu (read_word cpu);
                  6
              | p when p >= 1 && p <= 3 -> _CB_decoder ()
              | _ -> decode_error_p ())
        | 6 ->
            (* Operate on accumulator and immediate operand *)
            (* alu[y] n *)
            alu cpu y (read_byte cpu) + 1
        | 7 ->
            (* Restart *)
            (* RST y*8 *)
            rst cpu y
        | _ -> decode_error_z ())
    | _ -> decode_error_x ()
  and _CB_decoder () =
    match x with
    | 0 ->
        (* Roll/shift register or memory location *)
        (* rot[y] r[z] *)
        rot cpu y z
    | 1 ->
        (* Test bit *)
        (* BIT y, r[z] *)
        let read_hl, readf = r_table_read cpu z in
        bit cpu (readf cpu.regs) (r8_of_int y);
        if read_hl = 0 then 2 else 3
    | 2 ->
        (* Reset bit *)
        (* RES y, r[z] *)
        let read_hl, readf = r_table_read cpu z in
        r_table_write cpu z (res (readf cpu.regs) (r8_of_int y));
        if read_hl = 0 then 2 else 4
    | 3 ->
        (* Set bit *)
        (* SET y, r[z] *)
        let read_hl, readf = r_table_read cpu z in
        r_table_write cpu z (set (readf cpu.regs) (r8_of_int y));
        if read_hl = 0 then 2 else 4
    | _ -> decode_error_x ()
  in
  main_decoder ()

(** Performs one step of the decode-execute cycle *)
let step cpu =
  if !(cpu.halted) then 1
  else
    let opcode = read_byte cpu in
    decode_execute cpu opcode
