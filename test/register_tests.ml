open Cpu.Registers

(* Functions to test *)
module To_test = struct
  let a = a
  let b = b
  let c = c
  let d = d
  let e = e
  let f = f
  let h = h
  let l = l
  let sp = sp
  let pc = pc
  let af = af
  let bc = bc
  let de = de
  let hl = hl
  let set_a = set_a
  let set_b = set_b
  let set_c = set_c
  let set_d = set_d
  let set_e = set_e
  let set_f = set_f
  let set_h = set_h
  let set_l = set_l
  let set_sp = set_sp
  let set_pc = set_pc
  let set_af = set_af
  let set_bc = set_bc
  let set_de = set_de
  let set_hl = set_hl
  let string_of_regs = string_of_regs
end

(* 8-bit reads *)
let _8bit_reads_regs = _init_registers ()

let test_a () =
  Alcotest.(check char)
    "same char"
    (high !(_8bit_reads_regs.af))
    (To_test.a _8bit_reads_regs)

let test_f () =
  Alcotest.(check char)
    "same char"
    (low !(_8bit_reads_regs.af))
    (To_test.f _8bit_reads_regs)

let test_b () =
  Alcotest.(check char)
    "same char"
    (high !(_8bit_reads_regs.bc))
    (To_test.b _8bit_reads_regs)

let test_c () =
  Alcotest.(check char)
    "same char"
    (low !(_8bit_reads_regs.bc))
    (To_test.c _8bit_reads_regs)

let test_d () =
  Alcotest.(check char)
    "same char"
    (high !(_8bit_reads_regs.de))
    (To_test.d _8bit_reads_regs)

let test_e () =
  Alcotest.(check char)
    "same char"
    (low !(_8bit_reads_regs.de))
    (To_test.e _8bit_reads_regs)

let test_h () =
  Alcotest.(check char)
    "same char"
    (high !(_8bit_reads_regs.hl))
    (To_test.h _8bit_reads_regs)

let test_l () =
  Alcotest.(check char)
    "same char"
    (low !(_8bit_reads_regs.hl))
    (To_test.l _8bit_reads_regs)

(* 16-bit reads *)
let _16bit_reads_regs = _init_registers ()

let test_af () =
  Alcotest.(check int)
    "same int" !(_16bit_reads_regs.af)
    (To_test.af _16bit_reads_regs)

let test_bc () =
  Alcotest.(check int)
    "same int" !(_16bit_reads_regs.bc)
    (To_test.bc _16bit_reads_regs)

let test_de () =
  Alcotest.(check int)
    "same int" !(_16bit_reads_regs.de)
    (To_test.de _16bit_reads_regs)

let test_hl () =
  Alcotest.(check int)
    "same int" !(_16bit_reads_regs.hl)
    (To_test.hl _16bit_reads_regs)

let test_sp () =
  Alcotest.(check int)
    "same int" !(_16bit_reads_regs.sp)
    (To_test.sp _16bit_reads_regs)

let test_pc () =
  Alcotest.(check int)
    "same int" !(_16bit_reads_regs.pc)
    (To_test.pc _16bit_reads_regs)

(* 8-bit writes *)
let _8bit_writes_regs = _init_registers ()

let test_set_a () =
  Alcotest.(check char)
    "same char" '\237'
    (let _ = To_test.set_a _8bit_writes_regs (`R8 '\237') in
     a _8bit_writes_regs)

let test_set_b () =
  Alcotest.(check char)
    "same char" '\237'
    (let _ = To_test.set_b _8bit_writes_regs (`R8 '\237') in
     b _8bit_writes_regs)

let test_set_c () =
  Alcotest.(check char)
    "same char" '\237'
    (let _ = To_test.set_c _8bit_writes_regs (`R8 '\237') in
     c _8bit_writes_regs)

let test_set_d () =
  Alcotest.(check char)
    "same char" '\237'
    (let _ = To_test.set_d _8bit_writes_regs (`R8 '\237') in
     d _8bit_writes_regs)

let test_set_e () =
  Alcotest.(check char)
    "same char" '\237'
    (let _ = To_test.set_e _8bit_writes_regs (`R8 '\237') in
     e _8bit_writes_regs)

let test_set_f () =
  Alcotest.(check char)
    "same char" '\237'
    (let _ = To_test.set_f _8bit_writes_regs (`R8 '\237') in
     f _8bit_writes_regs)

let test_set_h () =
  Alcotest.(check char)
    "same char" '\237'
    (let _ = To_test.set_h _8bit_writes_regs (`R8 '\237') in
     h _8bit_writes_regs)

let test_set_l () =
  Alcotest.(check char)
    "same char" '\237'
    (let _ = To_test.set_l _8bit_writes_regs (`R8 '\237') in
     l _8bit_writes_regs)

(* 16-bit writes *)
let _16bit_writes_regs = _init_registers ()

let test_set_af () =
  Alcotest.(check int)
    "same int" 48088
    (let _ = To_test.set_af _16bit_writes_regs 48088 in
     af _16bit_writes_regs)

let test_set_bc () =
  Alcotest.(check int)
    "same int" 48088
    (let _ = To_test.set_bc _16bit_writes_regs 48088 in
     bc _16bit_writes_regs)

let test_set_de () =
  Alcotest.(check int)
    "same int" 48088
    (let _ = To_test.set_de _16bit_writes_regs 48088 in
     de _16bit_writes_regs)

let test_set_hl () =
  Alcotest.(check int)
    "same int" 48088
    (let _ = To_test.set_hl _16bit_writes_regs 48088 in
     hl _16bit_writes_regs)

let test_set_sp () =
  Alcotest.(check int)
    "same int" 48088
    (let _ = To_test.set_sp _16bit_writes_regs 48088 in
     sp _16bit_writes_regs)

let test_set_pc () =
  Alcotest.(check int)
    "same int" 48088
    (let _ = To_test.set_pc _16bit_writes_regs 48088 in
     pc _16bit_writes_regs)

let test_string_of_regs () =
  Alcotest.(check string)
    "same string"
    "AF: 0x01b0\n\
     \tA: 0x01\n\
     \tF: 0xb0\n\
     BC: 0x0013\n\
     \tB: 0x00\n\
     \tC: 0x13\n\
     DE: 0x00d8\n\
     \tD: 0x00\n\
     \tE: 0xd8\n\
     HL: 0x014d\n\
     \tH: 0x01\n\
     \tL: 0x4d\n\
     SP: 0xfffe\n\
     PC: 0x0150"
    (To_test.string_of_regs (_init_registers ()))

(* Run tests *)
let () =
  let open Alcotest in
  run "Registers"
    [
      ( "read-8bit",
        [
          test_case "A" `Quick test_a;
          test_case "B" `Quick test_b;
          test_case "C" `Quick test_c;
          test_case "D" `Quick test_d;
          test_case "E" `Quick test_e;
          test_case "F" `Quick test_f;
          test_case "H" `Quick test_h;
          test_case "L" `Quick test_l;
        ] );
      ( "read-16bit",
        [
          test_case "AF" `Quick test_af;
          test_case "BC" `Quick test_bc;
          test_case "DE" `Quick test_de;
          test_case "HL" `Quick test_hl;
          test_case "SP" `Quick test_sp;
          test_case "PC" `Quick test_pc;
        ] );
      ( "write-8bit",
        [
          test_case "A" `Quick test_set_a;
          test_case "B" `Quick test_set_b;
          test_case "C" `Quick test_set_c;
          test_case "D" `Quick test_set_d;
          test_case "E" `Quick test_set_e;
          test_case "F" `Quick test_set_f;
          test_case "H" `Quick test_set_h;
          test_case "L" `Quick test_set_l;
        ] );
      ( "write-16bit",
        [
          test_case "AF" `Quick test_set_af;
          test_case "BC" `Quick test_set_bc;
          test_case "DE" `Quick test_set_de;
          test_case "HL" `Quick test_set_hl;
          test_case "SP" `Quick test_set_sp;
          test_case "PC" `Quick test_set_pc;
        ] );
      ("misc", [ test_case "string_of_regs" `Quick test_string_of_regs ]);
    ]
