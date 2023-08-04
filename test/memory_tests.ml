open Memory.Ram

(* Functions to test *)
module To_test = struct
  let address_in_bounds = address_in_bounds
  let get_ram_bank = get_ram_bank
  let get_sub_bank = get_sub_bank
  let string_of_bank = string_of_bank
  let ram_read = ram_read
  let ram_write = ram_write
end

let _8byte_bank = get_ram_bank 8

let test_address_in_bounds_low () =
  Alcotest.(check bool)
    "same bool" true
    (To_test.address_in_bounds _8byte_bank 0)

let test_address_in_bounds_high () =
  Alcotest.(check bool)
    "same bool" true
    (To_test.address_in_bounds _8byte_bank 7)

let test_address_in_bounds_oob_low () =
  Alcotest.(check bool)
    "same bool" false
    (To_test.address_in_bounds _8byte_bank (-1))

let test_address_in_bounds_oob_high () =
  Alcotest.(check bool)
    "same bool" false
    (To_test.address_in_bounds _8byte_bank 9)

let test_get_ram_bank () =
  Alcotest.(check bytes)
    "same bytes" (Bytes.make 5 '\000') (To_test.get_ram_bank 5)

let test_ram_write () =
  Alcotest.(check char)
    "same char" 'h'
    (let _ =
       To_test.ram_write _8byte_bank 0 'h';
       To_test.ram_write _8byte_bank 1 'i';
       To_test.ram_write _8byte_bank 2 '!'
     in
     Bytes.get _8byte_bank 0)

let test_ram_read () =
  Alcotest.(check char) "same char" '!' (To_test.ram_read _8byte_bank 2)

let test_get_sub_bank () =
  Alcotest.(check bytes)
    "same bytes" (Bytes.of_string "hi!")
    (To_test.get_sub_bank _8byte_bank 0 2)

let test_string_of_bank () =
  Alcotest.(check string)
    "same string" "hi!\000\000\000\000\000"
    (To_test.string_of_bank _8byte_bank)

(* Run tests *)
let () =
  let open Alcotest in
  run "Memory"
    [
      ( "RAM",
        [
          test_case "address-in-bounds-low" `Quick test_address_in_bounds_low;
          test_case "address-in-bounds-high" `Quick test_address_in_bounds_high;
          test_case "address-in-bounds-oob-low" `Quick
            test_address_in_bounds_oob_low;
          test_case "address-in-bounds-oob-high" `Quick
            test_address_in_bounds_oob_high;
          test_case "get-ram-bank" `Quick test_get_ram_bank;
          test_case "get-ram-write" `Quick test_ram_write;
          test_case "get-ram-read" `Quick test_ram_read;
          test_case "get-sub-bank" `Quick test_get_sub_bank;
          test_case "string-of-bank" `Quick test_string_of_bank;
        ] );
    ]
