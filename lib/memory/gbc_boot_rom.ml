open Utils.B64_decode

let boot_rom_encoded_string =
  "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"

let gbc_boot_rom () = decode_base64_to_bytes boot_rom_encoded_string