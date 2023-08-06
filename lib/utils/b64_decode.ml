open Base64

let decode_base64_to_bytes encoded_data =
  try Bytes.of_string (decode_exn encoded_data)
  with Failure reason -> failwith ("Base64 decoding error: " ^ reason)
