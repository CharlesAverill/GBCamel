open Utils.B64_decode

let mgb_boot_b64 =
  "Mf7/ryH/nzLLfCD7ISb/DhE+gDLiDD7z4jI+d3c+/OBHEQQBIRCAGs2VAM2WABN7/jQg8xHYAAYIGhMiIwUg+T4Z6hCZIS+ZDgw9KAgyDSD5Lg8Y82c+ZFfgQj6R4EAEHgIODPBE/pAg+g0g9x0g8g4TJHweg/5iKAYewf5kIAZ74gw+h+LwQpDgQhUg0gUgTxYgGMtPBgTFyxEXwcsRFwUg9SIjIiPJzu1mZswNAAsDcwCDAAwADQAIER+IiQAO3Mxu5t3d2Zm7u2djbg7szN3cmZ+7uTM+PEK5pbmlQjwhBAERqAAaE74g/iN9/jQg9QYZeIYjBSD7hiD+Pv/gUA=="

let mgb_boot_rom () = decode_base64_to_bytes mgb_boot_b64
