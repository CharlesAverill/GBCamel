(** constants.ml - Physical constants of GB hardware - {{:https://github.com/mattbruv/Gameboy-Crust/blob/master/src/core/gpu.rs} Pulled from GBCrust} *)

(** Actual screen width *)
let _PHYSICAL_W = 160
(** Actual screen height*)
let _PHYSICAL_H = 144

let _SCANLINE_CYCLES = 456
let _OAM_CYCLES = 80
let _TRANSFER_CYCLES = _OAM_CYCLES + 172
let _HBLANK_CYCLES = _SCANLINE_CYCLES - _TRANSFER_CYCLES
let _FRAME_CYCLES = _SCANLINE_CYCLES * _PHYSICAL_H
let _VBLANK_CYCLES = _FRAME_CYCLES + 4560
