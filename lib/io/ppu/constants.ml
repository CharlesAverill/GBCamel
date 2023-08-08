(** constants.ml - Physical constants of GB hardware - {{:https://github.com/mattbruv/Gameboy-Crust/blob/master/src/core/gpu.rs} Pulled from GBCrust} *)

(** Actual screen width *)
let _PHYSICAL_W = 160

(** Actual screen height*)
let _PHYSICAL_H = 144

(** Cycles taken for scanline mode to complete *)
let _SCANLINE_CYCLES = 456

(** Cycles taken for OAM mode to complete *)
let _OAM_CYCLES = 80

(** Cycles taken for transfer mode to complete *)
let _TRANSFER_CYCLES = _OAM_CYCLES + 172

(** Cycles taken for hblank mode to complete *)
let _HBLANK_CYCLES = _SCANLINE_CYCLES - _TRANSFER_CYCLES

(** Cycles taken to render full screen *)
let _FRAME_CYCLES = _SCANLINE_CYCLES * _PHYSICAL_H

(** Cycles taken to render vblank *)
let _VBLANK_CYCLES = _FRAME_CYCLES + 4560
