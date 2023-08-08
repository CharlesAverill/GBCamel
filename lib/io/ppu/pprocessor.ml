(** pprocessor.ml - PPU Logic *)

open Io.Virtual_register
open Gbcamel.Logging

type ppu = {
  lcdc : virtual_register;
  stat : virtual_register;
  lyc : virtual_register;
  ly : virtual_register;
  scanline_cycles : int ref;
  frame_cycles : int ref;
}
(** Emulates PPU state*)

(** Returns initialized PPU state *)
let _init_ppu () =
  {
    lcdc = ref 0;
    stat = ref 0;
    lyc = ref 0;
    ly = ref 0;
    scanline_cycles = ref 0;
    frame_cycles = ref 0;
  }

(** Synchronizes the PPU with other hardware *)
let update_ppu_cycles ppu cycles =
  ppu.scanline_cycles := !(ppu.scanline_cycles) + cycles

(** Represents which phase the PPU is in *)
type ppu_status = Hblank | Vblank | OAM | Transfer

(** Compute the current PPU phase based on the STAT virtual register *)
let get_mode ppu =
  let mode = _get ppu.stat land 0x3 in
  match mode with
  | 0 -> Hblank
  | 1 -> Vblank
  | 2 -> OAM
  | 3 -> Transfer
  | _ ->
      fatal rc_PPUError
        (Printf.sprintf "Decoded PPU mode as %d, range is [0:3]" mode)

(** Update the current PPU phase *)
let set_mode ppu mode = set ppu.stat (_get ppu.stat land (0x3 lxor -1) lor mode)
