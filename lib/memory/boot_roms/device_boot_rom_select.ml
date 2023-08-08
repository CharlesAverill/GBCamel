open Dmg_boot
open Mgb_boot
open Cgb_boot
open Sameboy_cgb_boot
open Gbcamel.Logging

type model = Dmg | Mgb | Cgb | SCgb

let is_cgb = function Cgb -> true | SCgb -> true | _ -> false

let get_device_and_boot_rom s =
  match String.uppercase_ascii s with
  | "DMG" -> (Dmg, dmg_boot_rom)
  | "MGB" -> (Mgb, mgb_boot_rom)
  | "CGB" -> (Cgb, cgb_boot_rom)
  | "SCGB" -> (SCgb, sameboy_cgb_boot_rom)
  | _ ->
      fatal rc_Error
        (Printf.sprintf
           "\"%s\" is not a recognized GB model, run [gbcamel -help] to see \
            options"
           s)
