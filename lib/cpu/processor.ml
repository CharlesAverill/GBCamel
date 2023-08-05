(** processor.ml - CPU logic *)

open Registers

type cpu = { regs : registers }

let _init_cpu = { regs = _init_registers () }
