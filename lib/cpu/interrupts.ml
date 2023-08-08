(** interrupts.ml - Logic for CPU Interrupts *)

open Utils.U8

type interrupt_handler = {
  cycles_since_last_interrupt : int ref;
  interrupts_enabled : bool ref;
  _IE : u8 ref;
  _IF : u8 ref;
}

let _init_interrupt_handler () =
  {
    cycles_since_last_interrupt = ref 0;
    interrupts_enabled = ref false;
    _IE = ref (u8_of_int 0xFA);
    _IF = ref (u8_of_int 0x00);
  }

let enable_interrupts ih = ih.interrupts_enabled := true
let disable_interrupts ih = ih.interrupts_enabled := false
let execute_next _ih = 0
