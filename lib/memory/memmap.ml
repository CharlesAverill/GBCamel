(** memmap.ml - Memory-mapped location definitions  *)

(* List of 8-bit MemoryRegister addresses for lookup *)

(** Joypad info *)
let _P1 = 0xFF00

(** Serial transfer data *)
let _SB = 0xFF01

(** SIO Control *)
let _SC = 0xFF02

(** Divider Register *)
let _DIV = 0xFF04

(** Timer counter *)
let _TIMA = 0xFF05

(** Timer Modulo *)
let _TMA = 0xFF06

(** Timer Control *)
let _TAC = 0xFF07

(** Interrupt Flag *)
let _IF = 0xFF0F

(** Sound Mode 1 sweep *)
let _NR10 = 0xFF10

(** Sound Mode 1 wave pattern *)
let _NR11 = 0xFF11

(** Sound Mode 1 envelope *)
let _NR12 = 0xFF12

(** Sound Mode 1 frequency low *)
let _NR13 = 0xFF13

(** Sound Mode 1 frequency high *)
let _NR14 = 0xFF14

(** Sound Mode 2 wave pattern *)
let _NR21 = 0xFF16

(** Sound Mode 2 envelope *)
let _NR22 = 0xFF17

(** Sound Mode 2 frequency low *)
let _NR23 = 0xFF18

(** Sound Mode 2 frequency high *)
let _NR24 = 0xFF19

(** Sound Mode 3 sound on/off *)
let _NR30 = 0xFF1A

(** Sound Mode 3 sound length *)
let _NR31 = 0xFF1B

(** Sound Mode 3 output level *)
let _NR32 = 0xFF1C

(** Sound Mode 3 frequency low *)
let _NR33 = 0xFF1D

(** Sound Mode 3 frequency high *)
let _NR34 = 0xFF1E

(** Sound Mode 4 sound length *)
let _NR41 = 0xFF20

(** Sound Mode 4 envelope *)
let _NR42 = 0xFF21

(** Sound Mode 4 polynomial counter *)
let _NR43 = 0xFF22

(** Sound Mode 4 counter/consecutive *)
let _NR44 = 0xFF23

(** Channel Control/Volume *)
let _NR50 = 0xFF24

(** Sound output terminal *)
let _NR51 = 0xFF25

(** Sound on/off *)
let _NR52 = 0xFF26

(* FF30 - FF3F Wave pattern RAM *)

(** LCD Control *)
let _LCDC = 0xFF40

(** LCD Status *)
let _STAT = 0xFF41

(** Scroll Y *)
let _SCY = 0xFF42

(** Scroll X *)
let _SCX = 0xFF43

(** LCD Y coordinate *)
let _LY = 0xFF44

(** LY Compare *)
let _LYC = 0xFF45

(** DMA transfer/start address *)
let _OAM_DMA = 0xFF46

(** Background/Window palette data *)
let _BGP = 0xFF47

(** Object Palette 0 data *)
let _OBP0 = 0xFF48

(** Object Palette 1 data *)
let _OBP1 = 0xFF49

(** Window Y position *)
let _WY = 0xFF4A

(** Window X position *)
let _WX = 0xFF4B

(** Interrupt Enable *)
let _IE = 0xFFFF

(** 16 KB ROM Bank 00 (in cartridge, fixed at bank 00) *)
let _ROM_START = 0x0000

let _ROM_END = 0x3FFF
let _ROM_BANK_START = 0x4000
let _ROM_BANK_END = 0x7FFF

(** 8KB Video RAM (VRAM) *)
let _VRAM_START = 0x8000

let _VRAM_END = 0x9FFF

(** 8KB External RAM (in cartridge, switchable bank, if any) *)
let _ERAM_START = 0xA000

let _ERAM_END = 0xBFFF

(** 4KB Work RAM Bank 0 (WRAM) *)
let _WRAM_START = 0xC000

let _WRAM_END = 0xDFFF

(** Same as C000-DDFF (ECHO or Mirror RAM) (Typically not used) *)
let _ECHO_START = 0xE000

let _ECHO_END = 0xFDFF

(** 160 byte Sprite Attribute Table (OAM) *)
let _OAM_START = 0xFE00

let _OAM_END = 0xFE9F

(** 352 bytes of High RAM *)
let _HRAM_START = 0xFEA0

let _HRAM_END = 0xFFFF
