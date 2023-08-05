(** logging.ml - Custom logging and error messages *)

open Globals

type log_type =
  | Log_None
  | Log_Debug
  | Log_Info
  | Log_Warning
  | Log_Error
  | Log_Critical

let int_of_log = function
  | Log_Debug -> 1
  | Log_Info -> 2
  | Log_Warning -> 3
  | Log_Error -> 4
  | Log_Critical -> 5
  | Log_None -> 0

let log_of_int = function
  | 1 -> Log_Debug
  | 2 -> Log_Info
  | 3 -> Log_Warning
  | 4 -> Log_Error
  | 5 -> Log_Critical
  | _ -> Log_None

type return_code = int * string

let rc_Ok = (0, "OK")
and rc_Error = (1, "ERROR")
and rc_MemError = (2, "MEMORY ERROR")
and rc_FileError = (3, "FILE ERROR")
and rc_ArgError = (4, "ARGUMENT ERROR")
and rc_DecodeError = (5, "DECODE ERROR")

exception LogError of string

let ansi_bold = "\x1b[1m"
let ansi_red = "\x1b[38:5:196m"
let ansi_orange = "\x1b[38:5:208m"
let ansi_yellow = "\x1b[38:5:178m"
let ansi_reset = "\x1b[0m"
let error_red = ansi_bold ^ ansi_red
let error_orange = ansi_bold ^ ansi_orange
let error_yellow = ansi_bold ^ ansi_yellow

let string_of_log = function
  | Log_Debug -> ansi_bold ^ "[DEBUG]"
  | Log_Info -> ansi_bold ^ "[INFO]"
  | Log_Warning -> ansi_yellow ^ "[WARNING]"
  | Log_Error -> ansi_orange ^ "[ERROR]"
  | Log_Critical -> ansi_red ^ "[CRITICAL]"
  | Log_None -> ansi_reset ^ "[NONE]"

let fatal rc message =
  Printf.fprintf stderr
    "%s[%s] - %s%s\n----------------------------------------\n" error_red
    (snd rc) ansi_reset message;
  flush stderr;
  exit (fst rc)

let _log log_level message =
  if log_level = Log_None || !_GLOBAL_LOG_LEVEL > int_of_log log_level then ()
  else
    let stream =
      if log_level = Log_Debug || log_level = Log_Info then stdout else stderr
    in
    Printf.fprintf stream "LOG:%s%s - %s\n" (string_of_log log_level) ansi_reset
      message
