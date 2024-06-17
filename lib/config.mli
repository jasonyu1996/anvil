type compile_config = { verbose : bool; input_filenames : string list; }
val parse_args : unit -> compile_config
val debug_println : compile_config -> string -> unit
