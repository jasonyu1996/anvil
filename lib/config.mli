(** This module provides handling and parsing of configurations. *)

(** Set of configurations that control the compilation process. *)
type compile_config = {
  verbose : bool; (** output verbose details related to compilation to stderr *)
  input_filenames : string list; (** list of file names to be compiled *)
}

(** Parse the process arguments for a set of configurations. *)
val parse_args : unit -> compile_config

val debug_println : compile_config -> string -> unit
