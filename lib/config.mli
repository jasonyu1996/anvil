(** This module provides handling and parsing of configurations. *)

(** Set of configurations that control the compilation process. *)
type compile_config = {
  verbose : bool; (** output verbose details related to compilation to stderr *)
  disable_lt_checks : bool; (** disable all lifetime and borrow related checks *)
  weak_typecasts : bool; (** allow weak typecasts, i.e., allow type mismatches
                             between data types of different widths *)
  opt_level : int; (** optimisation level *)                             
  output_filename : string option; (** optional output filename *)

    just_check : bool; (** only typecheck and validate the input files without generating output *)
  two_round_graph: bool; (** enable codegen of two rounds for each thread
                             NOTE: with general recursive graphs, this may not be
                             literally two rounds *)

  json_output : bool; (** output compilation results in JSON format *)
  input_filenames : string list; (** list of file names to be compiled *)
}

(** Parse the process arguments for a set of configurations. *)
val parse_args : unit -> compile_config

val debug_println : compile_config -> string -> unit
