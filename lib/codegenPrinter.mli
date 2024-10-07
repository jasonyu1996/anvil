(** This module provides utilities for code printing, used during code generation.
Central to this is a type {!CodegenPrinter.t} which maintains the code printer state.
*)

(** A type that maintains the code printer state. *)
type t

(** Create a code printer that prints to a specified output channel.
The integer argument specifies the indentation width.
*)
val create : out_channel -> int -> t

(** Print a line. Arguments [lvl_delta_pre] and [lvl_delta_post] specify the change of the indentation
level before and after printing the line, respectively, and both default to [0].
*)
val print_line : ?lvl_delta_pre : int -> ?lvl_delta_post : int -> t -> string -> unit

(** Similar to {!print_line} but prints out a list of lines instead of a single line. *)
val print_lines : ?lvl_delta_pre : int -> ?lvl_delta_post : int -> t -> string list -> unit

(** Adjust the indentation level of a code printer by a specified amount. *)
val adjust_level : int -> t -> unit
