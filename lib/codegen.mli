(**
Backend that handles code generation. This currently supports the SystemVerilog target.
*)

(** Generate code for external import. *)
val generate_extern_import : out_channel -> string -> unit

(** Generate preamble at the top of the output file. *)
val generate_preamble : out_channel -> unit

(** Generate code for a {!type:EventGraph.event_graph_collection} to a specified output channel. *)
val generate : out_channel -> Config.compile_config -> EventGraph.event_graph_collection -> unit
