(**
Backend that handles code generation. This currently supports the SystemVerilog target.
*)

(** Generate code for a {!type:EventGraph.event_graph_collection} to a specified output channel. *)
val generate : out_channel -> Config.compile_config -> EventGraph.event_graph_collection -> unit
