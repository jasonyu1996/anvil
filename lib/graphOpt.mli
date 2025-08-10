(** Optimization passes for {!EventGraph}s. *)

(** The second argument specifies if the graph is intended for lifetime checks. This does not touch the old graph but instead
    returns a new one. *)
val optimize : Config.compile_config -> bool -> EventGraph.cunit_info -> EventGraph.event_graph -> EventGraph.event_graph
val combinational_codegen : Config.compile_config -> EventGraph.event_graph -> EventGraph.cunit_info -> EventGraph.event_graph