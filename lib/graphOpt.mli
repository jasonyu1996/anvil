(** Optimization passes for {!EventGraph}s. *)

(** The second argument specifies if the graph is intended for lifetime checks. This does not touch the old graph but instead
    returns a new one. *)
val optimize : Config.compile_config -> bool ->
    EventGraph.event_graph_collection ->
    EventGraph.proc_graph -> EventGraph.event_graph -> EventGraph.event_graph
