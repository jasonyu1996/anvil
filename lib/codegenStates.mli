(** Generate and print out code for states in a given design ({!EventGraph.event_graph}). *)
val codegen_states :
    CodegenPrinter.t ->
    EventGraph.event_graph_collection ->
    EventGraph.proc_graph ->
    EventGraph.event_graph -> unit
