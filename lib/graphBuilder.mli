(** Construct a collection of event graphs from a compilation unit.
If lifetime checks are not disabled in the configuration,
this also performs lifetime checking and throws {!EventGraph.LifetimeCheckError}
and {!EventGraph.EventGraphError} upon failure.
*)
val build : Config.compile_config -> Lang.compilation_unit -> EventGraph.event_graph_collection
