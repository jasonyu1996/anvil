(** Construct a collection of event graphs from a compilation unit.
If lifetime checks are not disabled in the configuration,
this also performs lifetime checking and throws {!EventGraph.LifetimeCheckError}
and {!EventGraph.EventGraphError} upon failure.
*)
val build : Config.compile_config -> BuildScheduler.build_scheduler ->
    string -> Lang.param_value list ->
    Lang.compilation_unit -> EventGraph.event_graph_collection

(** Lightweight checks on the syntax tree. This should be performed first before
    calling {!build}.
*)
val syntax_tree_precheck : Config.compile_config -> Lang.compilation_unit -> unit
