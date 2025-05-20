(** Perform lifetime check on an event graph and throw out {!EventGraph.LifetimeCheckError} if failed. *)
val lifetime_check : Config.compile_config -> EventGraph.cunit_info -> EventGraph.proc_graph -> EventGraph.event_graph -> unit
