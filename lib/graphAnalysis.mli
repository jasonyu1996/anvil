(** This module provides a set of helper functions for performing analyses on the event graph. *)

val event_traverse : EventGraph.event -> ((EventGraph.event -> unit) -> EventGraph.event -> unit) -> EventGraph.event list


(** Compute predecessors of an event in the event graph.
    Does not include the event itself.
    Result in topological order.
*)
val event_predecessors : EventGraph.event -> EventGraph.event list

(** {!events_prepare_outs} must be called first.
    Does not include the event itself.
    Result in topological order. *)
val event_successors : EventGraph.event -> EventGraph.event list

(** {!events_prepare_outs} must be called first. *)
val event_is_successor : EventGraph.event -> EventGraph.event -> bool

val event_is_predecessor : EventGraph.event -> EventGraph.event -> bool

val in_control_set_endps : EventGraph.event -> string -> bool
val find_controller : string list -> EventGraph.event list -> EventGraph.event option
val find_first_msg_after : EventGraph.event -> Lang.message_specifier -> bool -> EventGraph.event option
val event_succ_msg_match_earliest : EventGraph.event -> Lang.message_specifier -> bool -> EventGraph.event option
val event_succ_msg_match_latest : EventGraph.event -> Lang.message_specifier -> bool -> EventGraph.event option
val event_distance_max : int
val event_slack_graph : EventGraph.event list -> EventGraph.event -> int Array.t

(** Compute the minimal weight among the successors of an event *)
val event_min_among_succ : EventGraph.event list -> int Array.t -> int Array.t

(** Compute the min distances from a specified event. The second event is the current event (the event known to
    have taken place). *)
val event_min_distance : EventGraph.event list -> EventGraph.event -> EventGraph.event -> int Hashtbl.Make(Int).t

(** Same as {!event_min_distance} but compute the max distances instead. *)
val event_max_distance : EventGraph.event list -> EventGraph.event -> EventGraph.event -> int Hashtbl.Make(Int).t

(** Check if two events are ordered in all possible traces in which both of them appear (also returns true if
    they never appear in the same trace as in two branches). *)
val events_are_ordered : EventGraph.event list -> EventGraph.event -> EventGraph.event -> bool


(** Visit events in topologically backward order *)
val events_visit_backward : (EventGraph.event -> unit) -> EventGraph.event list -> unit

(** Visit events in topologically forward order *)
val events_visit_forward : (EventGraph.event -> unit) -> EventGraph.event list -> unit

(** Compute the minimum distances to the nearest root among predecessors *)
(* val events_min_dist_to_root : EventGraph.event list -> int Array.t *)

val events_prepare_outs : EventGraph.event list -> unit

(** [event_is_dominant e e'] checks if event [e'] is dominant over [e]: [e']
- appears in all traces with [e], and
- is reached {i no later than} [e].

Note that this is different from {!event_is_predecessor}. [e'] can be a predecessor
of [e] without being dominant.
*)
val event_is_dominant : EventGraph.event -> EventGraph.event -> bool
