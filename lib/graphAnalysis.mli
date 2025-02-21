(** This module provides a set of helper functions for performing analyses on the event graph. *)

val event_traverse : EventGraph.event -> ((EventGraph.event -> unit) -> EventGraph.event -> unit) -> EventGraph.event list


(** Compute predecessors of an event in the event graph.
    Does not include the event itself.
    Result in topological order.
*)
val event_predecessors : EventGraph.event -> EventGraph.event list

(** {!events_prepare_outs} must be called first.
    Includes the event itself.
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
val events_are_ordered : EventGraph.event list -> (Lang.message_specifier -> Lang.message_def option)
                        -> EventGraph.event -> EventGraph.event -> bool


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

(** Compute an approximate min distance for each predecessor of an event.
    The approximation can be smaller than the actual possible distance.
    The return results use [-1] to indicate that an event is not a predecessor. *)
val events_pred_min_dist : EventGraph.event -> int Array.t

(** Compute approximate max distance from a given event to {i any} event in the graph.
    This does not require the two events to be on a shared path. The approximation
    can be larger than the actual possible distance.

    If an event shares no trace with the given event, the result is [-event_distance_max].

    This is essentially a newer version of {!event_slack_graph} that
    handles branches correctly but is more relaxed for graphs
    without branches.
*)
val events_max_dist : EventGraph.event list -> (Lang.message_specifier -> Lang.message_def option)
                        -> EventGraph.event -> int Array.t

(** Returns all events with a specific message. Those are the {i until} events of messages (when
    send/recv completes). *)
val events_with_msg : EventGraph.event list -> Lang.message_specifier -> EventGraph.event list

(** Returns all events that start a specific message. *)
val events_start_msg : EventGraph.event list -> Lang.message_specifier -> EventGraph.event list

(** Returns an array indicating whether each event is reachable when a given event is reached. *)
val events_reachable : EventGraph.event list -> EventGraph.event -> bool Array.t

(** Over-approximation of first events with send/recv of a given message
    {i after} (with greater timestamp than) a given event. *)
val events_first_msg : EventGraph.event list -> EventGraph.event -> Lang.message_specifier -> EventGraph.event list

(** Sort in topological order. *)
val toposort : EventGraph.event list -> EventGraph.event list
