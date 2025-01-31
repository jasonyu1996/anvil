(** This module provides a set of helper functions for performing analyses on the event graph. *)

val event_traverse : EventGraph.event -> ((EventGraph.event -> unit) -> EventGraph.event -> unit) -> EventGraph.event list
val event_predecessors : EventGraph.event -> EventGraph.event list
val event_successors : EventGraph.event -> EventGraph.event list
val event_is_successor : EventGraph.event -> EventGraph.event -> bool
val event_is_predecessor : EventGraph.event -> EventGraph.event -> bool
val in_control_set_reg : EventGraph.event -> string -> bool
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
