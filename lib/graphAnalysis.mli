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
val event_min_distance : EventGraph.event list -> EventGraph.event -> int Hashtbl.Make(Int).t
val event_max_distance : EventGraph.event list -> EventGraph.event -> int Hashtbl.Make(Int).t
