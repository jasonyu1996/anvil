(** Basic operations for event graphs. *)

(** Print the structure of the graph to standard error stream. *)
val print_graph : EventGraph.event_graph -> unit

(** Print the graph in dot format to output. *)
val print_dot_graph : EventGraph.event_graph -> out_channel -> unit

val lifetime_const : EventGraph.event -> EventGraph.lifetime
val lifetime_immediate : EventGraph.event -> EventGraph.lifetime

(** A {!EventGraph.subreg_range} that covers a full register. *)
val full_reg_range : Lang.identifier -> int -> EventGraph.subreg_range

(** Return [true] if we don't know for sure that two subreg ranges don't intersect. *)
val subreg_ranges_possibly_intersect : EventGraph.subreg_range -> EventGraph.subreg_range -> bool

(** Return the first event at the other side of the branch. *)
val branch_other_side : EventGraph.branch_side_info -> EventGraph.event

(** Find the last event (recurse) in a given event graph. *)
val find_last_event : EventGraph.event_graph -> EventGraph.event
