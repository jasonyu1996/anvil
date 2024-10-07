(** An {{!event_graph}event graph} is a DAG that describes the relationships between time points
(aka {{!event}events}).
Usually, an event graph corresponds to a looping thread, but this might not be the case
when there are cross-loop synchronisations through shared wires.
The edges between events provide a full description of when and the circumstances under which
each event is reached.
They also entail a {i no-later-than} partial order between events.

As the event graph is handy for analysing timing order, it is used for lifetime checking.

This module provides the related types and methods to create, manipulate, and type-check
event graphs.
*)

type wire = WireCollection.wire
type wire_collection = WireCollection.t

(** An event that may lead to delay in time. *)
type atomic_delay = [
  | `Cycles of int (** elapse of a specific number of cycles *)
  | `Send of Lang.message_specifier (** sending of a message *)
  | `Recv of Lang.message_specifier (** receiving of a message *)
]

(** Describe a time window that starts with an eventl ({!live}) and ends with an
{{!event_pat}event pattern} ({!dead}) *)
type lifetime = {
  live : event;
  dead : event_pat;
}

(** Data with a lifetime and potentially borrowing from a set of registers. *)
and timed_data = {
  w : wire option; (** the {!type:wire} carrying the underlying raw data *)
  lt : lifetime; (** lifetime of the data *)
  reg_borrows : (Lang.identifier * event) list; (** each is tuple of (name of register, starting event of the borrow) *)
}

(** An action that is performed instantly when an event is reached. *)
and action =
  | DebugPrint of string * timed_data list (** debug print ([dprint]) *)
  | DebugFinish (** [dfinish] *)
  | RegAssign of string * timed_data (** register assignment (technically this is not performed instantly) *)

(** Type of an action that may take multiple cycles. Those
are basically those that synchronise through message passing. *)
and sustained_action_type =
  | Send of Lang.message_specifier * timed_data
  | Recv of Lang.message_specifier

(** A condition.

Currently, an [if-then-else] expression produces a pair of events representing
the two cases respectively and both are associated with conditions.
The [then-] case is associated with a condition with [false] as {!neg}, whereas
the [else-] case is associated with a condition with [true] as {!neg}.
*)
and condition = {
  data : timed_data; (** the time data evaluated in the condition *)
  neg : bool; (** is the data negated? *)
}

(** An event pattern. Matched when a {{!Lang.delay_pat}delay pattern} is first satisfied
after a certain event is reached.
*)
and event_pat = (event * Lang.delay_pat) list

(** An event. *)
and event = {
  id : int; (** an integral identifier of the event, unique within the event graph *)
  mutable actions: action list; (** instant actions that take place when this event is reached *)
  mutable sustained_actions : sustained_action list; (** actions that may take multiple cycles and
                                                      start when this event is reached*)
  source: event_source; (** under what circumstances is this event reached.
                      {i Those are effectively the edges in the event graph} *)
  (* for lifetime checking *)
  mutable control_regs: (int * int) Utils.string_map;
  mutable control_endps: (int * int) Utils.string_map;
  mutable current_regs : (int * int) Utils.string_map;
  mutable current_endps : (int * int) Utils.string_map;
  (** used for lifetime checking *)
  mutable outs : event list; (** the outbound edges, i.e., the events that directly depend on this event *)
}

(** Describes when an event is reached. *)
and event_source = [
  | `Root (** at the beginning of the thread (initially already reached) *)
  | `Later of event * event (** reached when both events have been reached *)
  | `Seq of event * atomic_delay (** reached when a {{!atomic_delay}delay} takes place after another event is reached *)
  | `Branch of condition * event (** reached when a {{!condition}condition} is satisfied after another event is reached *)
  | `Either of event * event (** reached when either of the two events is reached *)
]

(** An action that starts at an event but may last multiple cycles.
It is in effect until {!until} is reached. *)
and sustained_action = {
  until : event;
  ty : sustained_action_type
}

(** An event graph, usually corresponding to a single looping thread. *)
type event_graph = {
  thread_id : int; (** unique identifier of the looping thread *)
  mutable events : event list;
  mutable wires : WireCollection.t;
  channels : Lang.channel_def list; (** all channel definitions.
          Note these do not include the channels passed from outside the process *)
  messages : MessageCollection.t; (** all messages referenceable from within the process,
            including those through channels passed from outside*)
  spawns : Lang.spawn_def list;
  regs: Lang.reg_def list;
  mutable last_event_id: int;
}

type proc_graph = {
  name : Lang.identifier;
  threads : event_graph list;
}

(** A collection of event graphs, corresponding to a compilation unit.
In addition to event graphs, it also includes the associated {{!typedefs}type definitions} and
{{!channel_classes}channel class definitions}.
*)
type event_graph_collection = {
  event_graphs : proc_graph list;
  typedefs : TypedefMap.t;
  channel_classes : Lang.channel_class_def list;
}

(** Construct a collection of event graphs from a compilation unit.
This also performs lifetime checking and throws {!LifetimeCheckError} upon failure.
*)
val build : Config.compile_config -> Lang.compilation_unit -> event_graph_collection

exception LifetimeCheckError of string

(** When a pair of endpoints are instantiated within a process, the process has access to
both of them although they merely mirror each other. This function {i canonicalises} a given endpoint
name so in such cases both endpoints are transformed into the same name. *)
val canonicalize_endpoint_name : Lang.identifier -> event_graph -> Lang.identifier
