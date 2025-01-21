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

(** Information about a compilation unit. *)
type cunit_info = {
  typedefs : TypedefMap.t;
  channel_classes : Lang.channel_class_def list;
  enum_mappings : (string * (string * int) list) list;
  func_defs : Lang.func_def list;
  macro_defs : Lang.macro_def list
}

type wire = WireCollection.wire
type wire_collection = WireCollection.t

(** An event that may lead to delay in time. *)
type atomic_delay = [
  | `Cycles of int (** elapse of a specific number of cycles *)
  | `Send of Lang.message_specifier (** sending of a message *)
  | `Recv of Lang.message_specifier (** receiving of a message *)
  | `Sync of Lang.identifier (** synchronising on a local shared value *)
]

type global_timed_data =
{
  mutable w : wire option;
  glt : Lang.sig_lifetime;
  gdtype : Lang.data_type;
}

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
  dtype : Lang.data_type;
}
and shared_var_info = {
  assigning_thread : int;
  value : global_timed_data;
  mutable assigned_at : event option;
}

(** Lvalue information after resolving indirection and indexing. *)
and lvalue_info = {
  reg_name : string;
  range : timed_data MaybeConst.maybe_int_const * int; (** range in the register, second component is size *)
  lval_dtype : Lang.data_type;
}

(** An action that is performed instantly when an event is reached. *)
and action =
  | DebugPrint of string * timed_data list (** debug print ([dprint]) *)
  | DebugFinish (** [dfinish] *)
  | RegAssign of lvalue_info * timed_data (** register assignment (technically this is not performed instantly) *)
  | PutShared of string * shared_var_info * timed_data

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
  graph : event_graph;
  mutable actions: action Lang.ast_node list; (** instant actions that take place when this event is reached *)
  mutable sustained_actions : sustained_action Lang.ast_node list;
  (** actions that may take multiple cycles and start when this event is reached*)
  mutable source: event_source; (** under what circumstances is this event reached.
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

(** An event graph, usually corresponding to a single looping thread.
All parameters in an event graph have been concretised. *)
and event_graph = {
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
    name: Lang.identifier;
    extern_module: string option;
    threads: event_graph list;
    shared_vars_info : (Lang.identifier, shared_var_info) Hashtbl.t;
    messages : MessageCollection.t;
    proc_body : Lang.proc_def_body_maybe_extern;
    spawns : (Lang.identifier * Lang.spawn_def) list;
}

(** A collection of event graphs, corresponding to a compilation unit.
In addition to event graphs, it also includes the associated {{!typedefs}type definitions} and
{{!channel_classes}channel class definitions}.
*)
type event_graph_collection = {
  event_graphs : proc_graph list;
  typedefs : TypedefMap.t;
  macro_defs : Lang.macro_def list;
  channel_classes : Lang.channel_class_def list;
  external_event_graphs : proc_graph list;
  enum_mappings : (string * (string * int) list) list;
}

(** Print the structure of the graph to standard error stream. *)
val print_graph : event_graph -> unit

val lifetime_const : event -> lifetime
val lifetime_immediate : event -> lifetime

(** Exception that can be throw during event graph generation *)
exception EventGraphError of string * Lang.code_span

exception LifetimeCheckError of string
