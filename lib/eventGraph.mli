type wire = WireCollection.wire
type wire_collection = WireCollection.t
type action =
  | DebugPrint of string * wire list
  | DebugFinish
  | RegAssign of string * wire

type sustained_action_type =
  | Send of Lang.message_specifier * wire
  | Recv of Lang.message_specifier

type condition = {
  w : wire;
  neg : bool;
}

type plain_lifetime = {
  live: Lang.delay;
  dead: Lang.delay;
}

val string_of_plain_lifetime : plain_lifetime -> string

type event = {
  id : int;
  mutable actions: action list;
  mutable sustained_actions : sustained_action list;
  source: event_source;
}
and sustained_action = {
  until : event;
  ty : sustained_action_type;
}
and event_source = [
  | `Root
  | `Later of event * event
  | `Earlier of event * event
  | `Seq of event * Lang.atomic_delay
  | `Branch of condition * event
]

type event_graph = {
  thread_id : int;
  mutable events : event list;
  mutable wires : wire_collection;
  channels : Lang.channel_def list;
  messages : MessageCollection.t;
  spawns : Lang.spawn_def list;
  regs: Lang.reg_def list;
  mutable last_event_id: int;
}

type proc_graph = {
  id : Lang.identifier;
  threads : event_graph list;
}

type event_graph_collection = {
  event_graphs : proc_graph list;
  typedefs : TypedefMap.t;
  channel_classes : Lang.channel_class_def list;
}

val build : Lang.compilation_unit -> event_graph_collection

exception BorrowCheckError of string * plain_lifetime * plain_lifetime

val canonicalize_endpoint_name : Lang.identifier -> event_graph -> Lang.identifier
