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


type atomic_delay = [
  | `Cycles of int
  | `Send of Lang.message_specifier
  | `Recv of Lang.message_specifier
]

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
  | `Seq of event * atomic_delay
  | `Branch of condition * event
  | `Either of event * event
]

type event_pat = (event * Lang.delay_pat) list

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
  name : Lang.identifier;
  threads : event_graph list;
}

type event_graph_collection = {
  event_graphs : proc_graph list;
  typedefs : TypedefMap.t;
  channel_classes : Lang.channel_class_def list;
}

val build : Lang.compilation_unit -> event_graph_collection

exception BorrowCheckError of string

val canonicalize_endpoint_name : Lang.identifier -> event_graph -> Lang.identifier
