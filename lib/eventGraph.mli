type wire = WireCollection.wire
type wire_collection = WireCollection.t

type atomic_delay = [
  | `Cycles of int
  | `Send of Lang.message_specifier
  | `Recv of Lang.message_specifier
]

type lifetime = {
  live : event;
  dead : event_pat;
}
and timed_data = {
  w : wire option;
  lt : lifetime;
}
and action =
  | DebugPrint of string * timed_data list
  | DebugFinish
  | RegAssign of string * timed_data
and sustained_action_type =
  | Send of Lang.message_specifier * timed_data
  | Recv of Lang.message_specifier
and condition = {
  data : timed_data;
  neg : bool;
}
and event_pat = (event * Lang.delay_pat) list
and event = {
  id : int;
  mutable actions: action list;
  mutable sustained_actions : sustained_action list;
  source: event_source;
  (* for lifetime checking *)
  mutable control_regs: (int * int) Utils.string_map;
  mutable control_endps: (int * int) Utils.string_map;
  mutable current_regs : (int * int) Utils.string_map;
  mutable current_endps : (int * int) Utils.string_map;
  mutable outs : event list;
}
and event_source = [
  | `Root
  | `Later of event * event
  | `Seq of event * atomic_delay
  | `Branch of condition * event
  | `Either of event * event (* joining if-then-else branches *)
]
(* sustained actions are in effect when current is reached and until
  is not reached *)
and sustained_action = {
  until : event;
  ty : sustained_action_type
}

type event_graph = {
  thread_id : int;
  mutable events : event list;
  mutable wires : WireCollection.t;
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

val build : Config.compile_config -> Lang.compilation_unit -> event_graph_collection

exception LifetimeCheckError of string

val canonicalize_endpoint_name : Lang.identifier -> event_graph -> Lang.identifier
