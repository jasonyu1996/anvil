type wire = WireCollection.wire
type wire_collection = WireCollection.t
type action =
  | DebugPrint of string * wire list
  | DebugFinish

type event = {
  id : int;
  mutable actions: action list;
  source: event_source;
}
and event_source = [
  | `Root
  | `Later of event * event
  | `Earlier of event * event
  | `Seq of event * Lang.atomic_delay
]
type event_graph = {
  name : Lang.identifier;
  mutable events : event list;
  mutable wires : wire_collection;
  channels : Lang.channel_def list;
  args : Lang.endpoint_def list;
  spawns : Lang.spawn_def list;
}

type event_graph_collection = {
  event_graphs : event_graph list;
  typedefs : TypedefMap.t;
  channel_classes : Lang.channel_class_def list;
}

val build : Lang.compilation_unit -> event_graph_collection
