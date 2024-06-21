type event = Lang.delay
type wire = WireCollection.wire
type wire_collection = WireCollection.t
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
