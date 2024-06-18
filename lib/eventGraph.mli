type event = Lang.delay
type wire = WireCollection.wire
type wire_collection = WireCollection.t
type event_graph = {
  mutable events : event list;
  mutable wires : wire_collection;
  typedefs : TypedefMap.t;
}

val build : Lang.compilation_unit -> event_graph list
