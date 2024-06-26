let lookup_proc (graphs : EventGraph.event_graph list) (name: Lang.identifier) : EventGraph.event_graph option =
  List.find_opt (fun (p : EventGraph.event_graph) -> p.name = name) graphs
