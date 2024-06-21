let lookup_channel_class (channel_classes : Lang.channel_class_def list) (name : Lang.identifier) : Lang.channel_class_def option =
  List.find_opt (fun (cc : Lang.channel_class_def) -> cc.name = name) channel_classes

let lookup_proc (graphs : EventGraph.event_graph list) (name: Lang.identifier) : EventGraph.event_graph option =
  List.find_opt (fun (p : EventGraph.event_graph) -> p.name = name) graphs

let lookup_endpoint (graph : EventGraph.event_graph)
                    (endpoints : Lang.endpoint_def list) (endpoint_name : Lang.identifier) : Lang.endpoint_def option =
  let match_fun = fun (p : Lang.endpoint_def) -> p.name = endpoint_name in
  let local_endpoint_opt = List.find_opt match_fun endpoints in
  if Option.is_none local_endpoint_opt then
    List.find_opt match_fun graph.args
  else
    local_endpoint_opt
