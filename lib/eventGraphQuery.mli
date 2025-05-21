val lookup_channel_class : EventGraph.event_graph_collection -> Lang.identifier -> Lang.channel_class_def option
val lookup_endpoint : EventGraph.event_graph_collection -> EventGraph.proc_graph -> string -> Lang.endpoint_def option

(** This only includes non-foreign messages. The message direction is adjusted to reflect the endpoint direction. *)
val lookup_message : EventGraph.event_graph_collection -> EventGraph.proc_graph -> Lang.message_specifier -> Lang.message_def option
