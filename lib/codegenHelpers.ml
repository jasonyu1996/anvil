(** Search a list of {!EventGraph.proc_graph} for one with a specific name. *)
let lookup_proc (graphs : EventGraph.proc_graph list) (name: Lang.identifier) : EventGraph.proc_graph option =
  List.find_opt (fun (p : EventGraph.proc_graph) -> p.name = name) graphs
