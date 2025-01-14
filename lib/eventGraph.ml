open Lang

exception EventGraphError of string * Lang.code_span
exception LifetimeCheckError of string

type wire = WireCollection.wire
type wire_collection = WireCollection.t

type cunit_info = {
  typedefs : TypedefMap.t;
  channel_classes : channel_class_def list;
}

type atomic_delay = [
  | `Cycles of int
  | `Send of Lang.message_specifier
  | `Recv of Lang.message_specifier
  | `Sync of identifier
]


type global_timed_data =
{
  mutable w : wire option;
  glt : sig_lifetime;
}
type lifetime = {
  live : event;
  dead : event_pat;
}
and timed_data = {
  w : wire option;
  lt : lifetime;
  reg_borrows : (identifier * event) list;
}
and shared_var_info = {
  assigning_thread : int;
  value : global_timed_data;
  mutable assigned_at : event option;
}
and lvalue_info = {
  reg_name : string;
  range : timed_data MaybeConst.maybe_int_const * int;
  (* left closed right open *)
  dtype : data_type;
}
and action =
  | DebugPrint of string * timed_data list
  | DebugFinish
  | RegAssign of lvalue_info * timed_data
  | PutShared of string * shared_var_info * timed_data
and sustained_action_type =
  | Send of message_specifier * timed_data
  | Recv of message_specifier
and condition = {
  data : timed_data;
  neg : bool;
}
and event_pat = (event * Lang.delay_pat) list
and event = {
  id : int;
  graph: event_graph;
  mutable actions: action ast_node list;
  mutable sustained_actions : sustained_action ast_node list;
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
and event_graph = {
  (* name : identifier; *)
  thread_id : int;
  mutable events: event list;
  mutable wires: wire_collection;
  channels: channel_def list;
  messages : MessageCollection.t;
  spawns: spawn_def list;
  regs: reg_def list;
  (* the id of the last event *)
  (* the process loops from the start when this event is reached *)
  mutable last_event_id: int;
}
let lifetime_const current = {live = current; dead = [(current, `Eternal)]}
let lifetime_immediate current = {live = current; dead = [(current, `Cycles 1)]}

let print_graph (g: event_graph) =
  List.iter (fun ev ->
    match ev.source with
    | `Later (e1, e2) -> Printf.eprintf "> %d: later %d %d\n" ev.id e1.id e2.id
    | `Either (e1, e2) -> Printf.eprintf "> %d: either %d %d\n" ev.id e1.id e2.id
    | `Seq (ev', a) ->
      let c = match a with
      | `Cycles n -> Printf.sprintf "C %d" n
      | `Send _ -> "S"
      | `Recv _ -> "R"
      | `Sync s -> Printf.sprintf "S %s" s
      in
      Printf.eprintf "> %d: seq %d %s\n" ev.id ev'.id c
    | `Branch (c, ev') -> Printf.eprintf "> %d: branch %b %d\n" ev.id c.neg ev'.id
    | `Root -> Printf.eprintf "> %d: root\n" ev.id
  ) g.events

type proc_graph = {
    name: Lang.identifier;
    extern_module: string option;
    threads: event_graph list;
    shared_vars_info : (Lang.identifier, shared_var_info) Hashtbl.t;
    messages : MessageCollection.t;
    proc_body : proc_def_body_maybe_extern;
    spawns : (identifier * spawn_def) list;
}

type event_graph_collection = {
  event_graphs : proc_graph list;
  typedefs : TypedefMap.t;
  channel_classes : channel_class_def list;
  external_event_graphs : proc_graph list;
}
