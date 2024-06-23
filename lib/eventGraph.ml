open Lang
open Except

type wire = WireCollection.wire
type wire_collection = WireCollection.t


type cunit_info = {
  typedefs : TypedefMap.t
}

type action =
  | DebugPrint of string * wire list
  | DebugFinish

type event = {
  mutable actions: action list;
  source: event_source;
}
and event_source = [
  | `Cycles of int
  | `Later of event * event
  | `Earlier of event * event
  | `Seq of event * delay
]

type event_graph = {
  name : identifier;
  mutable events: event list;
  mutable wires: wire_collection;
  channels: channel_def list;
  args: endpoint_def list;
  spawns: spawn_def list;
}

module Typing = struct
  type lifetime = {
    live : event;
    _dead : delay;
  }

  type timed_data = {
    w : wire option;
    lt : lifetime;
  }

  type context = timed_data Utils.string_map
  type build_context = {
    typing_ctx : context;
    current : event;
  }

  let event_create g source =
    let n = {actions = []; source} in
    g.events <- n::g.events;
    n

  let lifetime_const current = {live = current; _dead = `Ever}
  let lifetime_intersect g (a : lifetime) (b : lifetime) =
    {
      live = event_create g (`Later (a.live, b.live));
      _dead = `Earlier (a._dead, b._dead);
    }

  let cycles_data g (n : int) (current : event) = {w = None; lt = {live = event_create g (`Seq (current, `Cycles n)); _dead = `Ever}}
  let const_data _g (w : wire option) (current : event) = {w; lt = lifetime_const current}
  let merged_data g (w : wire option) (current : event) (lts : lifetime list) =
    match lts with
    | [] -> const_data g w current
    | lt::lts' ->
      let lt' = List.fold_left (lifetime_intersect g) lt lts' in
      {w; lt = lt'}
  let derived_data (w : wire option) (lt : lifetime) = {w; lt}

  let context_add (ctx : context) (v : identifier) (d : timed_data) : context =
    Utils.StringMap.add v d ctx
  let context_empty : context = Utils.StringMap.empty
  let context_lookup (ctx : context) (v : identifier) = Utils.StringMap.find_opt v ctx


  module BuildContext = struct
    type t = build_context
    let create_empty g : t = {
      typing_ctx = context_empty;
      current = event_create g (`Cycles 1)
    }

    let add_binding (ctx : t) (v : identifier) (d : timed_data) : t =
      {ctx with typing_ctx = context_add ctx.typing_ctx v d}
    let _temporal_progress g (ctx : t) (by : delay) : t =
      {ctx with current = event_create g (`Seq (ctx.current, by))}
    let wait g (ctx : t) (other : event) : t =
      {ctx with current = event_create g (`Later (ctx.current, other))}
  end


end


type build_context = Typing.build_context
module BuildContext = Typing.BuildContext

let rec visit_expr (graph : event_graph) (ci : cunit_info)
                   (ctx : build_context) (e : expr) : Typing.timed_data =
  match e with
  | Literal lit ->
    let (wires', w) = WireCollection.add_literal lit graph.wires in
    graph.wires <- wires';
    Typing.const_data graph (Some w) ctx.current
  | Identifier ident -> Typing.context_lookup ctx.typing_ctx ident |> Option.get
  | Assign _ -> raise (UnimplementedError "Assign expression unimplemented!")
  | Binop (binop, e1, e2) ->
    let td1 = visit_expr graph ci ctx e1
    and td2 = visit_expr graph ci ctx e2 in
    let w1 = Option.get td1.w
    and w2 = Option.get td2.w in
    let (wires', w) = WireCollection.add_binary ci.typedefs binop w1 w2 graph.wires in
    graph.wires <- wires';
    Typing.merged_data graph (Some w) ctx.current [td1.lt; td2.lt]
  | Unop (unop, e') ->
    let td = visit_expr graph ci ctx e' in
    let w' = Option.get td.w in
    let (wires', w) = WireCollection.add_unary ci.typedefs unop w' graph.wires in
    graph.wires <- wires';
    Typing.derived_data (Some w) td.lt
  | Tuple _ -> raise (UnimplementedError "Tuple expression unimplemented!")
  | LetIn (idents, e1, e2) ->
    let td1 = visit_expr graph ci ctx e1 in
    (
      match idents, td1.w with
      | [], None | ["_"], _  -> visit_expr graph ci ctx e2
      | [ident], _ ->
        let ctx' = BuildContext.add_binding ctx ident td1 in
        visit_expr graph ci ctx' e2
      | _ -> raise (TypeError "Discarding expression results!")
    )
  | Wait (e1, e2) ->
    let td1 = visit_expr graph ci ctx e1 in
    let ctx' = BuildContext.wait graph ctx td1.lt.live in
    visit_expr graph ci ctx' e2
  | Cycle n -> Typing.cycles_data graph n ctx.current
  | IfExpr (e1, e2, e3) ->
    let td1 = visit_expr graph ci ctx e1 in
    let ctx' = BuildContext.wait graph ctx td1.lt.live in
    let td2 = visit_expr graph ci ctx' e2
    and td3 = visit_expr graph ci ctx' e3 in
    (* TODO: type checking *)
    let w1 = Option.get td1.w in
    let lt = let open Typing in {live = Typing.event_create graph (`Later (td2.lt.live, td3.lt.live));
      _dead = `Earlier (td1.lt._dead, `Earlier (td2.lt._dead, td3.lt._dead))} in
    (
      match td2.w, td3.w with
      | None, None -> {w = None; lt}
      | Some w2, Some w3 ->
        let (wires', w) = WireCollection.add_switch ci.typedefs [(w1, w2)] w3 graph.wires in
        graph.wires <- wires';
        {w = Some w; lt}
      | _ -> raise (TypeError "Invalid if expression!")
    )
  | _ -> raise (UnimplementedError "Unimplemented expression!")

let build_proc (ci : cunit_info) (proc : proc_def) =
  let graph = {
    name = proc.name;
    events = [];
    wires = WireCollection.empty;
    channels = proc.body.channels;
    args = proc.args;
    spawns = proc.body.spawns;
  } in
  let _ = visit_expr graph ci (BuildContext.create_empty graph) proc.body.prog in
  graph

type event_graph_collection = {
  event_graphs : event_graph list;
  typedefs : TypedefMap.t;
  channel_classes : channel_class_def list;
}

let build (cunit : compilation_unit) =
  let typedefs = TypedefMap.of_list cunit.type_defs in
  let ci = { typedefs } in
  let graphs = List.map (build_proc ci) cunit.procs in
  {
    event_graphs = graphs;
    typedefs;
    channel_classes = cunit.channel_classes;
  }

