open Lang
open Except

type event = delay
type wire = WireCollection.wire
type wire_collection = WireCollection.t

type cunit_info = {
  typedefs : TypedefMap.t
}

module Typing = struct
  type lifetime = {
    live : event;
    _dead : event;
  }

  type timed_data = {
    w : wire option;
    lt : lifetime;
  }

  let lifetime_const current = {live = current; _dead = `Ever}
  let lifetime_intersect (a : lifetime) (b : lifetime) =
    {
      live = `Later(a.live, b.live);
      _dead = `Earlier(a.live, b.live);
    }

  let cycles_data (n : int) (current : event) = {w = None; lt = {live = `Seq(current, `Cycles n); _dead = `Ever}}
  let const_data (w : wire option) (current : event) = {w; lt = lifetime_const current}
  let merged_data (w : wire option) (current : event) (lts : lifetime list) =
    match lts with
    | [] -> const_data w current
    | lt::lts' ->
      let lt' = List.fold_left lifetime_intersect lt lts' in
      {w; lt = lt'}
  let derived_data (w : wire option) (lt : lifetime) = {w; lt}

  type context = timed_data Utils.string_map
  let context_add (ctx : context) (v : identifier) (d : timed_data) : context =
    Utils.StringMap.add v d ctx
  let context_empty : context = Utils.StringMap.empty
  let context_lookup (ctx : context) (v : identifier) = Utils.StringMap.find_opt v ctx
end

type event_graph = {
  name : identifier;
  mutable events: event list;
  mutable wires: wire_collection;
  channels: channel_def list;
  args: endpoint_def list;
  spawns: spawn_def list;
}

module BuildContext = struct
  type t = {
    typing_ctx : Typing.context;
    current : event;
  }

  let empty : t = {
    typing_ctx = Typing.context_empty;
    current = `Cycles 1
  }

  let add_binding (ctx : t) (v : identifier) (d : Typing.timed_data) : t =
    {ctx with typing_ctx = Typing.context_add ctx.typing_ctx v d}
  let _temporal_progress (ctx : t) (by : delay) : t =
    {ctx with current = `Seq (ctx.current, by)}
  let wait (ctx : t) (other : event) : t =
    {ctx with current = `Later (ctx.current, other)}
end

type build_context = BuildContext.t

let rec visit_expr (graph : event_graph) (ci : cunit_info)
                   (ctx : build_context) (e : expr) : Typing.timed_data =
  match e with
  | Literal lit ->
    let (wires', w) = WireCollection.add_literal lit graph.wires in
    graph.wires <- wires';
    Typing.const_data (Some w) ctx.current
  | Identifier ident -> Typing.context_lookup ctx.typing_ctx ident |> Option.get
  | Assign _ -> raise (UnimplementedError "Assign expression unimplemented!")
  | Binop (binop, e1, e2) ->
    let td1 = visit_expr graph ci ctx e1
    and td2 = visit_expr graph ci ctx e2 in
    let w1 = Option.get td1.w
    and w2 = Option.get td2.w in
    let (wires', w) = WireCollection.add_binary ci.typedefs binop w1 w2 graph.wires in
    graph.wires <- wires';
    Typing.merged_data (Some w) ctx.current [td1.lt; td2.lt]
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
    let ctx' = BuildContext.wait ctx td1.lt.live in
    visit_expr graph ci ctx' e2
  | Cycle n -> Typing.cycles_data n ctx.current
  | IfExpr (e1, e2, e3) ->
    let td1 = visit_expr graph ci ctx e1 in
    let ctx' = BuildContext.wait ctx td1.lt.live in
    let td2 = visit_expr graph ci ctx' e2
    and td3 = visit_expr graph ci ctx' e3 in
    (* TODO: type checking *)
    let w1 = Option.get td1.w in
    let lt = let open Typing in {live = `Later (td2.lt.live, td3.lt.live);
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
  let _ = visit_expr graph ci BuildContext.empty proc.body.prog in
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

