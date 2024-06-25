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
  | RegAssign of string * wire

type condition = {
  w : wire;
  neg : bool;
}

type event = {
  id : int;
  mutable actions: action list;
  source: event_source;
}
and event_source = [
  | `Root
  | `Later of event * event
  | `Earlier of event * event
  | `Seq of event * atomic_delay
  | `Branch of condition * event
]

type event_graph = {
  name : identifier;
  mutable events: event list;
  mutable wires: wire_collection;
  channels: channel_def list;
  args: endpoint_def list;
  spawns: spawn_def list;
  regs: reg_def list;
  (* the id of the last event *)
  (* the process loops from the start when this event is reached *)
  mutable last_event_id: int;
}

let string_of_delay (d : delay) : string =
  let buf = Buffer.create 16 in
  let rec append_string_of_delay = function
    | `Ever -> Buffer.add_string buf "oo"
    | `Cycles n -> Buffer.add_string buf @@ string_of_int n
    | `Later (d1, d2) ->
      Buffer.add_char buf '<';
      append_string_of_delay d1;
      Buffer.add_string buf ", ";
      append_string_of_delay d2;
      Buffer.add_char buf '>'
    | `Earlier (d1, d2) ->
      Buffer.add_char buf '[';
      append_string_of_delay d1;
      Buffer.add_string buf ", ";
      append_string_of_delay d2;
      Buffer.add_char buf ']'
    | `Seq (d', dd) ->
      append_string_of_delay d';
      Buffer.add_string buf " => ";
      append_string_of_delay (dd :> delay)
  in
  append_string_of_delay d;
  Buffer.contents buf

type plain_lifetime = {
  live: delay;
  dead: delay;
}

let string_of_plain_lifetime (lt : plain_lifetime) : string =
  Printf.sprintf "{%s, %s}" (string_of_delay lt.live) (string_of_delay lt.dead)

module Typing = struct
  type lifetime = {
    live : event;
    dead : delay;
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
    let n = {actions = []; source; id = List.length g.events} in
    g.events <- n::g.events;
    n

  let rec delay_of_event (e : event) : delay =
    match e.source with
    | `Root -> `Cycles 0
    | `Later (e1, e2) -> `Later (delay_of_event e1, delay_of_event e2)
    | `Earlier (e1, e2) -> `Earlier (delay_of_event e1, delay_of_event e2)
    | `Seq (e', d) -> `Seq (delay_of_event e', d)
    | `Branch (_, e') -> delay_of_event e'

  let lifetime_plainify (lt : lifetime) : plain_lifetime =
    {
      live = delay_of_event lt.live;
      dead = lt.dead;
    }

  let lifetime_const current = {live = current; dead = `Ever}
  let lifetime_immediate current = {live = current; dead = `Seq (delay_of_event current, `Cycles 1)}
  let lifetime_intersect g (a : lifetime) (b : lifetime) =
    {
      live = event_create g (`Later (a.live, b.live));
      dead = `Earlier (a.dead, b.dead);
    }

  let cycles_data g (n : int) (current : event) = {w = None; lt = {live = event_create g (`Seq (current, `Cycles n)); dead = `Ever}}
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
  (* checks if lt lives at least as long as required *)

  let rec delay_leq_check (a : delay) (b : delay) : bool =
    match a, b with
    | _, `Ever -> true
    | _, `Later (b1, b2) ->
      (delay_leq_check a b1) || (delay_leq_check a b2)
    | _, `Earlier (b1, b2) ->
      (delay_leq_check a b1) && (delay_leq_check a b2)
    | `Later (a1, a2), _ ->
      (delay_leq_check a1 b) && (delay_leq_check a2 b)
    | `Earlier (a1, a2), _ ->
      (delay_leq_check a1 b) || (delay_leq_check a2 b)
    (* only both seq or atomic *)
    | `Cycles na, `Cycles nb -> na <= nb
    | `Ever, `Cycles _ -> false
    | `Ever, `Seq (b', _) -> delay_leq_check `Ever b'
    | `Cycles na, `Seq (b', `Cycles nb) ->
      na <= nb || (delay_leq_check (`Cycles (na - nb)) b')
    | `Seq (a', `Cycles na), `Seq (b', `Cycles nb) ->
      let m = min na nb in
      if na = m then
        delay_leq_check a' (`Seq (b', `Cycles (nb - m)))
      else
        delay_leq_check (`Seq (a', `Cycles (na - m))) b'

    | `Seq (a', `Cycles na), `Cycles nb ->
      na <= nb && (delay_leq_check a' (`Cycles (nb - na)))

  let lifetime_check (lt : lifetime) (req : lifetime) : bool =
    (* requires lt.live <= req.live && lt.dead >= req.dead *)
    let lt_live_delay = delay_of_event lt.live
    and req_live_delay = delay_of_event req.live in
    (delay_leq_check lt_live_delay req_live_delay) &&
      (delay_leq_check req.dead lt.dead)

  module BuildContext = struct
    type t = build_context
    let create_empty g : t = {
      typing_ctx = context_empty;
      current = event_create g `Root
    }

    let add_binding (ctx : t) (v : identifier) (d : timed_data) : t =
      {ctx with typing_ctx = context_add ctx.typing_ctx v d}
    let wait g (ctx : t) (other : event) : t =
      {ctx with current = event_create g (`Later (ctx.current, other))}
    let branch g (ctx : t) (w : wire) : t * t =
      (
        {ctx with current = event_create g (`Branch ({w; neg = false}, ctx.current))},
        {ctx with current = event_create g (`Branch ({w; neg = true}, ctx.current))}
      )
  end


end

exception BorrowCheckError of string * plain_lifetime * plain_lifetime

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
  | Assign (lval, e') ->
    let td = visit_expr graph ci ctx e' in
    let w' = Option.get td.w in
    if Typing.lifetime_check td.lt (Typing.lifetime_immediate ctx.current) |> not then
      raise (BorrowCheckError ("Value does not live long enough in assignment!",
        Typing.lifetime_plainify td.lt,
        Typing.lifetime_immediate ctx.current |> Typing.lifetime_plainify))
    else ();
    let reg_ident = (
      let open Lang in
      match lval with
      | Reg ident ->
        ident
      | _ -> raise (UnimplementedError "Lval with indexing/indirection unimplemented!")
    ) in
    ctx.current.actions <- (RegAssign (reg_ident, w'))::ctx.current.actions;
    Typing.cycles_data graph 1 ctx.current
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
  | Tuple [] -> Typing.const_data graph None ctx.current
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
    (* TODO: type checking *)
    let w1 = Option.get td1.w in
    let (ctx_true, ctx_false) =
      let ctx' = BuildContext.wait graph ctx td1.lt.live in
      if Typing.lifetime_check td1.lt (Typing.lifetime_immediate ctx.current) |> not then
        raise (BorrowCheckError ("If condition does not live long enough!",
          Typing.lifetime_plainify td1.lt,
          Typing.lifetime_immediate ctx.current |> Typing.lifetime_plainify))
      else ();
      BuildContext.branch graph ctx' w1
    in
    let td2 = visit_expr graph ci ctx_true e2
    and td3 = visit_expr graph ci ctx_false e3 in
    let lt = let open Typing in {live = Typing.event_create graph (`Earlier (td2.lt.live, td3.lt.live));
      dead = `Earlier (td1.lt.dead, `Earlier (td2.lt.dead, td3.lt.dead))} in
    (
      match td2.w, td3.w with
      | None, None -> {w = None; lt}
      | Some w2, Some w3 ->
        let (wires', w) = WireCollection.add_switch ci.typedefs [(w1, w2)] w3 graph.wires in
        graph.wires <- wires';
        {w = Some w; lt}
      | _ -> raise (TypeError "Invalid if expression!")
    )
  | Concat es ->
    let tds = List.map (visit_expr graph ci ctx) es in
    let ws = List.map (fun (td : Typing.timed_data) -> Option.get td.w) tds in
    let (wires', w) = WireCollection.add_concat ci.typedefs ws graph.wires in
    graph.wires <- wires';
    Typing.merged_data graph (Some w) ctx.current (List.map (fun (td : Typing.timed_data) -> td.lt) tds)
  | Read reg_ident ->
    let r = List.find (fun (r : Lang.reg_def) -> r.name = reg_ident) graph.regs in
    let (wires', w) = WireCollection.add_reg_read ci.typedefs r graph.wires in
    graph.wires <- wires';
    {w = Some w; lt = Typing.lifetime_const ctx.current}
  | Debug op ->
    (
      match op with
      | DebugPrint (s, e_list) ->
        let timed_ws = List.map (visit_expr graph ci ctx) e_list in
        let ws = List.map (fun (x : Typing.timed_data) -> Option.get x.w)
          timed_ws
        in
        ctx.current.actions <- (DebugPrint (s, ws))::ctx.current.actions;
        (* TODO: incorrect lifetime *)
        {w = None; lt = Typing.lifetime_const ctx.current}
      | DebugFinish ->
        ctx.current.actions <- DebugFinish::ctx.current.actions;
        {w = None; lt = Typing.lifetime_const ctx.current}
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
    regs = proc.body.regs;
    last_event_id = 0;
  } in
  let td = visit_expr graph ci (BuildContext.create_empty graph) proc.body.prog in
  graph.last_event_id <- td.lt.live.id;
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

