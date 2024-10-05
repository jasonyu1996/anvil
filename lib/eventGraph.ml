open Lang
open Except

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
]

type lifetime = {
  live : event;
  dead : event_pat;
}
and timed_data = {
  w : wire option;
  lt : lifetime;
}
and action =
  | DebugPrint of string * timed_data list
  | DebugFinish
  | RegAssign of string * timed_data
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
  mutable actions: action list;
  mutable sustained_actions : sustained_action list;
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

type event_graph = {
  (* name : identifier; *)
  thread_id : int;
  mutable events: event list;
  mutable wires: wire_collection;
  channels: channel_def list;
  messages : MessageCollection.t;
  spawns: spawn_def list;
  regs: reg_def list;
  (* loops: expr list; *)
  (* the id of the last event *)
  (* the process loops from the start when this event is reached *)
  mutable last_event_id: int;
}

let print_graph (g: event_graph) =
  List.iter (fun ev ->
    match ev.source with
    | `Later (e1, e2) -> Printf.eprintf "> %d: later %d %d\n" ev.id e1.id e2.id
    | `Either (e1, e2) -> Printf.eprintf "> %d: either %d %d\n" ev.id e1.id e2.id
    | `Seq (ev', _) -> Printf.eprintf "> %d: seq %d\n" ev.id ev'.id
    | `Branch (c, ev') -> Printf.eprintf "> %d: branch %b %d\n" ev.id c.neg ev'.id
    | `Root -> Printf.eprintf "> %d: root\n" ev.id
  ) g.events

type proc_graph = {
  name: identifier;
  threads: event_graph list;
}

exception LifetimeCheckError of string
module Typing = struct

  type context = timed_data Utils.string_map
  type build_context = {
    typing_ctx : context;
    current : event;
  }

  let event_traverse (ev : event) visitor : event list =
    let visited = Seq.return ev.id |> Utils.IntSet.of_seq |> ref in
    let q = Seq.return ev |> Queue.of_seq in
    let res = ref [] in
    let add_to_queue ev' =
      if Utils.IntSet.mem ev'.id !visited |> not then (
        visited := Utils.IntSet.add ev'.id !visited;
        Queue.add ev' q
      )
      else ()
    in
    while Queue.is_empty q |> not do
      let cur = Queue.pop q in
      res := cur::!res;
      visitor add_to_queue cur
    done;
    !res

  (** Compute predecessors of an event in the event graph.
    Result in topological order
  *)
  let event_predecessors (ev : event) : event list =
    let visitor add_to_queue cur =
      match cur.source with
      | `Later (e1, e2)
      | `Either (e1, e2) ->
        add_to_queue e1;
        add_to_queue e2
      | `Branch (_, ev')
      | `Seq (ev', _) ->
        add_to_queue ev'
      | _ -> ()
    in
    event_traverse ev visitor

  (* Result in topological order *)
  let event_successors (ev : event) : event list =
    let visitor add_to_queue cur =
      List.iter add_to_queue cur.outs
    in
    event_traverse ev visitor |> List.rev

  let event_is_successor (ev : event) (ev' : event) =
    event_successors ev |> List.exists (fun x -> x.id = ev'.id)

  let event_is_predecessor (ev : event) (ev' : event) =
    event_predecessors ev |> List.exists (fun x -> x.id = ev'.id)


  let event_create g source =
    let event_create_inner () =
      let n = {actions = []; sustained_actions = []; source; id = List.length g.events;
        control_regs = Utils.StringMap.empty; control_endps = Utils.StringMap.empty;
        current_regs = Utils.StringMap.empty; current_endps = Utils.StringMap.empty;
        outs = []} in
      g.events <- n::g.events;
      (
        match source with
        | `Later (e1, e2)
        | `Either (e1, e2) ->
          e1.outs <- n::e1.outs;
          e2.outs <- n::e2.outs
        | `Branch (_, ev') ->
          ev'.outs <- n::ev'.outs
        | _ -> ()
      );
      n
    in
    (
      match source with
      | `Later (e1, e2) -> (
        if event_is_successor e1 e2 then
          e2
        else if event_is_predecessor e1 e2 then
          e1
        else
          event_create_inner ()
      )
      | _ -> event_create_inner ()
    )

  let lifetime_const current = {live = current; dead = [(current, `Eternal)]}
  let lifetime_immediate current = {live = current; dead = [(current, `Cycles 1)]}
  let lifetime_intersect g (a : lifetime) (b : lifetime) =
    {
      live = event_create g (`Later (a.live, b.live));
      dead = a.dead @ b.dead;
    }

  let cycles_data g (n : int) (current : event) =
    let live_event = event_create g (`Seq (current, `Cycles n)) in
    {w = None; lt = {live = live_event; dead = [(live_event, `Eternal)]}}
  let sync_data g (current : event) (td: timed_data) =
    let ev = event_create g (`Later (current, td.lt.live)) in
    {td with lt = {td.lt with live = ev}}

  let const_data _g (w : wire option) (current : event) = {w; lt = lifetime_const current}
  let merged_data g (w : wire option) (current : event) (lts : lifetime list) =
    match lts with
    | [] -> const_data g w current
    | lt::lts' ->
      let lt' = List.fold_left (lifetime_intersect g) lt lts' in
      {w; lt = lt'}
  let derived_data (w : wire option) (lt : lifetime) = {w; lt}
  let send_msg_data g (msg : message_specifier) (current : event) =
    let live_event = event_create g (`Seq (current, `Send msg)) in
    {w = None; lt = {live = live_event; dead = [(live_event, `Eternal)]}}
  let recv_msg_data g (w : wire option) (msg : message_specifier) (_msg_def : message_def) (current : event) =
    let event_received = event_create g (`Seq (current, `Recv msg)) in
    (* FIXME: take into consideration the lifetime signature *)
    {w; lt = {live = event_received; dead = [(event_received, `Eternal)]}}

  let context_add (ctx : context) (v : identifier) (d : timed_data) : context =
    Utils.StringMap.add v d ctx
  let context_empty : context = Utils.StringMap.empty
  let context_lookup (ctx : context) (v : identifier) = Utils.StringMap.find_opt v ctx
  (* checks if lt lives at least as long as required *)

  let in_control_set (cur : (int * int) Utils.string_map) (cnt : (int * int) Utils.string_map) (s : identifier) =
    let (pre, post) = Utils.StringMap.find s cnt in
    if Utils.StringMap.find s cur |> fst = 0 then
      pre + 1 = post
    else
      pre + 2 = post

  let in_control_set_reg (ev : event) = in_control_set ev.current_regs ev.control_regs
  let in_control_set_endps (ev : event) = in_control_set ev.current_endps ev.control_endps


  let print_control_set (g : event_graph) =
    List.iter (fun ev ->
      Printf.eprintf "=== Event %d ===\n" ev.id;
      Utils.StringMap.iter (fun reg_ident r ->
        Printf.eprintf "Reg %s: %d, %d\n" reg_ident (fst r) (snd r)
      ) ev.control_regs;
      Utils.StringMap.iter (fun endp r ->
        Printf.eprintf "Endpoint %s: %d, %d\n" endp (fst r) (snd r)
      ) ev.control_endps
    ) g.events

  (** Generate the control set for each event in the event graph. The control set of event
  includes actions that are synchronised at the event. *)
  let gen_control_set (g : event_graph) =
    let events_rev = List.rev g.events in
    let all_endpoints = g.messages.endpoints @ g.messages.args in
    let reg_assign_cnt_empty = List.map (fun (r : reg_def) -> (r.name, 0)) g.regs
      |> List.to_seq |> Utils.StringMap.of_seq
    and endp_use_cnt_empty =
        List.map (fun (e : endpoint_def) -> (e.name, 0)) all_endpoints
          |> List.to_seq |> Utils.StringMap.of_seq
    in
    let reg_assign_cnt = ref reg_assign_cnt_empty
    and endp_use_cnt = ref endp_use_cnt_empty in
    let control_regs_init = Seq.repeat (0, Int.max_int) |> Seq.take (List.length g.regs)
          |> Seq.zip (List.map (fun (r : reg_def) -> r.name) g.regs |> List.to_seq) |> Utils.StringMap.of_seq
    and control_endps_init = Seq.repeat (0, Int.max_int) |> Seq.take (List.length all_endpoints)
          |> Seq.zip (List.map (fun (ep : endpoint_def) -> ep.name) all_endpoints |> List.to_seq)
          |> Utils.StringMap.of_seq in
    List.iter
      (fun (ev : event) ->
        ev.control_regs <- control_regs_init;
        ev.current_regs <- control_regs_init;
        ev.control_endps <- control_endps_init;
        ev.current_endps <- control_endps_init;
      )
      events_rev;

    let opt_incr (a_opt : int option) =
      let a = Option.get a_opt in
      Some (a + 1)
    in
    let opt_update_forward (v : int) (a_opt : (int * int) option) =
      let (a, b) = Option.get a_opt in
      Some (max v a, b)
    and opt_update_backward (v : int) (a_opt : (int * int) option) =
      let (a, b) = Option.get a_opt in
      Some (a, min v b)
    in
    let last_ev : (event option) ref = ref None in
    List.iter
      (fun (ev : event) ->
        List.iter
          (fun (a : action) ->
            match a with
            | RegAssign (reg_ident, _) -> (
              reg_assign_cnt := Utils.StringMap.update reg_ident opt_incr !reg_assign_cnt;
              let v = Utils.StringMap.find reg_ident !reg_assign_cnt in
              ev.current_regs <- Utils.StringMap.update reg_ident (opt_update_forward v) ev.current_regs;
              ev.current_regs <- Utils.StringMap.update reg_ident (opt_update_backward v) ev.current_regs
            )
            | _ -> ()
          )
          ev.actions;
        List.iter
          (fun (a : sustained_action) ->
            match a.ty with
            | Send (ms, _) | Recv ms -> (
              endp_use_cnt := Utils.StringMap.update ms.endpoint opt_incr !endp_use_cnt;
              let v = Utils.StringMap.find ms.endpoint !endp_use_cnt in
              ev.current_endps <- Utils.StringMap.update ms.endpoint (opt_update_forward v) ev.current_endps;
              ev.current_endps <- Utils.StringMap.update ms.endpoint (opt_update_backward v) ev.current_endps
            )
          )
          ev.sustained_actions;
        (* forward pass *)
        let forward_from (ev' : event) =
          ev.control_regs <- Utils.StringMap.merge (fun _ a b -> opt_update_forward (Option.get a |> fst) b) ev'.control_regs ev.control_regs;
          ev.control_endps <- Utils.StringMap.merge (fun _ a b -> opt_update_forward (Option.get a |> fst) b) ev'.control_endps ev.control_endps;
          ev.control_regs <- Utils.StringMap.merge (fun _ a b -> opt_update_forward (Option.get a |> fst) b) ev'.current_regs ev.control_regs;
          ev.control_endps <- Utils.StringMap.merge (fun _ a b -> opt_update_forward (Option.get a |> fst) b) ev'.current_endps ev.control_endps
        in
        (
          match ev.source with
          | `Root -> ()
          | `Later (e1, e2) | `Either (e1, e2) -> (* FIXME: handle 'either' correctly *)
            (* Printf.eprintf "FR %d, %d -> %d\n" e1.id e2.id ev.id; *)
            forward_from e1;
            forward_from e2
          | `Seq (ev', _)  ->
            (* Printf.eprintf "FR %d -> %d\n" ev'.id ev.id; *)
            forward_from ev'
          | `Branch (c, ev') ->
            if c.neg then
              forward_from (Option.get !last_ev)
            else
              forward_from ev'
        );
        last_ev := Some ev
      )
      events_rev;
    (* backward pass *)
    List.iteri
      (fun idx (ev : event) ->
        let backward_to (ev' : event) =
          ev'.control_regs <- Utils.StringMap.merge (fun _ a b -> opt_update_backward (Option.get a |> snd) b) ev.control_regs ev'.control_regs;
          ev'.control_endps <- Utils.StringMap.merge (fun _ a b -> opt_update_backward (Option.get a |> snd) b) ev.control_endps ev'.control_endps;
          ev'.control_regs <- Utils.StringMap.merge (fun _ a b -> opt_update_backward (Option.get a |> snd) b) ev.current_regs ev'.control_regs;
          ev'.control_endps <- Utils.StringMap.merge (fun _ a b -> opt_update_backward (Option.get a |> snd) b) ev.current_endps ev'.control_endps
        in
        (
          ev.control_regs <- Utils.StringMap.merge (fun _ a b -> opt_update_backward (let v = Option.get a in v + 1) b) !reg_assign_cnt ev.control_regs;
          ev.control_endps <- Utils.StringMap.merge (fun _ a b -> opt_update_backward (let v = Option.get a in v + 1) b) !endp_use_cnt ev.control_endps;
          match ev.source with
          | `Root -> ()
        | `Later (e1, e2) ->
          backward_to e1;
          backward_to e2
        | `Seq (ev', _) ->
          backward_to ev'
        | `Either (_e1, e2) ->
          backward_to e2
        | `Branch (c, ev') ->
          if c.neg then
            backward_to (List.nth g.events (idx + 1))
          else
            backward_to ev'
        )
      ) g.events;
    (* print_control_set g;
    print_graph g; *)
    (* check for violations of linearity *)
    List.iter
      (fun (ev : event) ->
        List.iter (fun (a : action) ->
          match a with
          | RegAssign (reg_ident, _) -> (
            if in_control_set_reg ev reg_ident |> not then
              raise (LifetimeCheckError "Non-linearizable register assignment!")
            else ()
          )
          | _ -> ()
        ) ev.actions;
        List.iter (fun (sa : sustained_action) ->
          match sa.ty with
          | Send (ms, _) | Recv ms -> (
            if in_control_set_endps ev ms.endpoint |> not then
              raise (LifetimeCheckError "Non-linearizable endpoint use!")
            else ()
          )
        ) ev.sustained_actions
      )
      g.events

  type _event_pat_match_result =
  {
    bef: bool;
    aft: bool;
    at: bool;
  }

  (** evs must be in topological order*)
  let find_controller (endpts : identifier list) =
    List.find_opt
      (fun ev ->
        List.for_all (fun endpt -> in_control_set_reg ev endpt) endpts
      )

  let find_first_msg_after (ev : event) (msg: Lang.message_specifier) =
    event_successors ev |> List.tl |> List.find_opt
      (fun ev' ->
        List.exists (fun sa ->
          match sa.ty with
          | Send (msg', _) | Recv msg' -> msg' = msg
        ) ev'.sustained_actions
      )

  let event_pat_bounds (ev_pat : event_pat) : (event * event) list =
    List.filter_map (fun (ev, dpat) ->
      match dpat with
      | `Eternal -> None
      | `Cycles _n -> Some (ev, ev) (* FIXME: incorrect *)
      | `Message msg -> (
        let preds = event_predecessors ev
        and succs = event_successors ev in
        let pred_controller = find_controller [msg.endpoint] (List.rev preds) |> Option.get in
        let succ_controller = find_controller [msg.endpoint] succs |> Option.get in
        let le_match = find_first_msg_after pred_controller msg |> Option.get
        and ri_match = find_first_msg_after succ_controller msg |> Option.get in
        Some (le_match, ri_match)
      )
    ) ev_pat

  (** When does the event take place relative to the event pattern? *)
  let event_pat_matches (ev : event) (ev_pat : event_pat) =
    let res = ref {bef = false; aft = false; at = false} in
    let bs = event_pat_bounds ev_pat in
    if (List.length ev_pat) <> (List.length bs) then
      res := {!res with bef = true}
    else ();
    let succs = event_successors ev |> List.tl |> List.map (fun x -> x.id) |> Utils.IntSet.of_list in
    let preds = event_predecessors ev |> List.tl |> List.map (fun x -> x.id) |> Utils.IntSet.of_list in
    List.iter (fun (le, ri) ->
      let le_in_preds = Utils.IntSet.mem le.id preds
      and ri_in_succs = Utils.IntSet.mem ri.id succs in
      if ri_in_succs then
        res := {!res with bef = true}
      else ();
      if le_in_preds then
        res := {!res with aft = true}
      else ();
      if (le_in_preds && ri_in_succs) || le.id = ev.id || ri.id = ev.id then
        res := {!res with at = true}
      else ();
      if (not le_in_preds) && (not ri_in_succs) && le.id <> ev.id && ri.id <> ev.id then
        res := {bef = true; aft = true; at = true}
      else ()
    ) bs;
    Printf.eprintf "Event pat: %d %b %b %b\n" ev.id !res.bef !res.at !res.aft;
    !res

  (** Check that lt1 is always fully covered by lt2 *)
  let lifetime_in_range (lt1 : lifetime) (lt2 : lifetime) =
    (* 1. check if lt2's start is a predecessor of lt1's start *)
    (* 2. derive a set of all time points A potentially within lt1 *)
    (* 4. check that end time of lt2 does not match any time point in A *)
    (* FIXME: not considering lt1's dead time now *)
    Printf.eprintf "Checking lt %d %d %b\n" lt1.live.id lt2.live.id (event_is_successor lt1.live lt2.live);
    (event_is_successor lt2.live lt1.live) && (
      let r = event_pat_matches lt1.live lt2.dead in
      (not r.at) && (not r.aft)
    )

  (** Perform lifetime check on an event graph and throw out LifetimeCheckError if failed. *)
  let lifetime_check (config : Config.compile_config) (g : event_graph) =
    gen_control_set g;
    (* for debugging purposes*)
    if config.verbose then (
      print_graph g;
      print_control_set g
    );
    (* check lifetime for each use of a wire *)
    List.iter (fun ev ->
      List.iter (fun a ->
        match a with
        | RegAssign (_reg_ident, td) ->
          (* TODO: check that the register is not currently borrowed *)
          if lifetime_in_range (lifetime_immediate ev) td.lt |> not then
            raise (LifetimeCheckError "Value does not live long enough in reg assignment!")
          else ()
        | DebugPrint (_, tds) ->
          List.iter (fun td ->
            if lifetime_in_range (lifetime_immediate ev) td.lt |> not then
              raise (LifetimeCheckError "Value does not live long enough in debug print!")
            else ()
          ) tds
        | _ -> ()
      ) ev.actions
    ) g.events

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
    let branch g (ctx : t) (td : timed_data) neg: t  =
      {ctx with current = event_create g (`Branch ({data = td; neg}, ctx.current))}
  end


end

type build_context = Typing.build_context
module BuildContext = Typing.BuildContext

let rec visit_expr (graph : event_graph) (ci : cunit_info)
                   (ctx : build_context) (e : expr) : timed_data =
  match e with
  | Literal lit ->
    let (wires', w) = WireCollection.add_literal lit graph.wires in
    graph.wires <- wires';
    Typing.const_data graph (Some w) ctx.current
  | Identifier ident ->
    Typing.context_lookup ctx.typing_ctx ident |> Option.get |> Typing.sync_data graph ctx.current
  | Assign (lval, e') ->
    let td = visit_expr graph ci ctx e' in
    let reg_ident = (
      let open Lang in
      match lval with
      | Reg ident ->
        ident
      | _ -> raise (UnimplementedError "Lval with indexing/indirection unimplemented!")
    ) in
    ctx.current.actions <- (RegAssign (reg_ident, td))::ctx.current.actions;
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
    let ctx' = BuildContext.wait graph ctx td1.lt.live in
    let ctx_true = BuildContext.branch graph ctx' td1 false in
    let td2 = visit_expr graph ci ctx_true e2 in
    let ctx_false = BuildContext.branch graph ctx' td1 true in
    let td3 = visit_expr graph ci ctx_false e3 in
    (* FIXME: the dead time is incorrect *)
    let lt = {live = Typing.event_create graph (`Either (td2.lt.live, td3.lt.live));
      dead = td1.lt.dead @ td2.lt.dead @td3.lt.dead} in
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
    let ws = List.map (fun (td : timed_data) -> Option.get td.w) tds in
    let (wires', w) = WireCollection.add_concat ci.typedefs ws graph.wires in
    graph.wires <- wires';
    Typing.merged_data graph (Some w) ctx.current (List.map (fun (td : timed_data) -> td.lt) tds)
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
        ctx.current.actions <- (DebugPrint (s, timed_ws))::ctx.current.actions;
        (* TODO: incorrect lifetime *)
        {w = None; lt = Typing.lifetime_const ctx.current}
      | DebugFinish ->
        ctx.current.actions <- DebugFinish::ctx.current.actions;
        {w = None; lt = Typing.lifetime_const ctx.current}
    )
  | Send send_pack ->
    let td = visit_expr graph ci ctx send_pack.send_data in
    let _msg = MessageCollection.lookup_message graph.messages send_pack.send_msg_spec ci.channel_classes |> Option.get in
    (* TODO: lifetime checking *)
    let ntd = Typing.send_msg_data graph send_pack.send_msg_spec ctx.current in
    ctx.current.sustained_actions <-
      {
        until = ntd.lt.live;
        ty = Send (send_pack.send_msg_spec, td)
      }::ctx.current.sustained_actions;
    ntd
  | Recv recv_pack ->
    let msg = MessageCollection.lookup_message graph.messages recv_pack.recv_msg_spec ci.channel_classes |> Option.get in
    let (wires', w) = WireCollection.add_msg_port ci.typedefs recv_pack.recv_msg_spec 0 msg graph.wires in
    graph.wires <- wires';
    let ntd = Typing.recv_msg_data graph (Some w) recv_pack.recv_msg_spec msg ctx.current in
    ctx.current.sustained_actions <- {until = ntd.lt.live; ty = Recv recv_pack.recv_msg_spec}::ctx.current.sustained_actions;
    ntd
  | Indirect (e', fieldname) ->
    let td = visit_expr graph ci ctx e' in
    let w = Option.get td.w in
    let (offset_le, offset_ri, new_dtype) = TypedefMap.data_type_indirect ci.typedefs w.dtype fieldname |> Option.get in
    let (wires', new_w) = WireCollection.add_slice new_dtype w offset_le offset_ri graph.wires in
    graph.wires <- wires';
    {
      td with
      w = Some new_w
    }
  | Index (e', ind) ->
    let td = visit_expr graph ci ctx e' in
    let w = Option.get td.w in
    let (offset_le, offset_ri, new_dtype) = TypedefMap.data_type_index ci.typedefs w.dtype ind |> Option.get in
    let (wires', new_w) = WireCollection.add_slice new_dtype w offset_le offset_ri graph.wires in
    graph.wires <- wires';
    {
      td with
      w = Some new_w
    }
  | Record (record_ty_name, field_exprs) ->
    (
      match TypedefMap.data_type_name_resolve ci.typedefs @@ `Named record_ty_name with
      | Some (`Record record_fields) ->
        (
          match Utils.list_match_reorder (List.map fst record_fields) field_exprs with
          | Some expr_reordered ->
            let tds = List.map (visit_expr graph ci ctx) expr_reordered in
            let ws = List.rev_map (fun ({w; _} : timed_data) -> Option.get w) tds in
            let (wires', w) = WireCollection.add_concat ci.typedefs ws graph.wires in
            graph.wires <- wires';
            List.map (fun ({lt; _} : timed_data) -> lt) tds |>
            Typing.merged_data graph (Some w) ctx.current
          | _ -> raise (TypeError "Invalid record type value!")
        )
      | _ -> raise (TypeError "Invalid record type name!")
    )
  | Construct (cstr_spec, cstr_expr_opt) ->
    (
      match TypedefMap.data_type_name_resolve ci.typedefs @@ `Named cstr_spec.variant_ty_name with
      | Some (`Variant _ as dtype) ->
        let e_dtype_opt = variant_lookup_dtype dtype cstr_spec.variant in
        (
          match e_dtype_opt, cstr_expr_opt with
          | Some e_dtype, Some cstr_expr ->
            let td = visit_expr graph ci ctx cstr_expr in
            let w = Option.get td.w in
            let tag_size = variant_tag_size dtype
            and data_size = TypedefMap.data_type_size ci.typedefs e_dtype
            and tot_size = TypedefMap.data_type_size ci.typedefs dtype
            and var_idx = variant_lookup_index dtype cstr_spec.variant |> Option.get in
            let (wires', w_tag) = WireCollection.add_literal (WithLength (tag_size, var_idx)) graph.wires in
            let (wires', new_w) = if tot_size = tag_size + data_size then
              (* no padding *)
              WireCollection.add_concat ci.typedefs [w; w_tag] wires'
            else begin
              (* padding needed *)
              let (wires', w_pad) = WireCollection.add_literal (WithLength (tot_size - tag_size - data_size, 0)) wires' in
              WireCollection.add_concat ci.typedefs [w_pad; w; w_tag] wires'
            end in
            graph.wires <- wires';
            { td with w = Some new_w }
          | None, None ->
            let tag_size = variant_tag_size dtype
            and tot_size = TypedefMap.data_type_size ci.typedefs dtype
            and var_idx = variant_lookup_index dtype cstr_spec.variant |> Option.get in
            let (wires', w_tag) = WireCollection.add_literal (WithLength (tag_size, var_idx)) graph.wires in
            let (wires', new_w) = if tot_size = tag_size then
              (wires', w_tag)
            else begin
              let (wires', w_pad) = WireCollection.add_literal (WithLength (tot_size - tag_size, 0)) wires' in
              WireCollection.add_concat ci.typedefs [w_pad; w_tag] wires'
            end in
            graph.wires <- wires';
            Typing.const_data graph (Some new_w) ctx.current
          | _ -> raise (TypeError "Invalid variant construct expression!")
        )
      | _ -> raise (TypeError "Invalid variant type name!")
    )
  | _ -> raise (UnimplementedError "Unimplemented expression!")


(* Builds the graph representation for each process To Do: Add support for commands outside loop (be executed once or continuosly)*)
let build_proc (config : Config.compile_config) (ci : cunit_info) (proc : proc_def) =
  let proc_threads = List.mapi (fun i e ->  (* Use List.mapi to get the index *)
    let graph = {
      (* name = proc.name; *)
      thread_id = i;
      events = [];
      wires = WireCollection.empty;
      channels = proc.body.channels;
      messages = MessageCollection.create proc.body.channels proc.args ci.channel_classes;
      spawns = proc.body.spawns;
      regs = proc.body.regs;
      last_event_id = 0;
    } in
    let td = visit_expr graph ci (BuildContext.create_empty graph) e in
    graph.last_event_id <- td.lt.live.id;  (* Set last_event_id for each graph *)
    Typing.lifetime_check config graph;
    graph
  ) proc.body.loops in
  {name = proc.name; threads = proc_threads};

type event_graph_collection = {
  event_graphs : proc_graph list;
  typedefs : TypedefMap.t;
  channel_classes : channel_class_def list;
}

let build (config : Config.compile_config) (cunit : compilation_unit) =
  let typedefs = TypedefMap.of_list cunit.type_defs in
  let ci = { typedefs; channel_classes = cunit.channel_classes } in
  let graphs = List.map (build_proc config ci) cunit.procs in
  {
    event_graphs = graphs;
    typedefs;
    channel_classes = cunit.channel_classes;
  }


module Endpoint = struct
  let canonicalize (endpoint : endpoint_def) : identifier =
    match endpoint.dir with
    | Left -> endpoint.name
    | Right -> Option.value ~default:endpoint.name endpoint.opp

  let canonicalize_endpoint_name (endpoint_name : identifier) (g : event_graph) : identifier =
    match MessageCollection.lookup_endpoint g.messages endpoint_name with
    | Some endpoint_local -> canonicalize endpoint_local
    | None -> endpoint_name
end

let canonicalize_endpoint_name = Endpoint.canonicalize_endpoint_name
