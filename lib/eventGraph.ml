open Lang
open Except

exception EventGraphError of string * Lang.code_span

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
and action =
  | DebugPrint of string * timed_data list
  | DebugFinish
  | RegAssign of string * timed_data
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
  name: identifier;
  threads: event_graph list;
  shared_vars_info : (identifier, shared_var_info) Hashtbl.t;
}

exception LifetimeCheckError of string
module Typing = struct
  type context = timed_data Utils.string_map

  type build_context = {
    typing_ctx : context;
    current : event;
    shared_vars_info : (identifier, shared_var_info) Hashtbl.t;
    lt_check_phase : bool;
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
        outs = []; graph = g} in
      g.events <- n::g.events;
      (
        match source with
        | `Later (e1, e2)
        | `Either (e1, e2) ->
          e1.outs <- n::e1.outs;
          e2.outs <- n::e2.outs
        | `Branch (_, ev')
        | `Seq (ev', _) ->
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
    {w = None; lt = {live = live_event; dead = [(live_event, `Eternal)]}; reg_borrows = []}
  let sync_data g (current : event) (td: timed_data) =
    let ev = event_create g (`Later (current, td.lt.live)) in
    {td with lt = {td.lt with live = ev}}

  let const_data _g (w : wire option) (current : event) = {w; lt = lifetime_const current; reg_borrows = []}
  let merged_data g (w : wire option) (current : event) (tds : timed_data list) =
    let lts = List.map (fun x -> x.lt) tds in
    match lts with
    | [] -> const_data g w current
    | lt::lts' ->
      let lt' = List.fold_left (lifetime_intersect g) lt lts' in
      let reg_borrows' = List.concat_map (fun x -> x.reg_borrows) tds in
      {w; lt = lt'; reg_borrows = reg_borrows'}
  let derived_data (w : wire option) (td : timed_data) = {td with w}
  let send_msg_data g (msg : message_specifier) (current : event) =
    let live_event = event_create g (`Seq (current, `Send msg)) in
    {w = None; lt = {live = live_event; dead = [(live_event, `Eternal)]}; reg_borrows = []}

  let sync_event_data g ident gtd current =
    let event_synced = event_create g (`Seq (current, `Sync ident)) in
    let dpat = gtd.glt.e in
    (
      match dpat with
      | `Cycles _ -> ()
      | _ -> raise (UnimplementedError "Non-static lifetime for shared data is unsupported!")
    );
    {w = gtd.w; lt = {live = event_synced; dead = [(event_synced, dpat)]}; reg_borrows = []}

  let recv_msg_data g (w : wire option) (msg : message_specifier) (msg_def : message_def) (current : event) =
    let event_received = event_create g (`Seq (current, `Recv msg)) in
    let stype = List.hd msg_def.sig_types in
    let e = delay_pat_globalise msg.endpoint stype.lifetime.e in
    {w; lt = {live = event_received; dead = [(event_received, e)]}; reg_borrows = []}

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
          (fun (a : action ast_node) ->
            match a.d with
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
          (fun (a : sustained_action ast_node) ->
            match a.d.ty with
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
          | `Later (e1, e2) | `Either (e1, e2) ->
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
        List.iter (fun (a : action ast_node) ->
          match a.d with
          | RegAssign (reg_ident, _) -> (
            if in_control_set_reg ev reg_ident |> not then
              raise (EventGraphError ("Non-linearizable register assignment!", a.span))
            else ()
          )
          | _ -> ()
        ) ev.actions;
        List.iter (fun (sa : sustained_action ast_node) ->
          match sa.d.ty with
          | Send (ms, _) | Recv ms -> (
            if in_control_set_endps ev ms.endpoint |> not then
              raise (EventGraphError ("Non-linearizable endpoint use!", sa.span))
            else ()
          )
        ) ev.sustained_actions
      )
      g.events

  (** evs must be in topological order*)
  let find_controller (endpts : identifier list) =
    List.find_opt
      (fun ev ->
        List.for_all (fun endpt -> in_control_set_endps ev endpt) endpts
      )

  let find_first_msg_after (ev : event) (msg: Lang.message_specifier) =
    event_successors ev |> List.tl |> List.find_opt
      (fun ev' ->
        List.exists (fun sa ->
          match sa.d.ty with
          | Send (msg', _) | Recv msg' -> msg' = msg
        ) ev'.sustained_actions
      )

  let event_succ_msg_match_earliest (ev : event) (msg : message_specifier) =
    let preds = event_predecessors ev in
    let pred_controller = find_controller [msg.endpoint] (List.rev preds) |> Option.get in
    find_first_msg_after pred_controller msg

  let event_succ_msg_match_latest (ev : event) (msg : message_specifier) =
    let succs = event_successors ev in
    let succ_controller = find_controller [msg.endpoint] succs |> Option.get in
    find_first_msg_after succ_controller msg

  module IntHashtbl = Hashtbl.Make(Int)
  let event_distance_max = 1 lsl 20
  let event_succ_distance non_succ_dist msg_dist_f either_dist_f (ev : event) =
    let succs = event_successors ev in
    let dist = IntHashtbl.create 8 in
    IntHashtbl.add dist ev.id 0;
    let get_dist ev' = IntHashtbl.find_opt dist ev'.id |> Option.value ~default:non_succ_dist in
    let set_dist ev' d = IntHashtbl.add dist ev'.id d in
    List.tl succs |> List.iter (fun ev' ->
      let d = match ev'.source with
      | `Root -> raise (LifetimeCheckError "Unexpected root!")
      | `Later (ev1, ev2) -> max (get_dist ev1) (get_dist ev2)
      | `Seq (ev1, ad) ->
        let d1 = get_dist ev1 in
        (
          match ad with
          | `Cycles n' -> d1 + n'
          | `Send _ | `Recv _ | `Sync _ -> msg_dist_f d1
        )
      | `Branch (_, ev1) -> get_dist ev1
      | `Either (ev1, ev2) -> either_dist_f (get_dist ev1) (get_dist ev2)
      in
      set_dist ev' (min d event_distance_max)
    );
    dist

  let event_min_distance =
    event_succ_distance 0 (fun d -> d) min

  let event_max_distance =
    event_succ_distance event_distance_max (fun _ -> event_distance_max) max

  (** Is ev_pat1 matched always no later than ev_pat2 is matched?
  Produce a conservative result.
  Only length 1 ev_pat1 supported. *)
  let event_pat_rel (ev_pat1 : event_pat) (ev_pat2 : event_pat) =
    if (List.length ev_pat1) <> 1 then
      false
    else (
      let (ev1, d_pat1) = List.hd ev_pat1 in
      let check_f = match d_pat1 with
        | `Eternal -> fun _ev2 d_pat2 -> d_pat2 = `Eternal
        | `Cycles n1 ->
          (* compute the min possible cycle distance from n1 to successors *)
          let dist = event_min_distance ev1 in
          let get_dist ev' = IntHashtbl.find_opt dist ev'.id |> Option.value ~default:0 in
          (
            fun ev2 d_pat2 ->
              match d_pat2 with
              | `Eternal -> true
              | `Cycles n2 ->
                if event_is_successor ev1 ev2 then
                  let d = get_dist ev2 in
                  n1 <= d + n2
                else (
                  let dist2 = event_max_distance ev2 in
                  let d = IntHashtbl.find_opt dist2 ev1.id |> Option.value ~default:event_distance_max in
                  n1 + d <= n2
                )
              | `Message msg ->
                  let n2_opt' = event_succ_msg_match_earliest ev2 msg in
                  match n2_opt' with
                  | None -> true (* never matching *)
                  | Some n2' ->
                    if event_is_predecessor ev1 n2' then
                      true
                    else if event_is_successor ev1 n2' then
                      n1 <= get_dist n2'
                    else false
          )
        | `Message msg ->
          (
            let ri1_opt = event_succ_msg_match_latest ev1 msg in (* latest estimated *)
            match ri1_opt with
            | None -> fun _ _ -> true (* End of the second loop. Don't handle *)
            | Some ri1 ->
              fun ev2 d_pat2 -> (
                match d_pat2 with
                | `Eternal -> true
                | `Cycles n2 -> (* earliest estimate *)
                  if event_is_predecessor ev2 ri1 then true
                  else (
                    let dist = event_max_distance ev2 in
                    let d = IntHashtbl.find_opt dist ri1.id |> Option.value ~default:event_distance_max in
                    d <= n2
                  )
                | `Message msg2 ->
                  (
                    let le2_opt = event_succ_msg_match_earliest ev2 msg2 in
                    match le2_opt with
                    | None -> true
                    | Some le2 -> event_is_predecessor le2 ri1
                  )
              )
          )
      in
      List.for_all (fun (ev2, d_pat2) -> check_f ev2 d_pat2) ev_pat2
    )

  (** Check that lt1 is always fully covered by lt2 *)
  let lifetime_in_range (lt1 : lifetime) (lt2 : lifetime) =
    (* 1. check if lt2's start is a predecessor of lt1's start *)
    (* 2. derive a set of all time points A potentially within lt1 *)
    (* 4. check that end time of lt2 does not match any time point in A *)
    (* (event_is_successor lt2.live lt1.live) && (
      let r = event_pat_matches lt1.live lt2.dead in
      (not r.at) && (not r.aft)
    ) *)
    (event_is_successor lt2.live lt1.live) && (event_pat_rel lt1.dead lt2.dead)

  (** Definitely disjoint? *)
  let lifetime_disjoint lt1 lt2 =
    assert (List.length lt1.dead = 1);
    (* to be disjoint, either r1 <= l2 or r2 <= l1 *)
    if event_pat_rel lt1.dead [(lt2.live, `Cycles 0)] then
      true
    else
      List.for_all (fun de -> event_pat_rel [de] [(lt1.live, `Cycles 0)]) lt2.dead

  module StringHashtbl = Hashtbl.Make(String)

  (** Perform lifetime check on an event graph and throw out LifetimeCheckError if failed. *)
  let lifetime_check (config : Config.compile_config) (ci : cunit_info) (g : event_graph) =
    gen_control_set g;
    (* for debugging purposes*)
    if config.verbose then (
      print_graph g;
      print_control_set g
    );
    let reg_borrows = StringHashtbl.create 8 in
    let msg_borrows = StringHashtbl.create 8 in
    (* check lifetime for each use of a wire *)
    let not_borrowed tbl s lt =
      let borrows = StringHashtbl.find_opt tbl s |> Option.value ~default:[] in
      List.for_all (lifetime_disjoint lt) borrows
    in
    let not_borrowed_and_add tbl s lt =
      let borrows = StringHashtbl.find_opt tbl s |> Option.value ~default:[] in
      let not_borrowed = List.for_all (lifetime_disjoint lt) borrows in
      if not_borrowed then StringHashtbl.replace tbl s (lt::borrows);
      not_borrowed
    in
    let borrow_add tbl s lt =
      let borrows = StringHashtbl.find_opt tbl s |> Option.value ~default:[] in
      StringHashtbl.replace tbl s (lt::borrows)
    in
    let visit_actions a_visitor sa_visitor =
      List.iter (fun ev ->
        List.iter (a_visitor ev) ev.actions;
        List.iter (sa_visitor ev) ev.sustained_actions
      )
    in
    (* add all required borrows of registers first *)
    (* the time pat until which td lives *)
    let delay_pat_reduce_cycles n dpat =
      match dpat with
      | `Cycles n' -> `Cycles (max 0 (n' - n))
      | _ -> dpat
    in
    let td_to_live_until = ref [] in
    visit_actions
      (fun ev a ->
        match a.d with
        | RegAssign (_, td) ->
          (* `Cycles 0 rather than 1 because in the last cycle it is okay to update the register *)
          td_to_live_until := (td, (ev, `Cycles 0))::!td_to_live_until
        | DebugPrint (_, tds) ->
          let ns = List.map (fun td -> (td, (ev, `Cycles 0))) tds in
          td_to_live_until := ns @ !td_to_live_until
        | DebugFinish -> ()
        | PutShared (_, si, td) ->
          td_to_live_until := (td, (ev, si.value.glt.e))::!td_to_live_until
      )
      (fun _ev sa ->
        match sa.d.ty with
        | Send (msg, td) ->
          let msg_d = MessageCollection.lookup_message g.messages msg ci.channel_classes |> Option.get in
          let stype = List.hd msg_d.sig_types in
          let e_dpat = delay_pat_globalise msg.endpoint stype.lifetime.e |> delay_pat_reduce_cycles 1 in
          td_to_live_until := (td, (sa.d.until, e_dpat))::!td_to_live_until
        | Recv _ -> ()
      )
      g.events;
    List.iter (fun (td, dead) ->
      List.iter (fun (reg_ident, live) ->
        borrow_add reg_borrows reg_ident {live; dead = [dead]}
      ) td.reg_borrows
    ) !td_to_live_until;
    (* check register and message borrows *)
    visit_actions
      (fun ev a ->
        match a.d with
        | RegAssign (reg_ident, td) ->
          let lt = lifetime_immediate ev in
          if not_borrowed reg_borrows reg_ident lt |> not then
            raise (EventGraphError ("Attempted assignment to a borrowed register!", a.span))
          else ();
          if lifetime_in_range lt td.lt |> not then
            raise (EventGraphError ("Value does not live long enough in reg assignment!", a.span))
          else ()
        | DebugPrint (_, tds) ->
          List.iter (fun td ->
            if lifetime_in_range (lifetime_immediate ev) td.lt |> not then
              raise (EventGraphError ("Value does not live long enough in debug print!", a.span))
            else ()
          ) tds
        | DebugFinish -> ()
        | PutShared (_, si, td) ->
          if lifetime_in_range {live = ev; dead = [(ev, si.value.glt.e)]} td.lt |> not then
            raise (EventGraphError ("Value does not live long enough in put!", a.span))
          else ()
      )
      (fun ev sa ->
        match sa.d.ty with
        | Send (msg, td) ->
          let msg_d = MessageCollection.lookup_message g.messages msg ci.channel_classes |> Option.get in
          let stype = List.hd msg_d.sig_types in
          let e_dpat = delay_pat_globalise msg.endpoint stype.lifetime.e in
          let lt = {live = ev; dead = [(sa.d.until, e_dpat)]} in
          if not_borrowed_and_add msg_borrows (string_of_msg_spec msg) lt |> not then
            raise (EventGraphError ("Potentially conflicting message sending!", sa.span))
          else ();
          if lifetime_in_range lt td.lt |> not then
            raise (EventGraphError ("Value not live long enough in message send!", sa.span))
        | Recv _ -> ()
      )
      g.events

  module BuildContext = struct
    type t = build_context
    let create_empty g si lt_check_phase: t = {
      typing_ctx = context_empty;
      current = event_create g `Root;
      shared_vars_info = si;
      lt_check_phase;
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
                   (ctx : build_context) (e : expr_node) : timed_data =
  match e.d with
  | Literal lit ->
    let (wires', w) = WireCollection.add_literal lit graph.wires in
    graph.wires <- wires';
    Typing.const_data graph (Some w) ctx.current
  | Sync ident ->
    (
      match Hashtbl.find_opt ctx.shared_vars_info ident with
        | Some shared_info -> Typing.sync_event_data graph ident shared_info.value ctx.current
        | None -> raise (EventGraphError (("Undefined identifier: " ^ ident), e.span))
    )
  | Identifier ident ->
    (
      match Typing.context_lookup ctx.typing_ctx ident with
      | Some td -> Typing.sync_data graph ctx.current td
      | None -> raise (EventGraphError (("Undefined identifier: " ^ ident), e.span))
    )
  | Assign (lval, e') ->
    let td = visit_expr graph ci ctx e' in
    let reg_ident = (
      let open Lang in
      match lval with
      | Reg ident ->
        ident
      | _ -> raise (EventGraphError ("Lval with indexing/indirection unimplemented!", e.span))
    ) in
    ctx.current.actions <- (RegAssign (reg_ident, td) |> tag_with_span e.span)::ctx.current.actions;
    Typing.cycles_data graph 1 ctx.current
  | Binop (binop, e1, e2) ->
    let td1 = visit_expr graph ci ctx e1
    and td2 = visit_expr graph ci ctx e2 in
    let w1 = Option.get td1.w
    and w2 = Option.get td2.w in
    let (wires', w) = WireCollection.add_binary ci.typedefs binop w1 w2 graph.wires in
    graph.wires <- wires';
    Typing.merged_data graph (Some w) ctx.current [td1; td2]
  | Unop (unop, e') ->
    let td = visit_expr graph ci ctx e' in
    let w' = Option.get td.w in
    let (wires', w) = WireCollection.add_unary ci.typedefs unop w' graph.wires in
    graph.wires <- wires';
    Typing.derived_data (Some w) td
  | Tuple [] -> Typing.const_data graph None ctx.current
  | LetIn (idents, e1, e2) ->
    let td1 = visit_expr graph ci ctx e1 in
    (
      match idents, td1.w with
      | [], None | ["_"], _ ->
        let td = visit_expr graph ci ctx e2 in
        let lt = Typing.lifetime_intersect graph td1.lt td.lt in
        {td with lt} (* forcing the bound value to be awaited *)
      | [ident], _ ->
        let ctx' = BuildContext.add_binding ctx ident td1 in
        visit_expr graph ci ctx' e2
      | _ -> raise (EventGraphError ("Discarding expression results!", e.span))
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
    let lt = {live = Typing.event_create graph (`Either (td2.lt.live, td3.lt.live));
      dead = td1.lt.dead @ td2.lt.dead @td3.lt.dead} in
    let reg_borrows' = td1.reg_borrows @ td2.reg_borrows @ td3.reg_borrows in
    (
      match td2.w, td3.w with
      | None, None -> {w = None; lt; reg_borrows = reg_borrows'}
      | Some w2, Some w3 ->
        let (wires', w) = WireCollection.add_switch ci.typedefs [(w1, w2)] w3 graph.wires in
        graph.wires <- wires';
        {w = Some w; lt; reg_borrows = reg_borrows'}
      | _ -> raise (EventGraphError ("Invalid if expression!", e.span))
    )
  | Concat es ->
    let tds = List.map (visit_expr graph ci ctx) es in
    let ws = List.map (fun (td : timed_data) -> Option.get td.w) tds in
    let (wires', w) = WireCollection.add_concat ci.typedefs ws graph.wires in
    graph.wires <- wires';
    Typing.merged_data graph (Some w) ctx.current tds
  | Read reg_ident ->
    let r = List.find (fun (r : Lang.reg_def) -> r.name = reg_ident) graph.regs in
    let (wires', w) = WireCollection.add_reg_read ci.typedefs r graph.wires in
    graph.wires <- wires';
    {w = Some w; lt = Typing.lifetime_const ctx.current; reg_borrows = [(reg_ident, ctx.current)]}
  | Debug op ->
    (
      match op with
      | DebugPrint (s, e_list) ->
        let timed_ws = List.map (visit_expr graph ci ctx) e_list in
        ctx.current.actions <- (DebugPrint (s, timed_ws) |> tag_with_span e.span)::ctx.current.actions;
        {w = None; lt = Typing.lifetime_const ctx.current; reg_borrows = []}
      | DebugFinish ->
        ctx.current.actions <- (tag_with_span e.span DebugFinish)::ctx.current.actions;
        {w = None; lt = Typing.lifetime_const ctx.current; reg_borrows = []}
    )
  | Send send_pack ->
    let td = visit_expr graph ci ctx send_pack.send_data in
    let ntd = Typing.send_msg_data graph send_pack.send_msg_spec ctx.current in
    ctx.current.sustained_actions <-
      ({
        until = ntd.lt.live;
        ty = Send (send_pack.send_msg_spec, td)
      } |> tag_with_span e.span)::ctx.current.sustained_actions;
    ntd
  | Recv recv_pack ->
    let msg = MessageCollection.lookup_message graph.messages recv_pack.recv_msg_spec ci.channel_classes |> Option.get in
    let (wires', w) = WireCollection.add_msg_port ci.typedefs recv_pack.recv_msg_spec 0 msg graph.wires in
    graph.wires <- wires';
    let ntd = Typing.recv_msg_data graph (Some w) recv_pack.recv_msg_spec msg ctx.current in
    ctx.current.sustained_actions <-
      ({until = ntd.lt.live; ty = Recv recv_pack.recv_msg_spec} |> tag_with_span e.span)::ctx.current.sustained_actions;
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
            Typing.merged_data graph (Some w) ctx.current tds
          | _ -> raise (EventGraphError ("Invalid record type value!", e.span))
        )
      | _ -> raise (EventGraphError ("Invalid record type name!", e.span))
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
          | _ -> raise (EventGraphError ("Invalid variant construct expression!", e.span))
        )
      | _ -> raise (EventGraphError ("Invalid variant type name!", e.span))
    )
  | SharedAssign (id, value_expr) ->
    let shared_info = Hashtbl.find ctx.shared_vars_info id in
    if graph.thread_id = shared_info.assigning_thread then
      let value_td = visit_expr graph ci ctx value_expr in
      if not ctx.lt_check_phase then (
        if (Option.is_some shared_info.value.w) || (Option.is_some shared_info.assigned_at) then
          raise (EventGraphError ("Shared value can only be assigned in one place!", e.span));
        shared_info.value.w <- value_td.w;
        shared_info.assigned_at <- Some ctx.current;
      );
      ctx.current.actions <- (PutShared (id, shared_info, value_td) |> tag_with_span e.span)::ctx.current.actions;
      Typing.const_data graph None ctx.current
    else
      raise (EventGraphError ("Shared variable assigned in wrong thread", e.span))
  | _ -> raise (EventGraphError ("Unimplemented expression!", e.span))


(* Builds the graph representation for each process To Do: Add support for commands outside loop (be executed once or continuosly)*)
let build_proc (config : Config.compile_config) (ci : cunit_info) (proc : proc_def) : proc_graph =
  let shared_vars_info = Hashtbl.create (List.length proc.body.shared_vars) in
  List.iter (fun sv ->
    let v = {
      w = None;
      glt = sv.shared_lifetime;
    } in
    let r = {
      assigning_thread = sv.assigning_thread;
      value = v;
      assigned_at = None;
    } in
      Hashtbl.add shared_vars_info sv.ident r
    ) proc.body.shared_vars;
    let proc_threads = List.mapi (fun i e ->
      let graph = {
        thread_id = i;
        events = [];
        wires = WireCollection.empty;
        channels = proc.body.channels;
        messages = MessageCollection.create proc.body.channels proc.args ci.channel_classes;
        spawns = proc.body.spawns;
        regs = proc.body.regs;
        last_event_id = 0;
      } in
      (* Bruteforce treatment: just run twice *)
      let tmp_graph = {graph with last_event_id = 0} in
      let _ = visit_expr tmp_graph ci
        (BuildContext.create_empty tmp_graph shared_vars_info true)
        (dummy_ast_node_of_data (Wait (e, e))) in
      Typing.lifetime_check config ci tmp_graph;
      (* discard after type checking *)
      let ctx = (BuildContext.create_empty graph shared_vars_info false) in
      let td = visit_expr graph ci ctx e in
        graph.last_event_id <- td.lt.live.id;
        graph
    ) proc.body.loops in

     {name = proc.name; threads = proc_threads; shared_vars_info}

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
