open Lang
open EventGraph
open GraphAnalysis

(** Check if the uses of endpoints and registers follow defined order. *)
let check_linear (config : Config.compile_config) lookup_message (g : event_graph) =
  let events_rev = List.rev g.events in
  let reg_ops = ref [] in (* all uses of registers *)
  let msg_uses = Hashtbl.create 2 in (* uses of messages *)
  let string_of_msg msg = Printf.sprintf "%s.%s" msg.endpoint msg.msg in (* serialise messagse *)
  let add_msg msg ev sa_span =
    let msg_str = string_of_msg msg in
    match Hashtbl.find_opt msg_uses msg_str with
    | Some li -> li := (ev, sa_span)::!li
    | None -> Hashtbl.add msg_uses msg_str (ref [(ev, sa_span)])
  in
  (* gather register and message uses *)
  List.iter
    (fun (ev : event) ->
      List.iter (fun ac_span ->
        match ac_span.d with
        | DebugFinish -> ()
        | DebugPrint (_, _ds) -> ()
          (* List.iter (add_reg_ops_td ev) ds *)
        | RegAssign (lv, _td) ->
          (* add_reg_ops_td ev td; *)
          reg_ops := (ev, lv.lval_range, ac_span.span)::!reg_ops
        | PutShared (_, _, _td) ->
          ()
          (* add_reg_ops_td ev td *)
      ) ev.actions;
      List.iter (fun sa_span ->
        match sa_span.d.ty with
        | Send (msg, _)
        | Recv msg -> add_msg msg ev sa_span
      ) ev.sustained_actions
    )
    events_rev;

  if config.verbose then (
    List.iter (fun (ev, range, _span) ->
      let range_s = match range.subreg_range_interval with
      | (Const n, sz) -> Printf.sprintf "%s[%d, %d]" range.subreg_name n sz
      | (NonConst _, sz) -> Printf.sprintf "%s[var, %d]" range.subreg_name sz
      in
      Printf.eprintf "RegAssign at %d to %s\n" ev.id range_s
    ) !reg_ops;
    Hashtbl.iter (fun msg_str li ->
      Printf.eprintf "Uses of %s:\n" msg_str;
      List.iter (fun (ev, sa_span) ->
        Printf.eprintf "  %d -> %d\n" ev.id sa_span.d.until.id
      ) !li
    ) msg_uses
  );

  (* check for violations of linearity for register assignments *)
  let rec check_reg_violation = function
    | (ev, range, span)::remaining ->
      List.iter (fun (ev', range', span') ->
        if (EventGraphOps.subreg_ranges_possibly_intersect range range') &&
           (events_get_order g.events lookup_message ev ev' |> is_strict_ordered |> not) then
            raise (LifetimeCheckError
                [
                  Text "Non-linearizable register assignment!";
                  Except.codespan_local span;
                  Text "Conflicting with:";
                  Except.codespan_local span';
                ])
      ) remaining;
      check_reg_violation remaining
    | [] -> ()
  in
  check_reg_violation !reg_ops;

  (* check for violations of linearity for message uses *)
  Hashtbl.iter (fun _msg_str li ->
    let rec check_msg_violation = function
      | (ev, sa_span)::li' ->
        List.iter (fun (ev', sa_span') ->
          let order1 = events_get_order g.events lookup_message ev sa_span'.d.until in
          let order2 = events_get_order g.events lookup_message sa_span.d.until ev' in
          let order_to_sign = function
            | Before | BeforeEq -> -1
            | After | AfterEq -> 1
            | AlwaysEq | Unreachable -> 0
            | Unordered -> -2
          in
          let check_ok =
            match order_to_sign order1, order_to_sign order2 with
            | -2, _ | _, -2 -> false
            | 1, _ | _, -1 -> true
            | 0, _ | _, 0 -> true
            | _ -> false
          in
          if not check_ok then
            raise (LifetimeCheckError
                  [
                    Text "Non-linearizable message use!";
                    Except.codespan_local sa_span.span;
                    Text "Conflicting with:";
                    Except.codespan_local sa_span'.span;
                  ])
        ) li';
        check_msg_violation li'
      | [] -> ()
    in
    check_msg_violation !li
  ) msg_uses

module IntHashtbl = Hashtbl.Make(Int)

(* Newer and more rigorous algorithm. *)
let event_pat_rel2 events lookup_message ev_pat1 ev_pat2 =
  let get_points_dist (ev, d_pat) =
    match d_pat with
    | `Cycles n -> ([ev], n)
    | `Eternal -> ([ev], -1)
    | `Message m ->
      let g = GraphAnalysis.events_first_msg events ev m in
      (* Printf.eprintf "FM %d %d\n" ev.id (List.length g); *)
      (g, 0)
  in
  match ev_pat1 with
  | [spat1] -> (
    (* source_points is the set of source points to consider *)
    (* dist1 is the distance required, -1 to indicate eternal *)
    let (source_points, dist1) = get_points_dist spat1 in
    (* Printf.eprintf "Sources %d (%d) = " (fst spat1).id dist1;
    List.iter (fun source -> Printf.eprintf "%d " source.id) source_points;
    Printf.eprintf "\n"; *)
    let targets_list = List.map get_points_dist ev_pat2 in
    if dist1 = -1 then
      List.for_all (fun (_, dist2) -> dist2 = -1) targets_list
    else (
      (* dist1 != -1 *)
      List.for_all (fun (target_points, dist2) ->
        if dist2 = -1 then
          true
        else (
          (* Printf.eprintf "Targets (%d) = " dist2;
          List.iter (fun target -> Printf.eprintf "%d " target.id) target_points;
          Printf.eprintf "\n"; *)
          (* neither dist2 nor dist1 is -1 *)
          List.for_all (fun target ->
            let slacks = GraphAnalysis.events_max_dist events lookup_message target in
            List.for_all (fun source -> slacks.(source.id) <= dist2 - dist1) source_points
          ) target_points
        )
      ) targets_list
      (* List.for_all (fun source ->
        let slacks = GraphAnalysis.events_max_dist events source in
        List.for_all (fun (target_points, dist2) ->
          if dist2 = -1 then
            true
          else (
            (* neither dist2 nor dist1 is -1 *)
            List.for_all (fun target -> slacks.(target.id) <= dist2 - dist1) target_points
          )
        ) targets_list
      ) source_points *)
    )
  )
  | _ -> false (* not to be supported *)

(** Check that lt1 is always fully covered by lt2 *)
let lifetime_in_range events lookup_message (lt1 : lifetime) (lt2 : lifetime) =
  (* 1. check if lt2's start is a predecessor of lt1's start *)
  (* 2. derive a set of all time points A potentially within lt1 *)
  (* 4. check that end time of lt2 does not match any time point in A *)
  (* (event_is_successor lt2.live lt1.live) && (
    let r = event_pat_matches lt1.live lt2.dead in
    (not r.at) && (not r.aft)
  ) *)
  (event_pat_rel2 events lookup_message [(lt2.live, `Cycles 0)] [(lt1.live, `Cycles 0)])
    && (event_pat_rel2 events lookup_message lt1.dead lt2.dead)

(** Definitely disjoint? *)
let lifetime_disjoint events lookup_message lt1 lt2 =
  assert (List.length lt1.dead = 1);
  (* to be disjoint, either r1 <= l2 or r2 <= l1 *)
  (event_pat_rel2 events lookup_message lt1.dead [(lt2.live, `Cycles 0)])
  || (List.for_all (fun de -> event_pat_rel2 events lookup_message [de] [(lt1.live, `Cycles 0)]) lt2.dead)


(* An internal identifier for a message specifier. *)
let msg_ident msg = Printf.sprintf "%s@%s" msg.endpoint msg.msg

module StringHashtbl = Hashtbl.Make(String)

let lifetime_check (config : Config.compile_config) (ci : cunit_info) (g : event_graph) =
  let lookup_message msg = MessageCollection.lookup_message g.messages msg ci.channel_classes in

  if config.verbose then (
    EventGraphOps.print_graph g;
    Printf.eprintf "// BEGIN GRAPH IN DOT FORMAT\n";
    Printf.eprintf "// can render to PDF with 'dot -Tpdf -O <filename>'\n";
    EventGraphOps.print_dot_graph g Out_channel.stderr;
    Printf.eprintf "// END GRAPH IN DOT FORMAT\n"
  );

  GraphAnalysis.events_prepare_outs g.events;

  (* check that it takes at least one cycle to execute *)
  let root_ev = List.find (fun e -> e.source = `Root None) g.events in
  let last_ev = List.hd g.events in (* assuming reverse topo order *)
  let dist = event_min_distance g.events root_ev root_ev in
  if IntHashtbl.find dist last_ev.id = 0 then
    raise (LifetimeCheckError
      [
        Text "All paths must take at least one cycle to complete!";
        Except.codespan_local g.thread_codespan
      ]
    );

  check_linear config lookup_message g;

  let reg_borrows = StringHashtbl.create 8 in (* regname -> (lifetime, range)*)
  let msg_borrows = StringHashtbl.create 8 in
  (* check lifetime for each use of a wire *)
  let intersects_borrowed_reg s lt range =
    let borrows = StringHashtbl.find_opt reg_borrows s |> Option.value ~default:[] in
    List.filter (fun (lt_tagged', range') ->
      (EventGraphOps.subreg_ranges_possibly_intersect range range') && (lifetime_disjoint g.events lookup_message lt lt_tagged'.d |> not))
      borrows
  in
  let intersects_borrowed_msg_and_add s lt_tagged =
    let lt = lt_tagged.d in
    let borrows = StringHashtbl.find_opt msg_borrows s |> Option.value ~default:[] in
    let intersects = List.filter (fun lt_tagged' -> lifetime_disjoint g.events lookup_message lt lt_tagged'.d |> not) borrows in
    if intersects = [] then StringHashtbl.replace msg_borrows s (lt_tagged::borrows);
    intersects
  in
  let borrow_add_reg s lt_tagged range =
    let borrows = StringHashtbl.find_opt reg_borrows s |> Option.value ~default:[] in
    StringHashtbl.replace reg_borrows s ((lt_tagged, range)::borrows)
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
      | RegAssign (lval_info, td) ->
        (* `Cycles 0 rather than 1 because in the last cycle it is okay to update the register *)
        td_to_live_until := (td, (ev, `Cycles 0))::!td_to_live_until;
        let (range_st, _) = lval_info.lval_range.subreg_range_interval in
        (
          match range_st with
          | Const _ -> ()
          | NonConst range_st_td ->
            td_to_live_until := (range_st_td, (ev, `Cycles 0))::!td_to_live_until
        )
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
        let msg_d = lookup_message msg |> Option.get in
        let stype = List.hd msg_d.sig_types in
        let e_dpat = delay_pat_globalise msg.endpoint stype.lifetime.e |> delay_pat_reduce_cycles 1 in
        td_to_live_until := (td, (sa.d.until, e_dpat))::!td_to_live_until
      | Recv _ -> ()
    )
    g.events;
  List.iter (fun (td, dead) ->
    List.iter (fun borrow ->
      let lt_tagged = tag_with_span borrow.borrow_source_span {live = borrow.borrow_start; dead = [dead]} in
      borrow_add_reg borrow.borrow_range.subreg_name lt_tagged borrow.borrow_range
    ) td.reg_borrows
  ) !td_to_live_until;
  (* check register and message borrows *)
  visit_actions
    (fun ev a ->
      match a.d with
      | RegAssign (lval_info, td) ->
        let lt = EventGraphOps.lifetime_immediate ev in
        (
          match intersects_borrowed_reg lval_info.lval_range.subreg_name lt lval_info.lval_range with
          | [] -> ()
          | (lt_tagged, _)::_ ->
              let open Except in
              raise (LifetimeCheckError [
                Text "Attempted assignment to a borrowed register!";
                codespan_local a.span;
                Text "Borrowed at:";
                codespan_local lt_tagged.span
              ] )
        );
        if lifetime_in_range g.events lookup_message lt td.lt |> not then
          raise (LifetimeCheckError [
                    Text "Value does not live long enough in reg assignment!";
                    Except.codespan_local a.span
                ])
        else ();
        (
          match fst lval_info.lval_range.subreg_range_interval with
          | Const _ -> ()
          | NonConst range_st_td ->
            if lifetime_in_range g.events lookup_message lt range_st_td.lt |> not then
              raise (LifetimeCheckError [
                  Text "Lvalue index does not live long enough!";
                  Except.codespan_local a.span
                ])
        )
      | DebugPrint (_, tds) ->
        List.iter (fun td ->
          if lifetime_in_range g.events lookup_message (EventGraphOps.lifetime_immediate ev) td.lt |> not then
            raise (LifetimeCheckError
              [
                Text "Value does not live long enough in debug print!";
                Except.codespan_local a.span
              ])
          else ()
        ) tds
      | DebugFinish -> ()
      | PutShared (_, si, td) ->
        if lifetime_in_range g.events lookup_message {live = ev; dead = [(ev, si.value.glt.e)]} td.lt |> not then
          raise (LifetimeCheckError [
                  Text "Value does not live long enough in put!";
                  Except.codespan_local a.span
                ])
        else ()
    )
    (fun ev sa ->
      match sa.d.ty with
      | Send (msg, td) ->
        let msg_d = MessageCollection.lookup_message g.messages msg ci.channel_classes |> Option.get in
        let stype = List.hd msg_d.sig_types in
        let e_dpat = delay_pat_globalise msg.endpoint stype.lifetime.e in
        let lt = {live = ev; dead = [(sa.d.until, e_dpat)]} in
        (
          match intersects_borrowed_msg_and_add (string_of_msg_spec msg) (tag_with_span sa.span lt) with
          | lt_intersect_tagged::_ ->
            raise (LifetimeCheckError
              [
                Text "Potentially conflicting message sending!";
                Except.codespan_local sa.span;
                Text "Conflicting with:";
                Except.codespan_local lt_intersect_tagged.span
              ]
            )
          | [] -> ()
        );
        if lifetime_in_range g.events lookup_message lt td.lt |> not then
          raise (LifetimeCheckError [
            Text "Value not live long enough in message send!";
            Except.codespan_local sa.span
          ])
      | Recv _ -> ()
    )
    g.events;

  (* static sync pattern checks; we check the path between adjacent message send/recv *)
  (* collect all messages that require checking first *)
  (* First: find out all messages that require checks. Store inside msg_to_check,
    which maps an internal message identifier to the number of cycles in delay *)
  let msg_to_check = ref Utils.StringMap.empty in
  let get_msg_gap is_send msg =
    let msg_def = MessageCollection.lookup_message g.messages msg ci.channel_classes |> Option.get in
    let (sync_mode, other_sync_mode) = if is_send then
      (msg_def.send_sync, msg_def.recv_sync)
    else
      (msg_def.recv_sync, msg_def.send_sync) in
    (
      match sync_mode, other_sync_mode with
      | Static (o, n), Static _ -> Some (o, msg, n, true, true)
      | Static (o, n), Dynamic -> Some (o, msg, n, true, false)
      | Dynamic, Static (o, n) -> Some (o, msg, n, false, true)
      | Dependent (m, n), Dependent _ -> Some (0, {msg with msg = m}, n, true, true)
      | Dependent (m, n), Dynamic -> Some (0, {msg with msg = m}, n, true, false)
      | Dynamic, Dependent (m, n) -> Some (0, {msg with msg = m}, n, false, true)
      | _ -> None
    ) |> Option.map (
      function
      | (o, m, n, l, r) -> (o, msg_ident m, n, l, r)
    )

  in
  visit_actions
          (fun _ _ -> ())
          (fun _ev sa ->
            let (msg, msg_gap) = match sa.d.ty with
            | Send (msg, _) -> (msg, get_msg_gap true msg)
            | Recv msg -> (msg, get_msg_gap false msg)
            in
            match msg_gap with
            | None -> ()
            | Some n -> (
              msg_to_check := Utils.StringMap.add (msg_ident msg) n !msg_to_check
            )
          )
          g.events;
  (* Now we have all messages we need to check. *)
  if config.verbose then (
    Config.debug_println config "Messages requiring sync mode checks below:";
    Utils.StringMap.iter
      (fun m (o, relative_msg, n, self_check, other_check) ->
        Printf.sprintf "Message %s with init offset %d, gap %d, relative to %s(<=: %b, >=: %b)\n"
            m o n relative_msg self_check other_check
        |> Config.debug_println config)
    !msg_to_check
  );
  (* Check per message *)
  let check_msg_sync_mode msg (init_offset, relative_msg, gap, self_check, other_check) =
    (* if msg is an action at this event, obtain until *)
    let has_msg msg ev = List.find_map
      (fun sa ->
        match sa.d.ty with
        | Send (msg', _)
        | Recv msg' -> (
          let mi' = msg_ident msg' in
          if mi' = msg then
            Some sa
          else
            None
        )
      ) ev.sustained_actions in
    if self_check then (
      (* on the self side, check that adjacent events are no more than gap cycles apart,
         also check that the root to the first event is no more than init offset cycles apart
      *)
      (* check root *)
      let is_first = ref true in
      List.iter
        (fun ev ->
          match has_msg relative_msg ev with
          | Some sa ->
              if msg = relative_msg && !is_first then
                is_first := false (* skip the first msg (comes last) *)
              else (
                let slacks = GraphAnalysis.events_max_dist g.events lookup_message sa.d.until in
                (* mask out events that do not have the message *)
                List.iter (fun ev' ->
                  if has_msg msg ev' |> Option.is_none then
                    slacks.(ev'.id) <- GraphAnalysis.event_distance_max
                ) g.events;
                if config.verbose then (
                  Array.iteri (fun idx sl -> Printf.eprintf "Sl %d = %d\n" idx sl) slacks
                );
                let min_weights = GraphAnalysis.event_min_among_succ g.events slacks in
                if config.verbose then (
                  Array.iteri (fun idx sl -> Printf.eprintf "Mw %d = %d\n" idx sl) min_weights
                );
                if min_weights.(sa.d.until.id) > gap then (
                  let error_msg = Printf.sprintf "Static sync mode mismatch between %s and %s (actual gap = %d > expected gap %d)!"
                    relative_msg msg
                    min_weights.(sa.d.until.id) gap
                  in
                  raise (LifetimeCheckError [Text error_msg; Except.codespan_local sa.span])
                )
              )
          | None -> ()
        ) g.events;
      if msg = relative_msg then (
        let ev_root = (List.length g.events) - 1 |> List.nth g.events in
        assert(ev_root.source = (`Root None));
        let slacks = GraphAnalysis.events_max_dist g.events lookup_message ev_root in
        List.iter (fun ev' ->
          if has_msg relative_msg ev' |> Option.is_none then
            slacks.(ev'.id) <- GraphAnalysis.event_distance_max
        ) g.events;
        let min_weights = GraphAnalysis.event_min_among_succ g.events slacks in
        if min_weights.(ev_root.id) > init_offset then
          let error_msg = Printf.sprintf "Static sync mode mismatch (actual init offset = %d > expected init offset %d)!"
            min_weights.(ev_root.id) init_offset
          in
          raise (LifetimeCheckError [Text error_msg]) (* TODO: better error message *)
      )
    );
    if other_check then (
      (* if the other side is static, this side needs checking that the adjacent
      events are at least gap cycles apart; here we need to take into consideration
      the root event to ensure that the reference point is the beginning of a loop *)
      let has_msg_end msg ev =
        match ev.source with
        | `Seq (ev', `Send msg')
        | `Seq (ev', `Recv msg') ->
          if (msg_ident msg') = msg then
            Some ev'
          else
            None
        | _ -> None
      in
      let is_first = ref true in
      List.iter
        (fun ev ->
          match has_msg msg ev with
          | Some sa ->
            let slacks = GraphAnalysis.events_max_dist g.events lookup_message ev in
            if config.verbose then (
              Array.iteri (fun idx sl -> Printf.eprintf "Sl %d = %d\n" idx sl) slacks
            );
            List.iter (fun ev' ->
              if msg = relative_msg && !is_first && (ev'.source = `Root None) then
                slacks.(ev'.id) <- slacks.(ev'.id) + init_offset - gap
              else if has_msg_end relative_msg ev' |> Option.is_none then
                slacks.(ev'.id) <- -event_distance_max
            ) g.events;
            is_first := false;
            let maxv = GraphAnalysis.event_predecessors ev |> List.map (fun ev' -> slacks.(ev'.id))
              |> List.fold_left Int.max (-event_distance_max) in
            if maxv > -gap then
              let error_msg = Printf.sprintf "Static sync mode mismatch (actual gap = %d < expected gap %d)!"
                (-maxv) gap
              in
              raise (LifetimeCheckError [Text error_msg; Except.codespan_local sa.span])
          | None -> ()
        ) (List.rev g.events)
   )
  in
  Utils.StringMap.iter check_msg_sync_mode !msg_to_check;


