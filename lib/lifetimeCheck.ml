open Lang
open EventGraph
open GraphAnalysis

let print_control_set (g : event_graph) =
  List.iter (fun ev ->
    Printf.eprintf "=== Event %d ===\n" ev.id;
    Utils.StringMap.iter (fun endp r ->
      Printf.eprintf "Endpoint %s: %d, %d\n" endp (fst r) (snd r)
    ) ev.control_endps
  ) g.events

(** Generate the control set for each event in the event graph. The control set of event
includes actions that are synchronised at the event. *)
let gen_control_set (config : Config.compile_config) (g : event_graph) =
  let events_rev = List.rev g.events in
  let all_endpoints = g.messages.endpoints @ g.messages.args in
  let endp_use_cnt_empty =
      List.map (fun (e : endpoint_def) -> (e.name, 0)) all_endpoints
        |> List.to_seq |> Utils.StringMap.of_seq
  in
  let endp_use_cnt = ref endp_use_cnt_empty in
  let control_endps_init = Seq.repeat (0, Int.max_int) |> Seq.take (List.length all_endpoints)
        |> Seq.zip (List.map (fun (ep : endpoint_def) -> ep.name) all_endpoints |> List.to_seq)
        |> Utils.StringMap.of_seq in
  let reg_ops = ref [] in (* all uses of registers *)
  (* let _add_reg_ops_td ev td =
    List.iter (fun borrow -> reg_ops := (ev, borrow.borrow_range)::!reg_ops) td.reg_borrows
  in *)
  List.iter
    (fun (ev : event) ->
      ev.control_endps <- control_endps_init;
      ev.current_endps <- control_endps_init;
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
      (* List.iter (fun sa_span ->
        match sa_span.d.ty with
        | Send (_, td) ->
          add_reg_ops_td ev td
        | Recv _ -> ()
      ) ev.sustained_actions *)
    )
    events_rev;

  if config.verbose then (
    List.iter (fun (ev, range, _span) ->
      let range_s = match range.subreg_range_interval with
      | (Const n, sz) -> Printf.sprintf "%s[%d, %d]" range.subreg_name n sz
      | (NonConst _, sz) -> Printf.sprintf "%s[var, %d]" range.subreg_name sz
      in
      Printf.eprintf "RegAssign at %d to %s\n" ev.id range_s
    ) !reg_ops
  );

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
  List.iter
    (fun (ev : event) ->
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
        ev.control_endps <- Utils.StringMap.merge (fun _ a b -> opt_update_forward (Option.get a |> fst) b) ev'.control_endps ev.control_endps;
        ev.control_endps <- Utils.StringMap.merge (fun _ a b -> opt_update_forward (Option.get a |> fst) b) ev'.current_endps ev.control_endps
      in
      (
        match ev.source with
        | `Root None -> ()
        | `Later (e1, e2)
        | `Branch (_, {branch_val_true = Some e1; branch_val_false = Some e2; _}) ->
          (* Printf.eprintf "FR %d, %d -> %d\n" e1.id e2.id ev.id; *)
          forward_from e1;
          forward_from e2
        | `Seq (ev', _)  ->
          (* Printf.eprintf "FR %d -> %d\n" ev'.id ev.id; *)
          forward_from ev'
        | `Root (Some (ev', br_side_info)) ->
          (* to true side first *)
          if br_side_info.branch_side_sel then
            forward_from ev'
          else
            Option.get br_side_info.owner_branch.branch_val_true |> forward_from
        | _ ->
          raise (Except.UnknownError "Unexpected event source!")
      );
    )
    events_rev;
  (* backward pass *)
  List.iter
    (fun (ev : event) ->
      let backward_to (ev' : event) =
        ev'.control_endps <- Utils.StringMap.merge (fun _ a b -> opt_update_backward (Option.get a |> snd) b) ev.control_endps ev'.control_endps;
        ev'.control_endps <- Utils.StringMap.merge (fun _ a b -> opt_update_backward (Option.get a |> snd) b) ev.current_endps ev'.control_endps
      in
      (
        ev.control_endps <- Utils.StringMap.merge (fun _ a b -> opt_update_backward (let v = Option.get a in v + 1) b) !endp_use_cnt ev.control_endps;
        match ev.source with
        | `Root None -> ()
        | `Later (e1, e2) ->
          backward_to e1;
          backward_to e2
        | `Seq (ev', _) ->
          backward_to ev'
        | `Branch (_, {branch_val_true = Some _e1; branch_val_false = Some e2; _}) ->
          backward_to e2 (* to false side first *)
        | `Root (Some (ev', br_side_info)) ->
          if br_side_info.branch_side_sel then
            backward_to ev'
          else
            Option.get br_side_info.owner_branch.branch_val_true |> backward_to
        | _ ->
          raise (Except.UnknownError "Unexpected event source!")
      )
    ) g.events;
  (* print_control_set g;
  print_graph g; *)
  (* check for violations of linearity *)
  List.iter
    (fun (ev : event) ->
      List.iter (fun (sa : sustained_action ast_node) ->
        match sa.d.ty with
        | Send (ms, _) | Recv ms -> (
          if in_control_set_endps ev ms.endpoint |> not then
            raise (EventGraphError ("Non-linearizable endpoint use!", sa.span))
          else ()
        )
      ) ev.sustained_actions
    )
    g.events;
  (* check for violations of linearity for register assignments *)
  let rec check_reg_violation = function
    | (ev, range, span)::remaining ->
      List.iter (fun (ev', range', _span') ->
        if (EventGraphOps.subreg_ranges_possibly_intersect range range') &&
           (GraphAnalysis.events_are_ordered g.events ev ev' |> not) then
            raise (EventGraphError ("Non-linearizable register assignment!", span))
      ) remaining;
      check_reg_violation remaining
    | [] -> ()
  in
  check_reg_violation !reg_ops


module IntHashtbl = Hashtbl.Make(Int)

(** Is ev_pat1 matched always no later than ev_pat2 is matched?
Produce a conservative result.
Only length 1 ev_pat1 supported. *)
let event_pat_rel events (ev_pat1 : event_pat) (ev_pat2 : event_pat) =
  if (List.length ev_pat1) <> 1 then
    false
  else (
    let (ev1, d_pat1) = List.hd ev_pat1 in
    let check_f = match d_pat1 with
      | `Eternal -> fun _ev2 d_pat2 -> d_pat2 = `Eternal
      | `Cycles n1 ->
        (* compute the min possible cycle distance from n1 to successors *)
        let dist = event_min_distance events ev1 ev1 in
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
                let slacks = event_slack_graph events ev2 in
                let d = slacks.(ev1.id) in
                n1 + d <= n2
              )
            | `Message msg ->
                let n2_opt' = event_succ_msg_match_earliest ev2 msg false in
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
          let ri1_opt = event_succ_msg_match_latest ev1 msg true in (* latest estimated *)
          match ri1_opt with
          | None -> fun _ _ -> true (* End of the second loop. Don't handle *)
          | Some ri1 ->
            fun ev2 d_pat2 -> (
              match d_pat2 with
              | `Eternal -> true
              | `Cycles n2 -> (* earliest estimate *)
                if event_is_predecessor ev2 ri1 then true
                else (
                  let slacks = event_slack_graph events ev2 in
                  let d = slacks.(ri1.id) in
                  d <= n2
                )
              | `Message msg2 ->
                (
                  let le2_opt = event_succ_msg_match_earliest ev2 msg2 false in
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
let lifetime_in_range events (lt1 : lifetime) (lt2 : lifetime) =
  (* 1. check if lt2's start is a predecessor of lt1's start *)
  (* 2. derive a set of all time points A potentially within lt1 *)
  (* 4. check that end time of lt2 does not match any time point in A *)
  (* (event_is_successor lt2.live lt1.live) && (
    let r = event_pat_matches lt1.live lt2.dead in
    (not r.at) && (not r.aft)
  ) *)
  (event_pat_rel events [(lt2.live, `Cycles 0)] [(lt1.live, `Cycles 0)])
    && (event_pat_rel events lt1.dead lt2.dead)

(** Definitely disjoint? *)
let lifetime_disjoint events lt1 lt2 =
  let separated_branches ev1 ev2 =
    (event_is_successor ev1 ev2 |> not)
    && (event_is_successor ev2 ev1 |> not)
  in
  assert (List.length lt1.dead = 1);
  (* to be disjoint, either r1 <= l2 or r2 <= l1 *)
  (event_pat_rel events lt1.dead [(lt2.live, `Cycles 0)])
  || (List.for_all (fun de -> event_pat_rel events [de] [(lt1.live, `Cycles 0)]) lt2.dead)
  || ((separated_branches lt1.live (List.hd lt2.dead |> fst))
      && (separated_branches lt2.live (List.hd lt1.dead |> fst)))


(* An internal identifier for a message specifier. *)
let msg_ident msg = Printf.sprintf "%s@%s" msg.endpoint msg.msg

module StringHashtbl = Hashtbl.Make(String)

let lifetime_check (config : Config.compile_config) (ci : cunit_info) (g : event_graph) =
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
    raise (LifetimeCheckError "Thread must take at least one cycle to complete a loop!");

  gen_control_set config g;
  (* for debugging purposes*)
  if config.verbose then (
    print_control_set g
  );
  let reg_borrows = StringHashtbl.create 8 in (* regname -> (lifetime, range)*)
  let msg_borrows = StringHashtbl.create 8 in
  (* check lifetime for each use of a wire *)
  let not_borrowed_reg s lt range =
    let borrows = StringHashtbl.find_opt reg_borrows s |> Option.value ~default:[] in
    List.for_all (fun (lt', range') ->
      (EventGraphOps.subreg_ranges_possibly_intersect range range' |> not) || (lifetime_disjoint g.events lt lt'))
      borrows
  in
  let not_borrowed_msg_and_add s lt =
    let borrows = StringHashtbl.find_opt msg_borrows s |> Option.value ~default:[] in
    let not_borrowed = List.for_all (lifetime_disjoint g.events lt) borrows in
    if not_borrowed then StringHashtbl.replace msg_borrows s (lt::borrows);
    not_borrowed
  in
  let borrow_add_reg s lt range =
    let borrows = StringHashtbl.find_opt reg_borrows s |> Option.value ~default:[] in
    StringHashtbl.replace reg_borrows s ((lt, range)::borrows)
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
        let msg_d = MessageCollection.lookup_message g.messages msg ci.channel_classes |> Option.get in
        let stype = List.hd msg_d.sig_types in
        let e_dpat = delay_pat_globalise msg.endpoint stype.lifetime.e |> delay_pat_reduce_cycles 1 in
        td_to_live_until := (td, (sa.d.until, e_dpat))::!td_to_live_until
      | Recv _ -> ()
    )
    g.events;
  List.iter (fun (td, dead) ->
    List.iter (fun borrow ->
      borrow_add_reg borrow.borrow_range.subreg_name {live = borrow.borrow_start; dead = [dead]} borrow.borrow_range
    ) td.reg_borrows
  ) !td_to_live_until;
  (* check register and message borrows *)
  visit_actions
    (fun ev a ->
      match a.d with
      | RegAssign (lval_info, td) ->
        let lt = EventGraphOps.lifetime_immediate ev in
        if not_borrowed_reg lval_info.lval_range.subreg_name lt lval_info.lval_range |> not then
          raise (EventGraphError ("Attempted assignment to a borrowed register!", a.span))
        else ();
        if lifetime_in_range g.events lt td.lt |> not then
          raise (EventGraphError ("Value does not live long enough in reg assignment!", a.span))
        else ();
        (
          match fst lval_info.lval_range.subreg_range_interval with
          | Const _ -> ()
          | NonConst range_st_td ->
            if lifetime_in_range g.events lt range_st_td.lt |> not then
              raise (EventGraphError ("Lvalue index does not live long enough!", a.span))
        )
      | DebugPrint (_, tds) ->
        List.iter (fun td ->
          if lifetime_in_range g.events (EventGraphOps.lifetime_immediate ev) td.lt |> not then
            raise (EventGraphError ("Value does not live long enough in debug print!", a.span))
          else ()
        ) tds
      | DebugFinish -> ()
      | PutShared (_, si, td) ->
        if lifetime_in_range g.events {live = ev; dead = [(ev, si.value.glt.e)]} td.lt |> not then
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
        if not_borrowed_msg_and_add (string_of_msg_spec msg) lt |> not then
          raise (EventGraphError ("Potentially conflicting message sending!", sa.span))
        else ();
        if lifetime_in_range g.events lt td.lt |> not then
          raise (EventGraphError ("Value not live long enough in message send!", sa.span))
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
    match sync_mode, other_sync_mode with
    | Dependent (`Cycles n), Dependent (`Cycles _) -> Some (n, true, true)
    | Dependent (`Cycles n), Dynamic -> Some (n, true, false)
    | Dynamic, Dependent (`Cycles n) -> Some (n, false, true)
    | _ -> None
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
      (fun m (n, self_check, other_check) -> Printf.sprintf "Message %s with gap %d (<=: %b, >=: %b)\n" m n self_check other_check
        |> Config.debug_println config)
    !msg_to_check
  );
  (* Check per message *)
  let check_msg_sync_mode msg (gap, self_check, other_check) =
    (* if msg is an action at this event, obtain until *)
    let has_msg ev = List.find_map
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
      let is_first = ref true in
      List.iter
        (fun ev ->
          match has_msg ev with
          | Some sa ->
              if !is_first then
                is_first := false (* skip the first msg (comes last) *)
              else (
                let slacks = GraphAnalysis.event_slack_graph g.events sa.d.until in
                (* mask out events that do not have the message *)
                List.iter (fun ev' ->
                  if has_msg ev' |> Option.is_none then
                    slacks.(ev'.id) <- GraphAnalysis.event_distance_max
                ) g.events;
                if config.verbose then (
                  Array.iteri (fun idx sl -> Printf.eprintf "Sl %d = %d\n" idx sl) slacks
                );
                let min_weights = GraphAnalysis.event_min_among_succ g.events slacks in
                if config.verbose then (
                  Array.iteri (fun idx sl -> Printf.eprintf "Mw %d = %d\n" idx sl) min_weights
                );
                if min_weights.(sa.d.until.id) > gap then
                  let error_msg = Printf.sprintf "Static sync mode mismatch (actual gap = %d > expected gap %d)!"
                    min_weights.(sa.d.until.id) gap
                  in
                  raise (EventGraphError (error_msg, sa.span))
              )
          | None -> ()
        ) g.events
    );
    if other_check then (
      let has_msg_end ev =
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
          match has_msg ev with
          | Some sa ->
            if !is_first then
              is_first := false
            else (
              let slacks = GraphAnalysis.event_slack_graph g.events ev in
              if config.verbose then (
                Array.iteri (fun idx sl -> Printf.eprintf "Sl %d = %d\n" idx sl) slacks
              );
              List.iter (fun ev' ->
                if has_msg_end ev' |> Option.is_none then
                  slacks.(ev'.id) <- -event_distance_max
              ) g.events;
              let maxv = GraphAnalysis.event_predecessors ev |> List.map (fun ev' -> slacks.(ev'.id))
                |> List.fold_left Int.max (-event_distance_max) in
              if maxv > -gap then
                let error_msg = Printf.sprintf "Static sync mode mismatch (actual gap = %d < expected gap %d)!"
                  (-maxv) gap
                in
                raise (EventGraphError (error_msg, sa.span))
            )
          | None -> ()
        ) (List.rev g.events)
    )
  in
  Utils.StringMap.iter check_msg_sync_mode !msg_to_check;


