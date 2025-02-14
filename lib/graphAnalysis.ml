open Lang
open EventGraph

(* TODO: to use arrays instead of hash tables *)

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

let event_predecessors (ev : event) : event list =
  let visitor add_to_queue cur =
    match cur.source with
    | `Later (e1, e2)
    | `Branch (_, {branch_val_true = Some e1; branch_val_false = Some e2; _}) ->
      add_to_queue e1;
      add_to_queue e2
    | `Seq (ev', _)
    | `Root (Some (ev', _)) ->
      add_to_queue ev'
    | `Root None -> ()
    | _ -> ()
  in
  event_traverse ev visitor

let event_successors (ev : event) : event list =
  let visitor add_to_queue cur =
    List.iter add_to_queue cur.outs
  in
  event_traverse ev visitor |> List.rev

let events_pred events ev =
  let n = List.length events in
  let preds = event_predecessors ev in
  let event_is_pred = Array.make n false in
  List.iter (fun e -> event_is_pred.(e.id) <- true) preds;
  (preds, event_is_pred)


let event_is_successor (ev : event) (ev' : event) =
  event_successors ev |> List.exists (fun x -> x.id = ev'.id)

let event_is_predecessor (ev : event) (ev' : event) =
  event_predecessors ev |> List.exists (fun x -> x.id = ev'.id)


let in_control_set (cur : (int * int) Utils.string_map) (cnt : (int * int) Utils.string_map) (s : identifier) =
  let (pre, post) = Utils.StringMap.find s cnt in
  if Utils.StringMap.find s cur |> fst = 0 then
    pre + 1 = post
  else
    pre + 2 = post

let in_control_set_endps (ev : event) = in_control_set ev.current_endps ev.control_endps

(** evs must be in topological order*)
let find_controller (endpts : identifier list) =
  List.find_opt
    (fun ev ->
      List.for_all (fun endpt -> in_control_set_endps ev endpt) endpts
    )

(* inclusive: include the matching event itself or its predecessor? *)
let find_first_msg_after (ev : event) (msg: Lang.message_specifier) inclusive =
  event_successors ev |> List.find_map
    (fun ev' ->
      List.find_map (fun sa ->
        match sa.d.ty with
        | Send (msg', _) | Recv msg' ->
            if msg' = msg then Some (ev', sa) else None
      ) ev'.sustained_actions
      |> Option.map (fun (ev', sa) -> if inclusive then sa.d.until else ev')
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
let event_succ_distance non_succ_dist msg_dist_f either_dist_f events (ev : event) (cur: event) =
  let preds = event_predecessors ev in
  let preds_cur = event_predecessors cur in
  let succs = event_successors cur in
  let dist = IntHashtbl.create 8 in
  IntHashtbl.add dist ev.id 0;
  let get_dist ev' = IntHashtbl.find_opt dist ev'.id |> Option.value ~default:non_succ_dist in
  let set_dist ev' d = IntHashtbl.add dist ev'.id d in
  List.iter (fun ev' -> set_dist ev' 0) preds; (* predecessors must have been reached *)
  List.rev events |> List.iter (fun ev' ->
    if IntHashtbl.find_opt dist ev'.id |> Option.is_none then
    let d = match ev'.source with
    | `Root None -> raise (Except.unknown_error_default "Unexpected root!")
    | `Later (ev1, ev2) -> max (get_dist ev1) (get_dist ev2)
    | `Seq (ev1, ad) ->
      let d1 = get_dist ev1 in
      (
        match ad with
        | `Cycles n' -> d1 + n'
        | `Send _ | `Recv _ | `Sync _ -> msg_dist_f d1
      )
    | `Root (Some (ev1, _)) ->
      (* We need to check carefully to decide if we are sure
        the branch has/hasn't been taken. *)
      if (List.mem ev' succs) || (List.mem ev' preds_cur) then
        get_dist ev1
      else
        non_succ_dist
    | `Branch (_, {branch_val_true = Some ev1; branch_val_false = Some ev2; _}) ->
      either_dist_f (get_dist ev1) (get_dist ev2)
    | _ ->
      raise (Except.unknown_error_default "Unexpected event source!")
    in
    set_dist ev' (min d event_distance_max)
  );
  dist


(* Compute the slack: the max distance achievable to every event while keeping
  the distance to ev unchanged. FIXME: branching might be problematic *)
let event_slack_graph events ev =
  let n = List.length events in
  let root = List.nth events (n - 1) in
  let dist = event_succ_distance event_distance_max (fun d -> d) min events root root in
  (* IntHashtbl.iter (fun ev_id d -> Printf.eprintf "Dist %d = %d\n" ev_id d) dist; *)
  (* Now compute max dist allowed *)
  let max_dists = Array.make n event_distance_max in
  max_dists.(ev.id) <- IntHashtbl.find dist ev.id;
  List.iter (fun ev' ->
    let d = max_dists.(ev'.id) in
    if d < event_distance_max then (
      let update_dist ev_pred d =
        let d' = max_dists.(ev_pred.id) in
        if d' > d then
          max_dists.(ev_pred.id) <- d
      in
      match ev'.source with
      | `Root None -> ()
      | `Later (e1, e2)
      | `Branch (_, {branch_val_true = Some e1; branch_val_false = Some e2; _}) ->
          update_dist e1 d;
          update_dist e2 d
      | `Seq (e', `Cycles cyc) ->
        update_dist e' (d - cyc)
      | `Root (Some (e', br_side_info)) ->
        if br_side_info.branch_side_sel then (
          let e_false = Option.get br_side_info.owner_branch.branch_to_false in
          let d = Int.max d max_dists.(e_false.id) in
          update_dist e' d
        )
      | `Seq (e', _) ->
        update_dist e' d
      | _ ->
        raise (Except.unknown_error_default "Unexpected event source!")
    )
  ) events;
  (* Array.iteri (fun ev_id d -> Printf.eprintf "Max dist %d = %d\n" ev_id d) max_dists; *)
  (* let slacks = Array.mapi (fun idx md -> md - (IntHashtbl.find dist idx)) max_dists in *)
  (* Array.iteri (fun ev_id sl -> Printf.eprintf "Slack %d = %d\n" ev_id sl) slacks; *)
  (* Get the slack graph. This slack is the max distance achievable through adjusting
  message delays without affecting the distance to ev. *)
  let slacks = Array.make n 0 in
  List.rev events |> List.iter (fun ev' ->
    let d = match ev'.source with
      | `Root None -> 0
      | `Later (e1, e2)
      | `Branch (_, {branch_val_true = Some e1; branch_val_false = Some e2; _}) ->
        Int.max slacks.(e1.id) slacks.(e2.id)
      | `Seq (e', `Cycles cyc) ->
        slacks.(e'.id) + cyc
      | `Root (Some (e', _)) ->
        slacks.(e'.id)
      | `Seq (_e', _) ->
        max_dists.(ev'.id)
      | _ ->
        raise (Except.unknown_error_default "Unexpected event source!")
    in
    slacks.(ev'.id) <- d
  );
  (* Array.iteri (fun ev_id sl -> Printf.eprintf "Slack %d = %d\n" ev_id sl) slacks; *)
  let d = IntHashtbl.find dist ev.id in
  Array.map_inplace (fun sl -> sl - d) slacks;
  slacks

let event_min_among_succ events weights =
  let n = List.length events in
  assert (n = (Array.length weights));
  let res = Array.copy weights in
  List.iter (fun ev' ->
    let update_res e' v =
      let v' = res.(e'.id) in
      if v < v' then
        res.(e'.id) <- v
    in
    (* handle forward branches *)
    let (branch_v, has_branch) = List.fold_left (fun (mx, has) ev_succ ->
      match ev_succ.source with
      | `Root (Some _) ->
        (Int.max mx res.(ev_succ.id), true)
      | _ ->
        (mx, has)
    ) (0, false) ev'.outs in
    if has_branch then (
      update_res ev' branch_v
    );
    let v = res.(ev'.id) in
    match ev'.source with
    | `Root None -> ()
    | `Later (e1, e2)
    | `Branch (_, {branch_val_true = Some e1; branch_val_false = Some e2; _}) ->
      update_res e1 v;
      update_res e2 v
    | `Seq (e, _)
    | `Root (Some (e, _)) ->
      update_res e v
    | `Branch (_e, _) -> ()
      (* FIXME: this handling is rough; we handle this at
      forward edges instead *)
  ) events;
  res

let event_min_distance =
  event_succ_distance event_distance_max (fun d -> d) min

let event_max_distance =
  event_succ_distance event_distance_max (fun _ -> event_distance_max) max

let events_are_ordered events e1 e2 =
  if e1.id = e2.id then
    false
  else (
    let preds1 = event_predecessors e1 in
    let preds2 = event_predecessors e2 in
    let n = List.length events in
    let is_e2_preds = Array.make n false in
    List.iter (fun e -> is_e2_preds.(e.id) <- true) preds2;
    let common_pred = List.rev preds1 |> List.find (fun e -> is_e2_preds.(e.id)) in
    if common_pred.id = e1.id || common_pred.id = e2.id then
      true
    else (
      let find_cond =
        List.find_opt (fun e ->
              match e.source with
              | `Root (Some (pred, _)) -> pred.id = common_pred.id
              | _ -> false
        )
      in
      let e1_cond_pred = find_cond preds1 in
      let e2_cond_pred = find_cond preds2 in
      match e1_cond_pred, e2_cond_pred with
      | Some _, Some _ -> true
      | _ -> false
    )
  )

let events_visit_backward visitor = List.iter visitor
let events_visit_forward visitor events = List.rev events |> List.iter visitor

let events_prepare_outs events =
  List.iter (fun ev -> ev.outs <- []) events;
  List.iter (fun ev ->
    match ev.source with
    | `Later (e1, e2) ->
      e1.outs <- ev::e1.outs;
      e2.outs <- ev::e2.outs
    | `Branch (_ev', br_info) ->
      let e1 = Option.get br_info.branch_val_true
      and e2 = Option.get br_info.branch_val_false in
      e1.outs <- ev::e1.outs;
      e2.outs <- ev::e2.outs
    | `Seq (ev', _)
    | `Root (Some (ev', _)) ->
      ev'.outs <- ev::ev'.outs
    | `Root None -> ()
  ) events

let event_is_dominant e1 e2 =
  let preds = (event_predecessors e1) @ [e1] in
  (* max id + 1 *)
  let n = (List.fold_left (fun i e -> Int.max i e.id) 0 preds) + 1 in
  let is_dominated = Array.make n false in
  List.iter (fun e ->
    let d =
      if e.id = e2.id then
        true
      else (
        match e.source with
        | `Root None -> false
        | `Later (e1, e2) ->
          is_dominated.(e1.id) || is_dominated.(e2.id)
        | `Seq (e', _) ->
          is_dominated.(e'.id)
        | `Branch (_, {branch_val_true = Some e1; branch_val_false = Some e2; _}) ->
          is_dominated.(e1.id) && is_dominated.(e2.id)
        | `Root (Some (e', _)) ->
          is_dominated.(e'.id)
        | _ -> raise (Except.unknown_error_default "Unexpected event source!")
      )
    in
    is_dominated.(e.id) <- d
  ) preds;
  is_dominated.(e1.id)

let events_pred_min_dist ev =
  let preds = (event_predecessors ev) @ [ev] in
  let n = (List.fold_left (fun i e -> Int.max i e.id) 0 preds) + 1 in
  let is_preds = Array.make n false in
  let check_is_pred e = e.id < n && is_preds.(e.id) in
  List.iter (fun e -> is_preds.(e.id) <- true) preds;
  let res = Array.make n (-1) in
  let seen_branches = ref Utils.IntSet.empty in
  res.(ev.id) <- 0;
  let update_dist e v =
    if res.(e.id) < v then
      res.(e.id) <- v
  in
  List.rev preds
    |> List.iter (fun e ->
      let v = res.(e.id) in
      match e.source with
      | `Seq (e', d) -> (
        let gap =
          match d with
          | `Cycles n -> n
          | `Send _ | `Recv _
          | `Sync _ -> 0
        in
        update_dist e' (v + gap)
      )
      | `Later (e1, e2)
      | `Branch (_, {branch_val_true = Some e1; branch_val_false = Some e2; _}) -> (
        update_dist e1 v;
        update_dist e2 v
      )
      | `Root None -> ()
      | `Root (Some (e', {branch_event = Some br_ev; owner_branch; branch_side_sel})) -> (
        if Utils.IntSet.mem br_ev.id !seen_branches then (
          (* only pass when the both sides have been reached *)
          let e1 = Option.get owner_branch.branch_to_true
          and e2 = Option.get owner_branch.branch_to_false in
          let v1 = res.(e1.id) in
          let v2 = res.(e2.id) in
          update_dist e' (Int.min v1 v2)
        ) else (
          (* the other side of the branch may not be a predecessor, in which
          case we just pass through the result as there is no real branching *)
          let other =
            (if branch_side_sel then owner_branch.branch_to_false else owner_branch.branch_to_true)
            |> Option.get in
          if check_is_pred other then
            seen_branches := Utils.IntSet.add br_ev.id !seen_branches
          else
            update_dist e' v
        )
      )
      | _ -> raise (Except.unknown_error_default "Unexpected event source!")
    );
  res

let events_reachable events ev =
  let n = List.length events in
  let event_is_reachable = Array.make n true in
  let (preds, is_pred) = events_pred events ev in
  let preds = preds @ [ev] in
  List.iter (fun e ->
      match e.source with
      | `Root (Some (_, br_side_info)) ->
        (* invalidate the other side of the branch if it's not a predecessor *)
        let other_side = EventGraphOps.branch_other_side br_side_info in
        if not is_pred.(other_side.id) then
          event_is_reachable.(other_side.id) <- false
      | _ -> ()
    ) preds;
  (* now propagate *)
  List.rev events
  |> List.iter (fun e ->
      if event_is_reachable.(e.id) then (
        let reachable =
          match e.source with
          | `Root _ -> true
          | `Later (e1, e2) -> event_is_reachable.(e1.id) && event_is_reachable.(e2.id)
          | `Seq (e', _) -> event_is_reachable.(e'.id)
          | `Branch (_, {branch_val_true = Some e1; branch_val_false = Some e2; _}) ->
            event_is_reachable.(e1.id) || event_is_reachable.(e2.id)
          | _ -> raise (Except.unknown_error_default "Unexpected event source!")
        in
        event_is_reachable.(e.id) <- reachable
      )
    );
  event_is_reachable

let events_max_dist events ev =
  let n = List.length events in
  let res = Array.make n (-event_distance_max) in
  let pred_min_dist = events_pred_min_dist ev in
  let pred_min_dist_mxn = Array.length pred_min_dist in
  let is_pred e' = e'.id < pred_min_dist_mxn && pred_min_dist.(e'.id) >= 0 in
  let reachable = events_reachable events ev in
  let update_dist e v =
    if res.(e.id) < v then
      res.(e.id) <- v
  in
  List.rev events
    |> List.iter (fun e ->
      let v =
        if is_pred e then -pred_min_dist.(e.id)
        else if not reachable.(e.id) then -event_distance_max
        else (
          (* propagate *)
          match e.source with
          | `Root None -> -event_distance_max
          | `Seq (e', d) -> (
            let gap =
              match d with
              | `Cycles n -> n
              | `Send _ | `Recv _ | `Sync _ -> event_distance_max (* oo *)
            in
            res.(e'.id) + gap
          )
          | `Later (e1, e2)
          | `Branch (_, {branch_val_true = Some e1; branch_val_false = Some e2; _}) ->
            Int.max (res.(e1.id)) (res.(e2.id))
          | `Root (Some (e', _)) ->
            res.(e'.id)
          | _ -> raise (Except.unknown_error_default "Unexpected event source!")
        )
      in
      update_dist e v
    );
  res

let events_with_msg events msg =
  List.filter (fun e ->
      match e.source with
      | `Seq (_, `Send msg') | `Seq (_, `Recv msg') ->
        msg' = msg
      | _ -> false
    ) events

let events_first_msg events ev msg =
  let n = List.length events in
  let path_has_msg = Array.make n false in
  let event_has_msg = Array.make n false in
  events_with_msg events msg
    |> List.iter (fun e -> event_has_msg.(e.id) <- true);
  let succs = event_successors ev in
  let event_is_succ = Array.make n false in
  List.iter (fun e -> event_is_succ.(e.id) <- true) succs;
  let preds = event_predecessors ev in
  let event_is_pred = Array.make n false in
  List.iter (fun e -> event_is_pred.(e.id) <- true) preds;
  let event_succ_masked_has_msg e = ev.id <> e.id && event_is_succ.(e.id) && event_has_msg.(e.id) in
  assert (event_succ_masked_has_msg ev |> not);
  let reachable = events_reachable events ev in
  List.iter (fun e ->
      let v =
        match e.source with
        | `Root None -> false
        | `Later (e1, e2) ->
            path_has_msg.(e1.id) || path_has_msg.(e2.id)
              || (event_succ_masked_has_msg e1) || (event_succ_masked_has_msg e2)
        | `Root (Some (e', _))
        | `Seq (e', _) -> path_has_msg.(e'.id) || (event_succ_masked_has_msg e')
        | `Branch (_, {branch_val_true = Some e1; branch_val_false = Some e2; _}) ->
          if not reachable.(e1.id) then
            path_has_msg.(e2.id) || event_succ_masked_has_msg e2
          else if not reachable.(e2.id) then
            path_has_msg.(e1.id) || event_succ_masked_has_msg e1
          else
            (path_has_msg.(e1.id) || event_succ_masked_has_msg e1)
              && (path_has_msg.(e2.id) || event_succ_masked_has_msg e2)
        | _ -> raise (Except.unknown_error_default "Unexpected event source!")
      in
      path_has_msg.(e.id) <- v
    ) succs;
  (* now handle those that are neither predecessor nor successor but remove those in other sides of branches *)
  (* reachable non-predecessor or first successor *)
  List.filter (fun e ->
      event_has_msg.(e.id) && e.id <> ev.id && (
        (not event_is_pred.(e.id) && not event_is_succ.(e.id) && reachable.(e.id)) ||
        (event_is_succ.(e.id) && not path_has_msg.(e.id))
      )
    ) events
