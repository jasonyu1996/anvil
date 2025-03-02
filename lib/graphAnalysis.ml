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

(* returns immediate predecessors *)
let imm_preds ev =
  match ev.source with
  | `Seq (e', _)
  | `Root (Some (e', _)) -> [e']
  | `Branch (_, {branch_val_true = Some e1; branch_val_false = Some e2; _})
  | `Later (e1, e2) -> [e1; e2]
  | `Root None -> []
  | _ ->
      raise (Except.unknown_error_default "Unexpected event source!")

let toposort events =
  let n = List.fold_left (fun l e -> Int.max l e.id) 0 events in
  let n = n + 1 in
  let in_subgraph = Array.make n false in
  let outdegs = Array.make n 0 in
  let is_in_subgraph e' =
    e'.id < n && in_subgraph.(e'.id)
  in
  List.iter (fun e -> in_subgraph.(e.id) <- true) events;
  List.iter (fun e ->
    imm_preds e |>
    List.iter (fun e' ->
      if is_in_subgraph e' then
        outdegs.(e'.id) <- outdegs.(e'.id) + 1
    )
  ) events;
  let q = List.filter (fun e -> outdegs.(e.id) = 0) events |> List.to_seq |> Queue.of_seq in
  let res = ref [] in
  while Queue.is_empty q |> not do
    let e = Queue.pop q in
    res := e::!res;
    imm_preds e |> List.iter (fun e' ->
      if is_in_subgraph e' then (
        outdegs.(e'.id) <- outdegs.(e'.id) - 1;
        if outdegs.(e'.id) = 0 then
          Queue.add e' q
      )
    )
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
  event_traverse ev visitor |> toposort

let event_successors (ev : event) : event list =
  let visitor add_to_queue cur =
    List.iter add_to_queue cur.outs
  in
  event_traverse ev visitor |> toposort

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


module IntHashtbl = Hashtbl.Make(Int)
let event_distance_max = 1 lsl 20
let event_succ_distance non_succ_dist msg_dist_f later_dist_f either_dist_f events (ev : event) (cur: event) =
  assert (cur.id = ev.id); (* not supporting other cases *)
  let preds = event_predecessors ev in
  let dist = IntHashtbl.create 8 in
  IntHashtbl.add dist ev.id 0;
  let get_dist ev' = IntHashtbl.find_opt dist ev'.id |> Option.value ~default:non_succ_dist in
  let set_dist ev' d = IntHashtbl.add dist ev'.id d in
  List.iter (fun ev' -> set_dist ev' 0) preds; (* predecessors must have been reached *)
  List.rev events |> List.iter (fun ev' ->
    if IntHashtbl.find_opt dist ev'.id |> Option.is_none then
    let d = match ev'.source with
    | `Root None -> raise (Except.unknown_error_default "Unexpected root!")
    | `Later (ev1, ev2) -> later_dist_f (get_dist ev1) (get_dist ev2)
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
        get_dist ev1
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
  let dist = event_succ_distance event_distance_max (fun d -> d) max min events root root in
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
    let v = res.(ev'.id) in
    match ev'.source with
    | `Root None -> ()
    | `Later (e1, e2)
    | `Branch (_, {branch_val_true = Some e1; branch_val_false = Some e2; _}) ->
      update_res e1 v;
      update_res e2 v
    | `Seq (e, _) ->
      update_res e v
    | `Root (Some (e, br_side)) ->
      (* false side is visited first *)
      if br_side.branch_side_sel then (
        let other_side = Option.get br_side.owner_branch.branch_to_false in
        Int.max res.(other_side.id) v |> update_res e
      )
      (* we handle this at forward edges instead *)
    | `Branch _ -> ()
  ) events;
  res

let event_min_distance =
  event_succ_distance event_distance_max (fun d -> d) min min

let event_min_distance_with_later =
  event_succ_distance event_distance_max (fun d -> d) max min

let event_max_distance =
  event_succ_distance event_distance_max (fun _ -> event_distance_max) max max


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

let events_max_dist events lookup_message ev =
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
              | `Send m  ->
                let msg_def = lookup_message m |> Option.get in
                (
                  match msg_def.recv_sync with
                  | Static _ | Dependent _ -> 0
                  | _ -> event_distance_max
                )
              | `Recv m ->
                let msg_def = lookup_message m |> Option.get in
                (
                  match msg_def.send_sync with
                  | Static _ | Dependent _ -> 0
                  | _ -> event_distance_max
                )
              | `Sync _ -> event_distance_max (* oo *)
            in
            if res.(e'.id) = -event_distance_max then
              res.(e'.id)
            else
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

let event_is_msg_end msg e =
  List.exists (fun ac_span ->
    match ac_span.d with
    | ImmediateSend (msg', _)
    | ImmediateRecv msg' ->
      msg' = msg
    | _ -> false
  ) e.actions
  ||
  match e.source with
  | `Seq (_, `Send msg') | `Seq (_, `Recv msg') ->
    msg' = msg
  | _ -> false


let events_with_msg events msg = List.filter (event_is_msg_end msg) events

let events_start_msg events msg =
  List.filter (fun e ->
      List.exists (fun sa ->
        match sa.d.ty with
        | Send (msg', _) | Recv msg' -> msg' = msg
      ) e.sustained_actions
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


type events_order =
| Before
| After
| BeforeEq
| AfterEq
| AlwaysEq
| Unordered
| Unreachable

let is_strict_ordered = function
  | Before | After | Unreachable -> true
  | _ -> false

let events_get_order events lookup_message e1 e2 =
  let to_sign v =
    if v = -event_distance_max then -2
    else if v < 0 then -1
    else if v > 0 then 1
    else 0
  in
  let mx_dist1 = (events_max_dist events lookup_message e1).(e2.id) |> to_sign in
  let mx_dist2 = (events_max_dist events lookup_message e2).(e1.id) |> to_sign in
  match mx_dist1, mx_dist2 with
  | -2, _ | _, -2 -> Unreachable
  | 0, 0 -> AlwaysEq
  | 1, 1 | -1, -1 -> Unordered
  | -1, 0 | 0, 1 -> AfterEq
  | -1, 1 -> After
  | 1, 0 | 0, -1 -> BeforeEq
  | 1, -1 -> Before
  | _ -> Unreachable

let graph_owned_regs g =
  let res = ref [] in
  List.iter (fun e ->
    List.iter (fun ac_span ->
      match ac_span.d with
      | RegAssign (lval, _td) ->
        res := lval.lval_range.subreg_name::!res
      | _ -> ()
    ) e.actions
  ) g.events;
  !res

let message_is_immediate msg is_send =
  (is_send && msg.recv_sync <> Dynamic) || (not is_send && msg.send_sync <> Dynamic)
