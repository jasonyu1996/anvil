open Lang
open EventGraph

(* Simplifies combinational logic. Removes redundant nodes.
  This includes:
    1) merging events belonging to branches that take 0 cycles
    2) merging events known to take 0 cycles (e.g., send/recv with static/dependent sync modes)
*)
module CombSimplPass = struct
  (* Creates a new union-find set. *)
  let create_ufs n = Array.init n (fun i -> i)
  let rec find_ufs ufs i =
    let f = ufs.(i) in
    if f == i then
      i
    else (
      let f = find_ufs ufs f in
      ufs.(i) <- f;
      f
    )

  (* This always merges hi into lo. *)
  let union_ufs ufs hi lo =
    let f_hi = find_ufs ufs hi in
    ufs.(f_hi) <- lo

  let merge_pass_simpl_comb _config for_lt_check (ci : cunit_info) (graph : event_graph) event_ufs event_arr_old =
    List.rev graph.events
    |> List.iter (fun ev ->
      match ev.source with
      | `Branch (ev', {branches_val; _}) -> (
        let can_merge =
          List.for_all (fun e ->
            let e = event_arr_old.(find_ufs event_ufs e.id) in
            match e.source with
            | `Root (Some (ep, br_side)) ->
              ep.id = ev'.id && (Option.get br_side.branch_event).id = ev.id
                && e.actions = [] && e.sustained_actions = []
            | _ -> false
          )
          branches_val in
        if can_merge then (
          List.iter (fun e ->
            let e = event_arr_old.(find_ufs event_ufs e.id) in
            union_ufs event_ufs e.id ev'.id;
          ) branches_val;
          union_ufs event_ufs ev.id ev'.id
        )
      )
      | `Seq (ev', (`Send msg as delay)) | `Seq (ev', (`Recv msg as delay)) ->
        (* check if this msg takes any bit of time *)
        if not for_lt_check then (
          let msg_def = MessageCollection.lookup_message graph.messages msg ci.channel_classes |> Option.get in
          let immediate =
            (
              match delay with
              | `Send _ -> true
              | `Recv _ -> false
            ) |> GraphAnalysis.message_is_immediate msg_def
          in
          if immediate then (
            union_ufs event_ufs ev.id ev'.id
          )
        )
      | _ -> ()
    )

  module IntMap = Map.Make(Int)
  let merge_pass_isomorphic _config _for_lt_check (_ci : cunit_info) (graph : event_graph) event_ufs _event_arr_old =
    let n = List.length graph.events in
    let merged_edges = Array.make n IntMap.empty in
    let lookup_edge ev n = (* look an edge from ev to n *)
      let ev'_id = find_ufs event_ufs ev.id in
      IntMap.find_opt n merged_edges.(ev'_id)
    in
    let add_edge ev n cur_ev =
      let ev'_id = find_ufs event_ufs ev.id in
      merged_edges.(ev'_id) <- IntMap.add n cur_ev.id merged_edges.(ev'_id)
    in
    List.rev graph.events
    |> List.iter (fun ev ->
      match ev.source with
      | `Seq (ev', `Cycles n) -> (
        match lookup_edge ev' n with
        | None -> add_edge ev' n ev
        | Some ev_m_id ->
          (* merge *)
          union_ufs event_ufs ev.id ev_m_id
      )
      | _ -> ()
    )

  (* merge later to the same nodes *)
  let merge_pass_joint _config _for_lt_check (_ci : cunit_info) (graph : event_graph) event_ufs _event_arr_old =
    List.rev graph.events
    |> List.iter (fun ev ->
      match ev.source with
      | `Later (e1, e2) ->
        let e1'_id = find_ufs event_ufs e1.id
        and e2'_id = find_ufs event_ufs e2.id in
        if e1'_id = e2'_id then
          union_ufs event_ufs ev.id e1'_id
      | _ -> ()
    )

  let optimize_pass_merge merge_pass config for_lt_check (ci : EventGraph.cunit_info) graph =
    let event_n = List.length graph.events in
    let event_ufs = create_ufs event_n in
    (* scan all events in reverse order to find simplifable patterns *)
    let event_arr_old = List.rev graph.events |> Array.of_list in
    merge_pass config for_lt_check ci graph event_ufs event_arr_old;
    let to_keep = Array.init event_n (fun i -> find_ufs event_ufs i = i) in
    (* Array.iteri (fun idx k -> Printf.eprintf "Keep %d = %b\n" idx k) to_keep; *)
    (* replace events *)
    let event_list_new = ref [] in
    let event_arr_old = List.rev graph.events |> List.mapi
      (fun i ev ->
        if to_keep.(i) then
          let new_id = List.length !event_list_new in
          let new_ev = {ev with id = new_id} in
          event_list_new := new_ev::!event_list_new;
          new_ev
        else
          ev
      ) |> Array.of_list in
    assert (event_arr_old.(0).id = 0);
    let merge_event old_id ev =
      let rec replace_event_pat evp =
        List.map (fun (ev, dp) -> (replace_event ev, dp)) evp
      and replace_lifetime lt =
        {
          live = replace_event lt.live;
          dead = replace_event_pat lt.dead;
        }
      and replace_timed_data td =
        {td with
          lt = replace_lifetime td.lt;
          reg_borrows = List.map (fun borrow ->
            {borrow with borrow_start = replace_event borrow.borrow_start}
          ) td.reg_borrows;
        }
      and replace_event ev =
        let f = find_ufs event_ufs ev.id in
        event_arr_old.(f)
      in
      let replace_lvalue_info lval_info =
        let range_fst = match fst lval_info.lval_range.subreg_range_interval with
        | MaybeConst.NonConst td -> MaybeConst.NonConst (replace_timed_data td)
        | _ -> fst lval_info.lval_range.subreg_range_interval in
        let range = (range_fst, snd lval_info.lval_range.subreg_range_interval) in
        {lval_info with lval_range = {lval_info.lval_range with subreg_range_interval = range}}
      in
      let replace_sa_type = function
        | Send (msg, td) -> Send (msg, replace_timed_data td)
        | Recv msg -> Recv msg
      in
      let replace_branch_cond = function
        | TrueFalse -> TrueFalse
        | MatchCases cases ->
          MatchCases (List.map replace_timed_data cases)
      in
      let actions = List.map (fun (action: action Lang.ast_node) ->
        let d = match action.d with
        | DebugPrint (s, td_list) -> DebugPrint (s, List.map replace_timed_data td_list)
        | RegAssign (lval_info, td) -> RegAssign (replace_lvalue_info lval_info, replace_timed_data td)
        | PutShared (s, svi, td) -> PutShared (s, svi, replace_timed_data td)
        | DebugFinish -> DebugFinish
        | ImmediateRecv msg -> ImmediateRecv msg
        | ImmediateSend (msg, td) -> ImmediateSend (msg, replace_timed_data td)
        in
        {action with d}
      ) ev.actions in
      let sustained_actions = List.map (fun (sa : sustained_action Lang.ast_node) ->
        let d = {
          until = replace_event sa.d.until;
          ty = replace_sa_type sa.d.ty;
        } in
        {sa with d}
      ) ev.sustained_actions in
      let replace_branch_info br_info =
        {
          branch_cond_v = replace_timed_data br_info.branch_cond_v;
          branch_cond = replace_branch_cond br_info.branch_cond;
          branch_count = br_info.branch_count;
          branches_to = List.map replace_event br_info.branches_to;
          branches_val = List.map replace_event br_info.branches_val;
        }
      in
      let f = find_ufs event_ufs old_id in
      let (immediate_sa, sustained_actions) =
        List.partition (fun sa_span -> sa_span.d.until.id = event_arr_old.(f).id) sustained_actions in
      let actions = (
        List.map (fun sa_span ->
          match sa_span.d.ty with
          | Send (msg, td) -> { d = ImmediateSend (msg, td); span = sa_span.span }
          | Recv msg -> { d = ImmediateRecv msg; span = sa_span.span }
        ) immediate_sa
      ) @ actions in
      if to_keep.(old_id) then (
        (* we just need to replace things *)
        ev.actions <- actions;
        ev.sustained_actions <- sustained_actions;
        let source' = match ev.source with
        | `Root None -> `Root None
        | `Root (Some (ev', br_side_info)) ->
          `Root (Some (
            replace_event ev',
            {br_side_info with
              branch_event = Option.map replace_event br_side_info.branch_event;
              owner_branch = replace_branch_info br_side_info.owner_branch;
            }
          ))
        | `Later (ev1, ev2) -> `Later (replace_event ev1, replace_event ev2)
        | `Seq (ev', d) -> `Seq (replace_event ev', d)
        | `Branch (ev', br_info) ->
          `Branch (replace_event ev', replace_branch_info br_info)
        in
        ev.source <- source'
      ) else (
        assert (f != old_id);
        let ev_f = event_arr_old.(f) in
        ev_f.actions <- Utils.list_unordered_join ev_f.actions actions;
        ev_f.sustained_actions <- Utils.list_unordered_join ev_f.sustained_actions sustained_actions
      )
    in
    Array.iteri merge_event event_arr_old;
    List.iter (fun ev ->
      List.iter (fun sa_span ->
        assert (ev.id <> sa_span.d.until.id)
      ) ev.sustained_actions
    ) !event_list_new;
    {graph with
      events = !event_list_new;
      last_event_id = event_arr_old.(find_ufs event_ufs graph.last_event_id).id
    }

  let optimize_pass config for_lt_check ci graph =
    optimize_pass_merge merge_pass_simpl_comb config for_lt_check ci graph
    |> optimize_pass_merge merge_pass_isomorphic config for_lt_check ci
    |> optimize_pass_merge merge_pass_joint config for_lt_check ci
end

let optimize config for_lt_check ci graph =
  CombSimplPass.optimize_pass config for_lt_check ci graph
