open Lang
open EventGraph

(* Simplifies combinational logic. Removes redundant nodes.
  This includes:
    1) merging events belonging to branches that take 0 cycles
    2) merging events known to take 0 cycles (e.g., send/recv with static/dependent sync modes)
*)
module CombSimplPass = struct
    let ( .!() ) = Dynarray.get
    let ( .!()<- ) = Dynarray.set

  (* Creates a new union-find set. *)
  let create_ufs n = Dynarray.init n (fun i -> i)
  let rec find_ufs ufs i =
    let f = ufs.!(i) in
    if f == i then
      i
    else (
      let f = find_ufs ufs f in
      ufs.!(i) <- f;
      f
    )

  (* This always merges hi into lo. *)
  let union_ufs ufs hi lo =
    let f_hi = find_ufs ufs hi in
    ufs.!(f_hi) <- lo

  let merge_pass_simpl_comb _config for_lt_check (ci : cunit_info) (graph : event_graph) event_ufs event_arr_old =
    List.rev graph.events
    |> List.iter (fun ev ->
      match ev.source with
      | `Branch (ev', {branches_val; _}) -> (
        let can_merge =
          List.for_all (fun e ->
            let e = Dynarray.get event_arr_old @@ find_ufs event_ufs e.id in
            match e.source with
            | `Root (Some (ep, br_side)) ->
              ep.id = ev'.id && (Option.get br_side.branch_event).id = ev.id
                && e.actions = [] && e.sustained_actions = []
            | _ -> false
          )
          branches_val in
        if can_merge then (
          List.iter (fun e ->
            let e = Dynarray.get event_arr_old @@ find_ufs event_ufs e.id in
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

  let merge_pass_isomorphic_branch _config for_lt_check (_ci : cunit_info) (graph : event_graph) event_ufs event_arr_old =
    assert (not for_lt_check); (* this cannot be enabled for lifetime checks *)
    let n = List.length graph.events in
    assert (Dynarray.length event_ufs = n);
    let add_event source =
      let new_ev = {actions = []; sustained_actions = []; source; id = graph.last_event_id + 1;
        is_recurse = false;
        outs = []; graph; preds = Utils.IntSet.empty } in
      graph.last_event_id <- new_ev.id;
      graph.events <- new_ev::graph.events;
      Dynarray.add_last event_ufs new_ev.id;
      Dynarray.add_last event_arr_old new_ev;
      assert (Dynarray.length event_ufs = new_ev.id + 1);
      new_ev
    in
    let find_event e =
      let f = find_ufs event_ufs e.id in
      event_arr_old.!(f)
    in
    let rec merge_branch ev ev_r br_info =
      let branches_val = List.map find_event br_info.branches_val in
      let ev_v = List.hd branches_val in
      let ev_l = List.tl branches_val in
      (* need to insert extra nodes *)
      match ev_v.source with
      | `Seq (_, `Cycles n) ->
        if ev_v.actions = [] && ev_v.sustained_actions = [] &&
          (* check that all are #n *)
          List.for_all (fun ev' ->
            match ev'.source with
            | `Seq (_, `Cycles m) -> n = m && ev'.sustained_actions = [] && ev'.actions = []
            | _ -> false
          ) ev_l then
        (
          (* create a new event *)
          let new_ev_branches_val = List.map
            (fun e ->
              match e.source with
              | `Seq (e', _) -> e'
              | _ -> failwith "Something went wrong!"
            )
            branches_val in
          let new_ev_br_info = {br_info with branches_val = new_ev_branches_val} in
          let new_ev_src = `Branch (ev_r, new_ev_br_info) in
          let new_ev_branch = add_event new_ev_src in

          let new_ev_delay = add_event (`Seq (new_ev_branch, `Cycles n)) in

          (* merge nodes *)
          List.iter (fun e -> union_ufs event_ufs e.id new_ev_delay.id) branches_val;
          union_ufs event_ufs ev.id new_ev_delay.id;
          merge_branch new_ev_branch ev_r new_ev_br_info
        )
      | `Root (Some (e_p, _)) ->
        let e_p = find_event e_p in
        if List.for_all (fun ev' ->
            match ev'.source with
            | `Root (Some (e_p', _)) -> e_p.id = (find_event e_p').id
            | _ -> false
          ) ev_l then
          union_ufs event_ufs ev.id e_p.id
      | _ -> ()
    in
    List.rev graph.events
    |> List.iter (fun ev ->
      match ev.source with
      | `Branch (ev_r, br_info) ->
        merge_branch ev ev_r br_info
      | _ -> ()
    )

  let merge_pass_branch_fuse _config for_lt_check (_ci : cunit_info) (graph : event_graph) event_ufs event_arr_old =
    assert (not for_lt_check); (* can only be used for codegen *)
    List.iter (fun ev ->
      match ev.source with
      | `Branch (_, br_info) -> (
        List.iter (fun e ->
          match e.source with
          | `Branch _ -> (
            if e.actions = [] && e.sustained_actions = [] then
              union_ufs event_ufs e.id ev.id
          )
          | _ -> ()
        ) br_info.branches_val
      )
      | _ -> ()
    ) graph.events;
    List.iter (fun ev ->
      match ev.source with
      | `Branch (_, br_info) -> (
        let f = find_ufs event_ufs ev.id in
        let new_br_val = List.filter_map (fun e ->
          let f_p = find_ufs event_ufs e.id in
          let p = event_arr_old.!(f_p) in
          if f_p = f then
            None
          else
            Some p
        ) br_info.branches_val in
        if f = ev.id then
          br_info.branches_val <- new_br_val
        else (
          (* need to merge *)
          match event_arr_old.!(f).source with
          | `Branch (_, br_info') ->
            br_info'.branches_val <- new_br_val @ br_info'.branches_val
          | _ -> failwith "Something went wrong!"
        )
      )
      | _ -> ()
    ) graph.events

  let merge_pass_triangle_fuse _config for_lt_check (_ci : cunit_info) (graph : event_graph) event_ufs event_arr_old =
    assert (not for_lt_check);
    List.rev graph.events
    |> List.iter (fun ev ->
      match ev.source with
      | `Later (e1, e2) ->
        let e1 = event_arr_old.!(find_ufs event_ufs e1.id) in
        let e2 = event_arr_old.!(find_ufs event_ufs e2.id) in
        if List.exists (fun e -> e.id = e1.id) @@ GraphAnalysis.imm_preds e2 then
          union_ufs event_ufs ev.id e2.id
        else if List.exists (fun e -> e.id = e2.id) @@ GraphAnalysis.imm_preds e1 then
          union_ufs event_ufs ev.id e1.id
      | _ -> ()
    )

  let merge_pass_unbalanced_later _config for_lt_check (ci : cunit_info) (graph : event_graph) event_ufs event_arr_old =
    assert (not for_lt_check);
    List.rev graph.events
    |> List.iter (fun ev ->
      match ev.source with
      | `Later (e1, e2) ->
        let e1 = event_arr_old.!(find_ufs event_ufs e1.id) in
        let e2 = event_arr_old.!(find_ufs event_ufs e2.id) in
        let lookup_message msg = MessageCollection.lookup_message graph.messages msg ci.channel_classes in
        let order = GraphAnalysis.events_get_order graph.events lookup_message e1 e2 in
        (
          match order with
          | BeforeEq | Before | AlwaysEq ->
            union_ufs event_ufs ev.id e2.id
          | AfterEq | After ->
            union_ufs event_ufs ev.id e1.id
          | Unreachable ->
            failwith "Something went wrong!"
          | _ -> ()
        )
      | _ -> ()
    )

  let optimize_pass_merge merge_pass config for_lt_check (ci : EventGraph.cunit_info) graph =
    let graph = {graph with thread_id = graph.thread_id} in (* create a copy *)
    let event_ufs = create_ufs @@ List.length graph.events in

    let event_arr_old = List.rev graph.events |> Dynarray.of_list in
    merge_pass config for_lt_check ci graph event_ufs event_arr_old;

    let event_n = List.length graph.events in (* note that new events might be created in merge_pass *)
    let to_keep = Array.init event_n (fun i -> find_ufs event_ufs i = i) in
    (* Array.iteri (fun idx k -> Printf.eprintf "Keep %d = %b\n" idx k) to_keep; *)

    (* let events_to_keep_old = GraphAnalysis.toposort graph.events |> List.filter (fun e -> to_keep.(e.id)) in *)
    let event_arr_old = List.rev graph.events |> Array.of_list in
    let events_to_keep_old =
      List.filter (fun e -> to_keep.(e.id)) graph.events
      |> GraphAnalysis.toposort_with_preds
        (fun e -> GraphAnalysis.imm_preds e |> List.map (fun e -> event_arr_old.(find_ufs event_ufs e.id)))
    in
    (* replace events that will be kept *)
    List.iteri
      (fun idx e ->
        assert (event_arr_old.(e.id).id = e.id);
        event_arr_old.(e.id) <- {e with id = idx}
      )
      events_to_keep_old;
    let events_to_keep = List.map (fun e -> event_arr_old.(e.id)) events_to_keep_old in

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
      assert to_keep.(f);
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

    let merge_event old_id ev =
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
        ev_f.sustained_actions <- Utils.list_unordered_join ev_f.sustained_actions sustained_actions;
        ev_f.is_recurse <- ev_f.is_recurse || ev.is_recurse (* maintain recurse event *)
      )
    in
    List.iter2 (fun e_new e_old -> assert to_keep.(e_old.id); merge_event e_old.id e_new) events_to_keep events_to_keep_old; (* merge events to keep first *)
    Array.iteri (fun i e -> if not to_keep.(i) then merge_event i e) event_arr_old; (* then merge the rest *)

    List.iter (fun ev ->
      List.iter (fun sa_span ->
        assert (ev.id <> sa_span.d.until.id)
      ) ev.sustained_actions;
      List.iter (fun e ->
        assert (e.id < ev.id)
      ) @@ GraphAnalysis.imm_preds ev
    ) events_to_keep;

    {graph with
      events = List.rev events_to_keep;
      last_event_id = (List.length events_to_keep) - 1
    }

  let optimize_pass config for_lt_check ci graph =
    let codegen_only merge_pass graph =
      if for_lt_check then
        graph
      else
        optimize_pass_merge merge_pass config for_lt_check ci graph
    in
    optimize_pass_merge merge_pass_simpl_comb config for_lt_check ci graph
    |> optimize_pass_merge merge_pass_isomorphic config for_lt_check ci
    |> optimize_pass_merge merge_pass_joint config for_lt_check ci
    |> codegen_only merge_pass_isomorphic_branch
    |> codegen_only merge_pass_isomorphic
    |> codegen_only merge_pass_joint
    |> codegen_only merge_pass_branch_fuse
    |> codegen_only merge_pass_triangle_fuse
    |> codegen_only merge_pass_unbalanced_later
end

let optimize config for_lt_check ci graph =
  CombSimplPass.optimize_pass config for_lt_check ci graph
