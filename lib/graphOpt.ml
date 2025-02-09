open EventGraph

(* Simplifies combinational logic. Removes redundant nodes. *)
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

  let optimize_pass _config _for_lt_check _ci graph =
    let event_n = List.length graph.events in
    let event_ufs = create_ufs event_n in
    (* scan all events in reverse order to find simplifable patterns *)
    let event_arr_old = List.rev graph.events |> Array.of_list in
    let check_pattern ev =
      match ev.source with
      | `Branch (ev', br_info) -> (
        let ev1 = Option.get br_info.branch_val_true
        and ev2 = Option.get br_info.branch_val_false in
        let ev1 = event_arr_old.(find_ufs event_ufs ev1.id) in
        let ev2 = event_arr_old.(find_ufs event_ufs ev2.id) in
        match ev1.source, ev2.source with
        | `Root (Some (ev_1', br_side_1)), `Root (Some (ev_2', br_side_2)) ->
          let br_ev1 = Option.get br_side_1.branch_event
          and br_ev2 = Option.get br_side_2.branch_event in
          if br_ev1.id = br_ev2.id &&
             ev_1'.id = ev'.id &&
             ev_2'.id = ev'.id &&
            ev1.actions = [] && ev2.actions = [] then (
            (* we can merge the nodes ev, ev1, ev2 with ev1' *)
            union_ufs event_ufs ev.id ev'.id;
            union_ufs event_ufs ev1.id ev'.id;
            union_ufs event_ufs ev2.id ev'.id
          )
        | _ -> ()
      )
      | _ -> ()
    in
    List.rev graph.events |> List.iter check_pattern;
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
      let actions = List.map (fun (action: action Lang.ast_node) ->
        let d = match action.d with
        | DebugPrint (s, td_list) -> DebugPrint (s, List.map replace_timed_data td_list)
        | RegAssign (lval_info, td) -> RegAssign (replace_lvalue_info lval_info, replace_timed_data td)
        | PutShared (s, svi, td) -> PutShared (s, svi, replace_timed_data td)
        | DebugFinish -> DebugFinish
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
          branch_cond = replace_timed_data br_info.branch_cond;
          branch_to_true = Option.map replace_event br_info.branch_to_true;
          branch_to_false = Option.map replace_event br_info.branch_to_false;
          branch_val_true = Option.map replace_event br_info.branch_val_true;
          branch_val_false = Option.map replace_event br_info.branch_val_false;
        }
      in
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
        let f = find_ufs event_ufs old_id in
        assert (f != old_id);
        let ev_f = event_arr_old.(f) in
        ev_f.actions <- ev_f.actions @ actions;
        ev_f.sustained_actions <- ev_f.sustained_actions @ sustained_actions
      )
    in
    Array.iteri merge_event event_arr_old;
    {graph with
      events = !event_list_new;
      last_event_id = event_arr_old.(find_ufs event_ufs graph.last_event_id).id
    }
end

let optimize config for_lt_check ci graph =
  CombSimplPass.optimize_pass config for_lt_check ci graph
