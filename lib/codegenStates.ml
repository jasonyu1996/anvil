module EventStateFormatter = struct
  let format_reg_state state_name thread_id ev_id =
    Printf.sprintf "_thread_%d_event_%s_%d" thread_id state_name ev_id

  (* these following are to be suffixed with _q and _n *)
  let format_counter = format_reg_state "counter"
  let format_scorer = format_reg_state "reg"
  let format_syncstate = format_reg_state "syncstate"

  let format_current thread_id ev_id =
    Printf.sprintf "EVENTS%d[%d].event_current" thread_id ev_id
end

let collect_reg_states g =
  let open EventGraph in
  List.concat_map (fun e ->
    match e.source with
    | `Seq (_, d) -> (
      match d with
      | `Cycles n ->
        let width = Utils.int_log2 (n + 1) in
        let sn = EventStateFormatter.format_counter g.thread_id e.id in
        [(Printf.sprintf "logic[%d:0]" (width - 1), sn)]
      | `Send _ | `Recv _ | `Sync _ ->
        let sn = EventStateFormatter.format_syncstate g.thread_id e.id in
        [("logic", sn)]
    )
    | `Later _ ->
      (* this state needs to distinguish two sources *)
      let sn = EventStateFormatter.format_scorer g.thread_id e.id in
      [("logic[1:0]", sn)]
    | `Branch _ | `Either _ | `Root -> []
  ) g.events

(* prints out declarations for event-related wires and registers *)
let codegen_decl printer (g : EventGraph.event_graph) =
  let st_count = List.length g.events in
  (* `current` is a wire indicates if an event is reached in the current cycle *)
  [
    Printf.sprintf "for (genvar i = 0; i < %d; i ++) begin : EVENTS%d" st_count g.thread_id;
    "logic event_current;";
    "end"
  ] |> CodegenPrinter.print_lines printer;
  (* print per-event states *)
  collect_reg_states g
  |> List.iter (fun (ty, sn) ->
    Printf.sprintf "%s %s_q, %s_n;" ty sn sn |> CodegenPrinter.print_line printer
  )

(* generate the next-cycle signals*)
let codegen_next printer (pg : EventGraph.proc_graph) (g : EventGraph.event_graph) =
  let print_line = CodegenPrinter.print_line printer in
  let print_lines = CodegenPrinter.print_lines printer in
  let print_compute_next (e : EventGraph.event) =
    let cn = EventStateFormatter.format_current g.thread_id e.id in
    match e.source with
    | `Seq (e', d) ->
      (
        let cn' = EventStateFormatter.format_current g.thread_id e'.id in
        match d with
        | `Cycles n when n >= 1 ->
          let width = Utils.int_log2 (n + 1) in
          let sn = EventStateFormatter.format_counter g.thread_id e.id in
          (* reached when the counter *)
          [
            Printf.sprintf "assign %s = %s_q == %d'd%d;" cn sn width n;
            Printf.sprintf "assign %s_n = %s ? %d'd1 : %s ? '0 : %s_q ? (%s_q + %d'd1) : %s_q;" sn cn' width cn sn sn width sn
          ] |> print_lines
        | `Cycles _ ->
          failwith "Invalid number of cycles"
        | `Send msg ->
          let sn = EventStateFormatter.format_syncstate g.thread_id e.id in
          let ack_n = CodegenFormat.format_msg_ack_signal_name (CodegenFormat.canonicalize_endpoint_name msg.endpoint g) msg.msg in
          [
            Printf.sprintf "assign %s = (%s || %s_q) && %s;" cn cn' sn ack_n;
            Printf.sprintf "assign %s_n = (%s || %s_q) && !%s;" sn cn' sn ack_n
          ] |> print_lines
        | `Recv msg ->
          let sn = EventStateFormatter.format_syncstate g.thread_id e.id in
          let vld_n = CodegenFormat.format_msg_valid_signal_name (CodegenFormat.canonicalize_endpoint_name msg.endpoint g) msg.msg in
          [
            Printf.sprintf "assign %s = (%s || %s_q) && %s;" cn cn' sn vld_n;
            Printf.sprintf "assign %s_n = (%s || %s_q) && !%s;" sn cn' sn vld_n
          ] |> print_lines
        | `Sync s ->
          let si = Hashtbl.find pg.shared_vars_info s in
          let assigned_at = Option.get si.assigned_at in
          let sn = EventStateFormatter.format_syncstate g.thread_id e.id in
          let wn = EventStateFormatter.format_current assigned_at.graph.thread_id assigned_at.id in
          [
            Printf.sprintf "assign %s = (%s || %s_q) && %s;" cn cn' sn wn;
            Printf.sprintf "assign %s_n = (%s || %s_q) && !%s;" sn cn' sn wn
          ] |> print_lines
      )
    | `Either (e1, e2) ->
      Printf.sprintf "assign %s = %s || %s;" cn
        (EventStateFormatter.format_current g.thread_id e1.id)
        (EventStateFormatter.format_current g.thread_id e2.id)
      |> print_line
    | `Later (e1, e2) ->
      let cn1 = EventStateFormatter.format_current g.thread_id e1.id in
      let cn2 = EventStateFormatter.format_current g.thread_id e2.id in
      let sn = EventStateFormatter.format_scorer g.thread_id e.id in
      [
        (* {cn1, cn2} kept in state *)
        Printf.sprintf "assign %s = (%s & %s_q[0]) | (%s & %s_q[1]) | (%s & %s);" cn cn1 sn cn2 sn cn1 cn2;
        Printf.sprintf "assign %s_n = %s_q ^ {%s, %s} ^ {%s, %s};" sn sn cn1 cn2 cn cn
      ] |> print_lines
    | `Root ->
      [
        Printf.sprintf "assign %s = _init || %s;"
          cn
          (EventStateFormatter.format_current g.thread_id g.last_event_id)
      ] |> print_lines
    | `Branch (cond, e') ->
      let cn' = EventStateFormatter.format_current g.thread_id e'.id in
      let w = Option.get cond.data.w in
      let wn = CodegenFormat.format_wirename w.thread_id w.id in
      print_line (if cond.neg then
        Printf.sprintf "assign %s = %s && !%s;" cn cn' wn
      else
        Printf.sprintf "assign %s = %s && %s;" cn cn' wn)
  in
  List.iter print_compute_next g.events

let codegen_actions printer (g : EventGraph.event_graph) =
  let print_line ?(lvl_delta_pre = 0) ?(lvl_delta_post = 0) =
    CodegenPrinter.print_line printer ~lvl_delta_pre:lvl_delta_pre ~lvl_delta_post:lvl_delta_post in
  let open EventGraph in
  let print_event_actions (e : event) =
    if e.actions <> [] then begin
      EventStateFormatter.format_current g.thread_id e.id
      |> Printf.sprintf "if (%s) begin" |> print_line ~lvl_delta_post:1;
      let print_action (a : action Lang.ast_node) =
        match a.d with
        | DebugPrint (s, tds) ->
          Printf.sprintf "$display(\"%s\"%s);"
            s
            (List.map (fun (td : timed_data) ->
              let w = Option.get td.w in
              Printf.sprintf ", %s" @@ CodegenFormat.format_wirename w.thread_id w.id) tds |>
            String.concat "") |> print_line
        | DebugFinish ->
          print_line "$finish;"
        | RegAssign (lval_info, td) ->
          let (le, len) = lval_info.range in
          let w = Option.get td.w in
          Printf.sprintf "%s[%s +: %d] <= %s;"
            (CodegenFormat.format_regname_current lval_info.reg_name)
            (MaybeConst.map (fun td -> Option.get td.w) le |> CodegenFormat.format_wire_maybe_const)
            len
            (CodegenFormat.format_wirename w.thread_id w.id)
            |> print_line
        | PutShared _ -> ()
      in
      List.iter print_action e.actions;
      print_line ~lvl_delta_pre:(-1) "end"
    end else ()
  in
  List.iter print_event_actions g.events


let codegen_transition printer (_graphs : EventGraph.event_graph_collection) (g : EventGraph.event_graph) =
  let print_line ?(lvl_delta_pre = 0) ?(lvl_delta_post = 0) =
    CodegenPrinter.print_line printer ~lvl_delta_pre:lvl_delta_pre ~lvl_delta_post:lvl_delta_post in
  let reg_states = collect_reg_states g in
  let print_reset_states () =
    List.iter (fun (_, sn) -> Printf.sprintf "%s_q <= '0;" sn |> print_line) reg_states;
    print_line "_init <= '1;"
  in
  (* reset states *)
  Printf.sprintf "always_ff @(posedge clk_i or negedge rst_ni) begin : _thread_%d_st_transition" g.thread_id |> print_line ~lvl_delta_post:1;
  print_line ~lvl_delta_post:1 "if (~rst_ni) begin";
  print_reset_states ();
  (* register reset *)
  List.iter (
    fun (r : Lang.reg_def) ->
      let open CodegenFormat in
      Printf.sprintf "%s <= '0;" (format_regname_current r.name) |> print_line
  ) g.regs;
  print_line ~lvl_delta_pre:(-1) ~lvl_delta_post:1 "end else begin";
  (* actions *)
  codegen_actions printer g;
  (* next states *)
  List.iter (fun (_, sn) -> Printf.sprintf "%s_q <= %s_n;" sn sn |> print_line) reg_states;
  print_line "_init <= '0;";
  print_line ~lvl_delta_pre:(-1) "end";
  print_line ~lvl_delta_pre:(-1) "end"

let codegen_sustained_actions printer (g : EventGraph.event_graph) =
  let print_line = CodegenPrinter.print_line printer in
  let send_or_assigns = Hashtbl.create 8 in
  let recv_or_assigns = Hashtbl.create 8 in
  let insert_to tbl w_name cond =
    (
      match Hashtbl.find_opt tbl w_name with
      | None -> Hashtbl.add tbl w_name (ref [cond])
      | Some li -> li := cond::!li
    )
  in
  let open Lang in
  let print_sa_event (e : EventGraph.event) =
    List.iter (fun (sa : EventGraph.sustained_action Lang.ast_node) ->
      let activated = Printf.sprintf "(%s || %s_q)"
        (EventStateFormatter.format_current g.thread_id e.id)
        (EventStateFormatter.format_syncstate g.thread_id sa.d.until.id) in
      match sa.d.ty with
      | Send (msg, td) ->
        let w = Option.get td.w in
        insert_to send_or_assigns
          (CodegenFormat.format_msg_valid_signal_name (CodegenFormat.canonicalize_endpoint_name msg.endpoint g) msg.msg)
          (
            activated,
            CodegenFormat.format_msg_data_signal_name (CodegenFormat.canonicalize_endpoint_name msg.endpoint g) msg.msg 0,
            CodegenFormat.format_wirename w.thread_id w.id
          )
      | Recv msg ->
        insert_to recv_or_assigns
          (CodegenFormat.format_msg_ack_signal_name (CodegenFormat.canonicalize_endpoint_name msg.endpoint g) msg.msg)
          activated
    ) e.sustained_actions
  in
  (* assuming reverse topo order, the assign lists will be in topo order *)
  List.iter print_sa_event g.events;
  Hashtbl.iter (fun w_name conds ->
    String.concat " || " !conds
    |> Printf.sprintf "assign %s = %s;" w_name
    |> print_line
  ) recv_or_assigns;

  let send_data_selectors = ref [] in
  Hashtbl.iter (fun w_name conds_dw ->
    List.map (fun (c, _, _) -> c) !conds_dw
    |> String.concat " || "
    |> Printf.sprintf "assign %s = %s;" w_name
    |> print_line;
    let n = List.length !conds_dw in
    if n <= 1 then (
      assert (n = 1);
      let (_, dw, vw) = List.hd !conds_dw in
      Printf.sprintf "assign %s = %s;" dw vw
      |> print_line
    ) else (
      (* if we have more than one site that sends, keep track of which event is the last send (TODO: optimisation) *)
      let width = Utils.int_log2 n in
      send_data_selectors := (width, w_name, !conds_dw)::!send_data_selectors;
      let (_, dw, _) = List.hd !conds_dw in
      List.mapi (fun site_idx (_, _, vw) ->
        Printf.sprintf "(%s_selector_n == %d'd%d) ? %s : " w_name width site_idx vw
      ) !conds_dw
      |> String.concat ""
      |> Printf.sprintf "assign %s = %s'0;" dw
      |> print_line
    )
  ) send_or_assigns;

  if !send_data_selectors <> [] then (
    (* generate declarations for selectors *)
    List.iter (fun (width, name, _) ->
      Printf.sprintf "logic[%d:0] %s_selector_q, %s_selector_n;" (width - 1) name name
      |> print_line
    ) !send_data_selectors;

    Printf.sprintf "always_comb begin: _thread_%d_selector" g.thread_id
    |> CodegenPrinter.print_line ~lvl_delta_post:1 printer;
    List.iter (fun (width, name, conds) ->
      Printf.sprintf "%s_selector_n = %s_selector_q;" name name |> print_line;
      List.iteri (fun site_idx (c, _, _) ->
        Printf.sprintf "if (%s) %s_selector_n = %d'd%d;" c name width site_idx
          |> print_line
      ) conds;
    ) !send_data_selectors;
    CodegenPrinter.print_line ~lvl_delta_pre:(-1) printer "end";

    Printf.sprintf "always_ff @(posedge clk_i or negedge rst_ni) begin : _thread_%d_selector_trans" g.thread_id
    |> CodegenPrinter.print_line ~lvl_delta_post:1 printer;

    CodegenPrinter.print_line ~lvl_delta_post:1 printer "if (~rst_ni) begin";
    List.iter (fun (_, name, _) ->
      Printf.sprintf "%s_selector_q <= '0;" name
      |> print_line
    ) !send_data_selectors;
    CodegenPrinter.print_line ~lvl_delta_pre:(-1) ~lvl_delta_post:1 printer "end else begin";
    List.iter (fun (_, name, _) ->
      Printf.sprintf "%s_selector_q <= %s_selector_n;" name name
      |> print_line
    ) !send_data_selectors;
    CodegenPrinter.print_line ~lvl_delta_pre:(-1) printer "end";

    CodegenPrinter.print_line ~lvl_delta_pre:(-1) printer "end"
  )

let codegen_states printer
  (graphs : EventGraph.event_graph_collection)
  (pg : EventGraph.proc_graph)
  (g : EventGraph.event_graph) =
  codegen_decl printer g;
  codegen_next printer pg g;
  codegen_sustained_actions printer g;
  codegen_transition printer graphs g

