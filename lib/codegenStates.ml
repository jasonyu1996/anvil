let event_is_reg (e : EventGraph.event) =
  match e.source with
  | `Root -> true
  | `Seq (_, `Sync _) -> false
  | `Seq _ -> true
  | _ -> false

let codegen_decl printer (g : EventGraph.event_graph) =
  let st_count = List.length g.events in
  Printf.sprintf "logic[%d:0] _thread_%d_event_reached;" (st_count - 1) g.thread_id |>
    CodegenPrinter.print_line printer;
  for i = 0 to st_count - 1 do
    Printf.sprintf "logic _thread_%d_event_current%d;" g.thread_id i|>
      CodegenPrinter.print_line printer
  done;
  let print_next_signal (e : EventGraph.event) =
    if event_is_reg e then
      Printf.sprintf "logic _thread_%d_event_next%d;" g.thread_id e.id |>
        CodegenPrinter.print_line printer
    else ()
  in
  List.iter print_next_signal g.events

(* generate the next-cycle signals*)
let codegen_next printer (pg : EventGraph.proc_graph) (g : EventGraph.event_graph) =
  let print_line = CodegenPrinter.print_line printer in
  let print_lines = CodegenPrinter.print_lines printer in
  let print_compute_next (e : EventGraph.event) =
    match e.source with
    | `Seq (e', d) ->
      (
        match d with
        | `Cycles n when n >= 1 ->
          if n == 1 then
            Printf.sprintf "assign _thread_%d_event_next%d = _thread_%d_event_current%d;"
              g.thread_id e.id g.thread_id e'.id |> print_line
          else
            [
              Printf.sprintf "logic [%d:0] _thread_%d_event_counter%d;" (Utils.int_log2 (n+1)) g.thread_id e.id;
              Printf.sprintf "logic _thread_%d_event_counter_valid%d;" g.thread_id e.id;
              Printf.sprintf  "logic _thread_%d_event_counter_begin%d;" g.thread_id e.id;
              Printf.sprintf "always_ff @(posedge clk_i or negedge rst_ni) begin";
              Printf.sprintf "  if (~rst_ni) begin";
              Printf.sprintf "    _thread_%d_event_counter%d <= '0;" g.thread_id e.id;
              Printf.sprintf "    _thread_%d_event_counter_valid%d <= 1'b0;" g.thread_id e.id;
              Printf.sprintf "    _thread_%d_event_counter_begin%d <= 1'b0;" g.thread_id e.id;
              Printf.sprintf "  end else if (_thread_%d_event_current%d) begin" g.thread_id e'.id;
              Printf.sprintf "      _thread_%d_event_counter_begin%d <= 1'b1;" g.thread_id e.id;
              Printf.sprintf "      _thread_%d_event_counter_valid%d <= 1'b0;" g.thread_id e.id;
              Printf.sprintf "  end else if (_thread_%d_event_counter_begin%d && !_thread_%d_event_counter_valid%d) begin" g.thread_id e.id g.thread_id e.id;
              Printf.sprintf "    if (_thread_%d_event_counter%d >= %d) begin" g.thread_id e.id (n - 1);
              Printf.sprintf "      _thread_%d_event_counter%d <= '0;" g.thread_id e.id;
              Printf.sprintf "      _thread_%d_event_counter_begin%d <= 1'b0;" g.thread_id e.id;
              Printf.sprintf "      _thread_%d_event_counter_valid%d <= 1'b1;" g.thread_id e.id;
              Printf.sprintf "    end else begin";
              Printf.sprintf "      _thread_%d_event_counter%d <= _thread_%d_event_counter%d + 1'b1;" g.thread_id e.id g.thread_id e.id;
              Printf.sprintf "    end";
              Printf.sprintf "  end else begin";
              Printf.sprintf "    _thread_%d_event_counter%d <= '0;" g.thread_id e.id;
              Printf.sprintf "    _thread_%d_event_counter_begin%d <= 1'b0;" g.thread_id e.id;
              Printf.sprintf "    _thread_%d_event_counter_valid%d <= 1'b0;" g.thread_id e.id;
              Printf.sprintf "  end";
              Printf.sprintf "end";
              Printf.sprintf "assign _thread_%d_event_next%d = _thread_%d_event_counter_valid%d;" g.thread_id e.id g.thread_id e.id;
            ] |> print_lines
        | `Cycles _ ->
          failwith "Invalid number of cycles"
        | `Send msg ->
          [
            Printf.sprintf "assign _thread_%d_event_next%d = !_thread_%d_event_current%d && !_thread_%d_event_reached[%d] &&"
              g.thread_id e.id g.thread_id e.id g.thread_id e.id;
            Printf.sprintf "(_thread_%d_event_current%d || _thread_%d_event_reached[%d]) && %s;"
              g.thread_id e'.id g.thread_id e'.id
              (CodegenFormat.format_msg_ack_signal_name (EventGraph.canonicalize_endpoint_name msg.endpoint g) msg.msg)
          ] |> print_lines
        | `Recv msg ->
          [
            Printf.sprintf "assign _thread_%d_event_next%d = !_thread_%d_event_current%d && !_thread_%d_event_reached[%d] &&"
              g.thread_id e.id g.thread_id e.id g.thread_id e.id;
            Printf.sprintf "(_thread_%d_event_current%d || _thread_%d_event_reached[%d]) && %s;"
              g.thread_id e'.id g.thread_id e'.id
              (CodegenFormat.format_msg_valid_signal_name (EventGraph.canonicalize_endpoint_name msg.endpoint g) msg.msg)
          ] |> print_lines
        | `Sync s ->
          let si = Hashtbl.find pg.shared_vars_info s in
          let assigned_at = Option.get si.assigned_at in
          Printf.sprintf "assign _thread_%d_event_current%d = _thread_%d_event_current%d;"
            g.thread_id e.id assigned_at.graph.thread_id assigned_at.id |> print_line
      )
    | `Either (e1, e2) ->
      Printf.sprintf "assign _thread_%d_event_current%d = !_thread_%d_event_reached[%d] && (_thread_%d_event_current%d || _thread_%d_event_current%d);"
        g.thread_id e.id g.thread_id e.id g.thread_id e1.id g.thread_id e2.id |> print_line
    | `Later (e1, e2) ->
      [
        Printf.sprintf "assign _thread_%d_event_current%d = !_thread_%d_event_reached[%d] &&" g.thread_id e.id g.thread_id e.id;
        Printf.sprintf "(_thread_%d_event_reached[%d] || _thread_%d_event_current%d) && (_thread_%d_event_reached[%d] || _thread_%d_event_current%d);"
        g.thread_id e1.id g.thread_id e1.id g.thread_id e2.id g.thread_id e2.id
      ] |> print_lines
    | `Root ->
      Printf.sprintf "assign _thread_%d_event_next%d = 1'b0;" g.thread_id e.id |> print_line
    | `Branch (cond, e') ->
      let print wn=
        if cond.neg then
          Printf.sprintf "assign _thread_%d_event_current%d = _thread_%d_event_current%d && !%s;" g.thread_id e.id g.thread_id e'.id wn |> print_line
        else
          Printf.sprintf "assign _thread_%d_event_current%d = _thread_%d_event_current%d && %s;" g.thread_id e.id g.thread_id e'.id wn |> print_line
      in
      let w = Option.get cond.data.w in
      print @@ CodegenFormat.format_wirename w.thread_id w.id
  in
  List.iter print_compute_next g.events

let codegen_actions printer (g : EventGraph.event_graph) =
  let print_line ?(lvl_delta_pre = 0) ?(lvl_delta_post = 0) =
    CodegenPrinter.print_line printer ~lvl_delta_pre:lvl_delta_pre ~lvl_delta_post:lvl_delta_post in
  let open EventGraph in
  let print_event_actions (e : event) =
    if e.actions <> [] then begin
      Printf.sprintf "if (_thread_%d_event_current%d) begin" g.thread_id e.id |> print_line ~lvl_delta_post:1;
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
  let print_lines = CodegenPrinter.print_lines printer in
  let root_id =
    List.find_map (fun (x : EventGraph.event) -> if x.source = `Root then Some x.id else None) g.events |> Option.get in
  let print_reset_states () =
    Printf.sprintf "_thread_%d_event_reached <= '0;" g.thread_id |> print_line;
    List.iter (fun (e : EventGraph.event) -> if event_is_reg e then begin
      if e.id = root_id then
        Printf.sprintf "_thread_%d_event_current%d <= 1'b1;" g.thread_id e.id |> print_line
      else
        Printf.sprintf "_thread_%d_event_current%d <= 1'b0;" g.thread_id e.id |> print_line
      end else ()
    ) g.events
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
  (* FIXME: timing is not quite right *)
  Printf.sprintf "if (_thread_%d_event_current%d) begin" g.thread_id g.last_event_id |> print_line ~lvl_delta_post:1;
  print_reset_states ();
  print_line ~lvl_delta_pre:(-1) ~lvl_delta_post:1 "end else begin";
  List.iter (fun (e : EventGraph.event) ->
    [
      Printf.sprintf "if (_thread_%d_event_current%d)" g.thread_id e.id;
      Printf.sprintf "_thread_%d_event_reached[%d] <= 1'b1;" g.thread_id e.id
    ] |> print_lines;
    if event_is_reg e then
      Printf.sprintf "_thread_%d_event_current%d <= _thread_%d_event_next%d;" g.thread_id e.id g.thread_id e.id |> print_line
    else ()
  ) g.events;
  print_line ~lvl_delta_pre:(-1) "end";
  print_line ~lvl_delta_pre:(-1) "end";
  print_line ~lvl_delta_pre:(-1) "end"

let codegen_sustained_actions printer (g : EventGraph.event_graph) =
  let print_line = CodegenPrinter.print_line printer in
  let or_assigns = Hashtbl.create 8
  and wire_assigns = Hashtbl.create 8 in (* name -> (cond, value) *)
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
      let started = Printf.sprintf "(_thread_%d_event_reached[%d] || _thread_%d_event_current%d)"
        g.thread_id e.id g.thread_id e.id in
      let activated = Printf.sprintf "((_thread_%d_event_reached[%d] || _thread_%d_event_current%d) && !_thread_%d_event_reached[%d] && !_thread_%d_event_current%d)"
        g.thread_id e.id g.thread_id e.id g.thread_id sa.d.until.id g.thread_id sa.d.until.id in
      match sa.d.ty with
      | Send (msg, td) ->
        insert_to or_assigns
          (CodegenFormat.format_msg_valid_signal_name (EventGraph.canonicalize_endpoint_name msg.endpoint g) msg.msg)
          activated;
        let w = Option.get td.w in
        insert_to wire_assigns
          (CodegenFormat.format_msg_data_signal_name (EventGraph.canonicalize_endpoint_name msg.endpoint g) msg.msg 0)
          (started, (CodegenFormat.format_wirename w.thread_id w.id))
      | Recv msg ->
        insert_to or_assigns
          (CodegenFormat.format_msg_ack_signal_name (EventGraph.canonicalize_endpoint_name msg.endpoint g) msg.msg)
          activated
    ) e.sustained_actions
  in
  (* assuming reverse topo order, the assign lists will be in topo order *)
  List.iter print_sa_event g.events;
  Hashtbl.iter (fun w_name conds ->
    String.concat " || " !conds
    |> Printf.sprintf "assign %s = %s;" w_name
    |> print_line
  ) or_assigns;
  Hashtbl.iter (fun w_name cond_vals ->
    let rev_cond_vals = List.rev !cond_vals in
    let (_, last_v) = List.hd rev_cond_vals in
    let v_sum = if List.length rev_cond_vals = 1 then
      last_v
    else (
      (List.map (fun (cond, v) -> Printf.sprintf "%s ? %s : " cond v) rev_cond_vals
      |> String.concat "") ^ last_v (* keep the last value as default to handle loop-around *)
      (* FIXME: there's a failing case for this when the last event is in a if branch that is not taken.
        fixable by recording the last event *)
    ) in
    Printf.sprintf "assign %s = %s;" w_name v_sum
    |> print_line
  ) wire_assigns

let codegen_states printer
  (graphs : EventGraph.event_graph_collection)
  (pg : EventGraph.proc_graph)
  (g : EventGraph.event_graph) =
  codegen_decl printer g;
  codegen_next printer pg g;
  codegen_sustained_actions printer g;
  codegen_transition printer graphs g

