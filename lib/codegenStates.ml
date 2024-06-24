let event_is_reg (e : EventGraph.event) =
  match e.source with
  | `Seq _ | `Root -> true
  | _ -> false

let codegen_decl out (g : EventGraph.event_graph) =
  let st_count = List.length g.events in
  Printf.fprintf out "  logic[%d:0] _event_reached;\n" (st_count - 1);
  for i = 0 to st_count - 1 do
    Printf.fprintf out "  logic _event_current%d;\n" i
  done;
  let print_next_signal (e : EventGraph.event) =
    if event_is_reg e then
      Printf.fprintf out "  logic _event_next%d;\n" e.id
    else ()
  in
  List.iter print_next_signal g.events

(* generate the next-cycle signals*)
let codegen_next out (g : EventGraph.event_graph) =
  let print_compute_next (e : EventGraph.event) =
    match e.source with
    | `Seq (e', d) ->
      (
        match d with
        | `Cycles 1 -> Printf.fprintf out "  assign _event_next%d = _event_current%d;\n"
          e.id e'.id
        | `Cycles _ ->
          raise (Except.UnimplementedError "Codegen states for multi cycle event unimplemented!")
      )
    | `Earlier (e1, e2) ->
      Printf.fprintf out "  assign _event_current%d = !_event_reached[%d] && (_event_current%d || _event_current%d);\n"
        e.id e.id e1.id e2.id
    | `Later (e1, e2) ->
      Printf.fprintf out "  assign _event_current%d = !_event_reached[%d] &&
    (_event_reached[%d] || _event_current%d) && (_event_reached[%d] || _event_current%d);\n"
        e.id e.id e1.id e1.id e2.id e2.id
    | `Root ->
      Printf.fprintf out "  assign _event_next%d = 1'b0;\n" e.id
    | `Branch (cond, e') ->
      let print =
        if cond.neg then
          Printf.fprintf out "  assign _event_current%d = _event_current%d && !%s;\n"
            e.id e'.id
        else
          Printf.fprintf out "  assign _event_current%d = _event_current%d && %s;\n"
            e.id e'.id
      in
      print @@ CodegenFormat.format_wirename cond.w.id
  in
  List.iter print_compute_next g.events

let codegen_actions out (g : EventGraph.event_graph) =
  let open EventGraph in
  let print_event_actions (e : event) =
    if e.actions <> [] then begin
      Printf.fprintf out "      if (_event_current%d) begin\n" e.id;
      let print_action = function
        | DebugPrint (s, ws) ->
          Printf.fprintf out "        $display(\"%s\"%s);\n"
            s
            (List.map (fun (x : wire) -> Printf.sprintf ", %s" @@ CodegenFormat.format_wirename x.id) ws |>
            String.concat "")
        | DebugFinish ->
          Printf.fprintf out "        $finish;\n"
        | RegAssign (reg_ident, w) ->
          Printf.fprintf out "        %s <= %s;\n"
            (CodegenFormat.format_regname_current reg_ident) (CodegenFormat.format_wirename w.id)
      in
      List.iter print_action e.actions;
      Printf.fprintf out "      end\n"
    end else ()
  in
  List.iter print_event_actions g.events


let codegen_transition out (_graphs : EventGraph.event_graph_collection) (g : EventGraph.event_graph) =
  let root_id =
    List.find_map (fun (x : EventGraph.event) -> if x.source = `Root then Some x.id else None) g.events |> Option.get in
  let print_reset_states () =
    Printf.fprintf out "      _event_reached <= '0;\n";
    List.iter (fun (e : EventGraph.event) -> if event_is_reg e then begin
      if e.id = root_id then
        Printf.fprintf out "      _event_current%d <= 1'b1;\n" e.id
      else
        Printf.fprintf out "      _event_current%d <= 1'b0;\n" e.id
      end else ()
    ) g.events
  in
  Printf.fprintf out
  (* reset states *)
"  always_ff @(posedge clk_i or negedge rst_ni) begin : st_transition
    if (~rst_ni) begin\n";
  print_reset_states ();
  (* register reset *)
  List.iter (
    fun (r : Lang.reg_def) ->
      let open CodegenFormat in
      Printf.fprintf out "      %s <= '0;\n" (format_regname_current r.name)
  ) g.regs;
  Printf.fprintf out
"    end else begin\n";
  (* actions *)
  codegen_actions out g;
  (* next states *)
  (* FIXME: timing is not quite right *)
  Printf.fprintf out
"      if (_event_current%d) begin\n" g.last_event_id;
  print_reset_states ();
  Printf.fprintf out "      end else begin\n";
  List.iter (fun (e : EventGraph.event) ->
    Printf.fprintf out
"      if (_event_current%d)
        _event_reached[%d] <= 1'b1;\n" e.id e.id;
    if event_is_reg e then
      Printf.fprintf out "      _event_current%d <= _event_next%d;\n" e.id e.id
    else ()
  ) g.events;
  Printf.fprintf out "      end\n    end\n  end\n"

let codegen_states out _ctx
  (graphs : EventGraph.event_graph_collection)
  (g : EventGraph.event_graph) =
  codegen_decl out g;
  codegen_next out g;
  codegen_transition out graphs g

