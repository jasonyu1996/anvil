type context = CodegenContext.t

let codegen_decl out (g : EventGraph.event_graph) =
  Printf.fprintf out "  typedef enum logic[1:0] {\n";
  Printf.fprintf out "    UNREACHED, CURRENT, REACHED\n";
  Printf.fprintf out "  } _state_t;\n";
  let st_count = List.length g.events in
  Printf.fprintf out "  _state_t[%d:0] _event_st_q;\n" (st_count - 1);
  Printf.fprintf out "  logic[%d:0] _event_next;\n" (st_count - 1)

(* generate the next-cycle signals*)
let codegen_next out (g : EventGraph.event_graph) =
  let print_compute_next (e : EventGraph.event) =
    match e.source with
    | `Seq (e', d) ->
      (
        match d with
        | `Cycles 1 -> Printf.fprintf out "  assign _event_next[%d] = _event_st_q[%d] == CURRENT;\n"
          e.id e'.id
        | `Cycles _ ->
          raise (Except.UnimplementedError "Codegen states for multi cycle event unimplemented!")
      )
    | `Earlier (e1, e2) ->
      Printf.fprintf out "  assign _event_next[%d] = _event_st_q[%d] == UNREACHED && (_event_next[%d] || _event_next[%d]);\n"
        e.id e.id e1.id e2.id
    | `Later (e1, e2) ->
      Printf.fprintf out "  assign _event_next[%d] = _event_st_q[%d] == UNREACHED &&
    (_event_next[%d] || _event_st_q[%d] != UNREACHED) && (_event_next[%d] || _event_st_q[%d] != UNREACHED);\n"
        e.id e.id e1.id e1.id e2.id e2.id
    | `Root ->
      Printf.fprintf out "  assign _event_next[%d] = 1'b0;\n" e.id
  in
  List.iter print_compute_next g.events

let codegen_actions out (g : EventGraph.event_graph) =
  let open EventGraph in
  let print_event_actions (e : event) =
    if e.actions <> [] then begin
      Printf.fprintf out "      if (_event_st_q[%d] == CURRENT) begin\n" e.id;
      let print_action = function
        | DebugPrint (s, ws) ->
          Printf.fprintf out "        $display(\"%s\"%s);\n"
            s
            (List.map (fun (x : wire) -> Printf.sprintf ", %s" @@ CodegenFormat.format_wirename x.id) ws |>
            String.concat "")
        | DebugFinish ->
          Printf.fprintf out "        $finish;\n"
      in
      List.iter print_action e.actions;
      Printf.fprintf out "      end\n"
    end else ()
  in
  List.iter print_event_actions g.events


let codegen_transition out (g : EventGraph.event_graph) =
  let st_count = List.length g.events in
  let root_id =
    List.find_map (fun (x : EventGraph.event) -> if x.source = `Root then Some x.id else None) g.events |> Option.get in
  Printf.fprintf out
"  always_ff @(posedge clk_i or negedge rst_ni) begin : st_transition
    if (~rst_ni) begin
      for (int i = 0; i < %d; i ++) begin
        if (i == %d)
          _event_st_q[i] <= CURRENT;
        else
          _event_st_q[i] <= UNREACHED;
      end
    end else begin\n" st_count root_id;
  (* actions *)
  codegen_actions out g;
  Printf.fprintf out
"      for (int i = 0; i < %d; i ++) begin
        if (_event_next[i])
          _event_st_q[i] <= CURRENT;
        else if (_event_st_q[i] == CURRENT) begin
          _event_st_q[i] <= REACHED;
        end
      end
    end
  end\n"
    st_count

let codegen_states out _ctx
  (_graphs : EventGraph.event_graph_collection)
  (g : EventGraph.event_graph) =
  codegen_decl out g;
  codegen_next out g;
  codegen_transition out g

