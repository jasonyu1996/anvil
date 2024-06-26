let event_is_reg (e : EventGraph.event) =
  match e.source with
  | `Seq _ | `Root -> true
  | _ -> false

let codegen_decl printer (g : EventGraph.event_graph) =
  let st_count = List.length g.events in
  Printf.sprintf "logic[%d:0] _event_reached;" (st_count - 1) |>
    CodegenPrinter.print_line printer;
  for i = 0 to st_count - 1 do
    Printf.sprintf "logic _event_current%d;" i |>
      CodegenPrinter.print_line printer
  done;
  let print_next_signal (e : EventGraph.event) =
    if event_is_reg e then
      Printf.sprintf "logic _event_next%d;" e.id |>
        CodegenPrinter.print_line printer
    else ()
  in
  List.iter print_next_signal g.events

(* generate the next-cycle signals*)
let codegen_next printer (g : EventGraph.event_graph) =
  let print_line = CodegenPrinter.print_line printer in
  let print_lines = CodegenPrinter.print_lines printer in
  let print_compute_next (e : EventGraph.event) =
    match e.source with
    | `Seq (e', d) ->
      (
        match d with
        | `Cycles 1 -> Printf.sprintf "assign _event_next%d = _event_current%d;" e.id e'.id |> print_line
        | `Cycles _ ->
          raise (Except.UnimplementedError "Codegen states for multi cycle event unimplemented!")
      )
    | `Earlier (e1, e2) ->
      Printf.sprintf "assign _event_current%d = !_event_reached[%d] && (_event_current%d || _event_current%d);"
        e.id e.id e1.id e2.id |> print_line
    | `Later (e1, e2) ->
      [
        Printf.sprintf "assign _event_current%d = !_event_reached[%d] &&" e.id e.id;
        Printf.sprintf "(_event_reached[%d] || _event_current%d) && (_event_reached[%d] || _event_current%d);"
        e1.id e1.id e2.id e2.id
      ] |> print_lines
    | `Root ->
      Printf.sprintf "assign _event_next%d = 1'b0;" e.id |> print_line
    | `Branch (cond, e') ->
      let print wn=
        if cond.neg then
          Printf.sprintf "assign _event_current%d = _event_current%d && !%s;" e.id e'.id wn |> print_line
        else
          Printf.sprintf "assign _event_current%d = _event_current%d && %s;" e.id e'.id wn |> print_line
      in
      print @@ CodegenFormat.format_wirename cond.w.id
  in
  List.iter print_compute_next g.events

let codegen_actions printer (g : EventGraph.event_graph) =
  let print_line ?(lvl_delta_pre = 0) ?(lvl_delta_post = 0) =
    CodegenPrinter.print_line printer ~lvl_delta_pre:lvl_delta_pre ~lvl_delta_post:lvl_delta_post in
  let open EventGraph in
  let print_event_actions (e : event) =
    if e.actions <> [] then begin
      Printf.sprintf "if (_event_current%d) begin" e.id |> print_line ~lvl_delta_post:1;
      let print_action = function
        | DebugPrint (s, ws) ->
          Printf.sprintf "$display(\"%s\"%s);"
            s
            (List.map (fun (x : wire) -> Printf.sprintf ", %s" @@ CodegenFormat.format_wirename x.id) ws |>
            String.concat "") |> print_line
        | DebugFinish ->
          print_line "$finish;"
        | RegAssign (reg_ident, w) ->
          Printf.sprintf "%s <= %s;"
            (CodegenFormat.format_regname_current reg_ident) (CodegenFormat.format_wirename w.id)
            |> print_line
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
    print_line " _event_reached <= '0;";
    List.iter (fun (e : EventGraph.event) -> if event_is_reg e then begin
      if e.id = root_id then
        Printf.sprintf "_event_current%d <= 1'b1;" e.id |> print_line
      else
        Printf.sprintf "_event_current%d <= 1'b0;" e.id |> print_line
      end else ()
    ) g.events
  in
  (* reset states *)
  print_line ~lvl_delta_post:1 "always_ff @(posedge clk_i or negedge rst_ni) begin : st_transition";
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
  Printf.sprintf "if (_event_current%d) begin" g.last_event_id |> print_line ~lvl_delta_post:1;
  print_reset_states ();
  print_line ~lvl_delta_pre:(-1) ~lvl_delta_post:1 "end else begin";
  List.iter (fun (e : EventGraph.event) ->
    [
      Printf.sprintf "if (_event_current%d)" e.id;
      Printf.sprintf "_event_reached[%d] <= 1'b1;" e.id
    ] |> print_lines;
    if event_is_reg e then
      Printf.sprintf "_event_current%d <= _event_next%d;" e.id e.id |> print_line
    else ()
  ) g.events;
  print_line ~lvl_delta_pre:(-1) "end";
  print_line ~lvl_delta_pre:(-1) "end";
  print_line ~lvl_delta_pre:(-1) "end"

let codegen_states printer _ctx
  (graphs : EventGraph.event_graph_collection)
  (g : EventGraph.event_graph) =
  codegen_decl printer g;
  codegen_next printer g;
  codegen_transition printer graphs g

