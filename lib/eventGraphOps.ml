open EventGraph


let lifetime_const current = {live = current; dead = [(current, `Eternal)]}
let lifetime_immediate current = {live = current; dead = [(current, `Cycles 1)]}

let full_reg_range regname size =
  {
    subreg_name = regname;
    subreg_range_interval = (Const 0, size)
  }

let subreg_ranges_possibly_intersect r1 r2 =
  r1.subreg_name = r2.subreg_name &&
    (match fst r1.subreg_range_interval, fst r2.subreg_range_interval with
    | Const n1, Const n2 ->
      let end1 = n1 + (snd r1.subreg_range_interval)
      and end2 = n2 + (snd r2.subreg_range_interval) in
      end2 > n1 && end1 > n2
    | _ -> true
    )

let print_graph (g: event_graph) =
  List.iter (fun ev ->
    match ev.source with
    | `Later (e1, e2) -> Printf.eprintf "> %d: later %d %d\n" ev.id e1.id e2.id
    | `Seq (ev', a) ->
      let c = match a with
      | `Cycles n -> Printf.sprintf "C %d" n
      | `Send _ -> "S"
      | `Recv _ -> "R"
      | `Sync s -> Printf.sprintf "S %s" s
      in
      Printf.eprintf "> %d: seq %d %s\n" ev.id ev'.id c
    | `Branch (ev', _) -> Printf.eprintf "> %d: branch %d\n" ev.id ev'.id
    | `Root None -> Printf.eprintf "> %d: root\n" ev.id
    | `Root (Some (ev', br_side_info)) ->
        Printf.eprintf "> %d: branch-root %d %d\n" ev.id br_side_info.branch_side_sel ev'.id
  ) g.events

let print_dot_graph g out =
  Printf.fprintf out "// BEGIN GRAPH IN DOT FORMAT\n";
  Printf.fprintf out "// can render to PDF with 'dot -Tpdf -O <filename>'\n";
  Printf.fprintf out "digraph {\n";
  let ev_node_name ev = Printf.sprintf "event%d" ev.id in
  let print_edge e1 e2 label =
    Printf.fprintf out "  %s -> %s [label = \"%s\"];\n"
      (ev_node_name e1)
      (ev_node_name e2)
      label
  in
  List.iter (fun ev ->
    match ev.source with
    | `Later (e1, e2) ->
      print_edge e1 ev "L";
      print_edge e2 ev "L"
    | `Seq (ev', a) ->
      let label = match a with
      | `Cycles n -> Printf.sprintf "#%d" n
      | `Send _ -> "S"
      | `Recv _ -> "R"
      | `Sync _ -> "G"
      in
      print_edge ev' ev label
    | `Branch (_, br_info) ->
      List.iter (fun e ->
        print_edge e ev "B"
      ) br_info.branches_val
    | `Root None -> ()
    | `Root (Some (ev', br_side_info)) ->
      print_edge ev' ev @@ string_of_int br_side_info.branch_side_sel
  ) g.events;
  Printf.fprintf out "}\n";
  Printf.fprintf out "// END GRAPH IN DOT FORMAT\n"

let find_last_event g =
  List.find (
    fun e ->
      e.is_recurse
  ) g.events
