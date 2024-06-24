open Lang

type codegen_context = CodegenContext.t

type event_graph = EventGraph.event_graph
type event_graph_collection = EventGraph.event_graph_collection

module Format = CodegenFormat

module Port = struct
  type t = {
    dir: message_direction;
    dtype: data_type;
    name: identifier;
  }

  let message_has_valid_port (msg : message_def) : bool = msg.send_sync = Dynamic
  let message_has_ack_port (msg : message_def) : bool = msg.recv_sync = Dynamic

  let gather_ports_from_endpoint (channel_classes : channel_class_def list) (endpoint : endpoint_def) : t list =
    let cc = Option.get (CodegenHelpers.lookup_channel_class channel_classes endpoint.channel_class) in
    let gen_endpoint_ports = fun (msg : message_def) ->
      let folder_inner = fun fmt msg_dir (n, port_list) (stype : sig_type_chan_local) ->
        let new_port : t = {
          name = fmt endpoint.name msg.name n;
          dir = msg_dir;
          dtype = stype.dtype;
        } in (n + 1, new_port::port_list)
      in
      let msg_data_dir = get_message_direction msg.dir endpoint.dir in
      let (_, res) = List.fold_left (folder_inner Format.format_msg_data_signal_name msg_data_dir) (0, []) msg.sig_types in
      let res =
        if message_has_valid_port msg then
          let valid_port = { name = Format.format_msg_valid_signal_name endpoint.name msg.name; dir = msg_data_dir; dtype = `Logic} in
          valid_port::res
        else res
      in
      if message_has_ack_port msg then
        let ack_port = {name = Format.format_msg_ack_signal_name endpoint.name msg.name; dir = reverse msg_data_dir; dtype = `Logic} in
        ack_port::res
      else res
    in List.concat_map gen_endpoint_ports cc.messages

  let gather_ports (channel_classes : channel_class_def list) (endpoints : endpoint_def list) : t list =
    List.concat_map (gather_ports_from_endpoint channel_classes) endpoints

  let clk = {dir = In; dtype = `Logic; name = "clk_i"}
  let rst = {dir = In; dtype = `Logic; name = "rst_ni"}

  let format (typedefs : TypedefMap.t) port =
    let inout = match port.dir with
      | In -> "input"
      | Out -> "output"
    in Printf.sprintf "%s %s %s" inout (Format.format_dtype typedefs port.dtype) port.name
end

module Endpoint = struct
  let _is_canonical (endpoint: endpoint_def) : bool =
    (Option.is_none endpoint.opp) || (endpoint.dir = Left)

  let canonical_name (endpoint: endpoint_def) : identifier =
    match endpoint.dir with
    | Left -> endpoint.name
    | Right -> Option.value ~default:endpoint.name endpoint.opp
end

type port_def = Port.t

let codegen_ports out (graphs : event_graph_collection)
                      (endpoints : endpoint_def list) =
  let port_list = Port.gather_ports graphs.channel_classes endpoints in
  let rec print_port_list out port_list =
  match port_list with
  | [] -> ()
  | port :: [] ->
      let port_fmt = Port.format graphs.typedefs port in Printf.fprintf out "  %s\n" port_fmt
  | port :: port_list' ->
      let port_fmt = Port.format graphs.typedefs port in Printf.fprintf out "  %s,\n" port_fmt;
      print_port_list out port_list'
  in
  print_port_list out ([Port.clk; Port.rst] @ port_list)

let codegen_spawns out (ctx: codegen_context) (graphs : event_graph_collection) (g : event_graph) =
  let gen_connect = fun (dst : string) (src : string) ->
    Printf.fprintf out ",\n    .%s (%s)" dst src
  in
  let gen_spawn = fun (idx : int) (spawn : spawn_def) ->
    Printf.fprintf out "  %s _spawn_%d (\n    .clk_i,\n    .rst_ni" spawn.proc idx;
    (* connect the wires *)
    let proc_other = CodegenHelpers.lookup_proc graphs.event_graphs spawn.proc |> Option.get in
    let connect_endpoints = fun (arg_endpoint : endpoint_def) (param_ident : identifier) ->
      let endpoint_local = CodegenHelpers.lookup_endpoint g ctx.endpoints param_ident |> Option.get in
      let endpoint_name_local = Endpoint.canonical_name endpoint_local in
      let cc = CodegenHelpers.lookup_channel_class graphs.channel_classes endpoint_local.channel_class |> Option.get in
      let print_msg_con = fun (msg : message_def) ->
        if Port.message_has_valid_port msg then
          gen_connect (Format.format_msg_valid_signal_name arg_endpoint.name msg.name)
            (Format.format_msg_valid_signal_name endpoint_name_local msg.name)
        else ();
        if Port.message_has_ack_port msg then
          gen_connect (Format.format_msg_ack_signal_name arg_endpoint.name msg.name)
            (Format.format_msg_ack_signal_name endpoint_name_local msg.name)
        else ();
        let print_data_con = fun fmt idx _ ->
          gen_connect (fmt arg_endpoint.name msg.name idx)
            (fmt endpoint_name_local msg.name idx)
        in begin
          List.iteri (print_data_con Format.format_msg_data_signal_name) msg.sig_types;
        end
      in List.iter print_msg_con cc.messages
    in List.iter2 connect_endpoints proc_other.args spawn.params;
    Printf.fprintf out "\n  );\n"
  in List.iteri gen_spawn g.spawns

let codegen_endpoints out (ctx: codegen_context) (graphs : event_graph_collection) =
  let print_port_signal_decl = fun (port : port_def) ->
    Printf.fprintf out "  %s %s;\n" (Format.format_dtype graphs.typedefs port.dtype) (port.name)
  in
  List.filter (fun (p : endpoint_def) -> p.dir = Left) ctx.endpoints |>
  Port.gather_ports graphs.channel_classes |>
  List.iter print_port_signal_decl

let codegen_wire_assignment out (w : WireCollection.wire) =
  let expr =
    match w.source with
    | Literal lit -> Format.format_literal lit
    | Binary (binop, w1, w2) ->
      Printf.sprintf "%s %s %s"
        (Format.format_wirename w1.id)
        (Format.format_binop binop)
        (Format.format_wirename w2.id)
    | Unary (unop, w') ->
      Printf.sprintf "%s%s"
        (Format.format_unop unop)
        (Format.format_wirename w'.id)
    | Switch (sw, d) ->
      let conds = List.map
        (fun ((cond, v) : WireCollection.wire * WireCollection.wire) ->
          Printf.sprintf "(%s) ? %s : "
            (Format.format_wirename cond.id)
            (Format.format_wirename v.id)
        )
        sw
      |> String.concat "" in
      Printf.sprintf "%s%s" conds (Format.format_wirename d.id)
    | RegRead reg_ident -> Format.format_regname_current reg_ident
  in
  Printf.fprintf out "  assign %s = %s;\n" (Format.format_wirename w.id) expr


let codegen_post_declare out (_ctx : codegen_context) (graphs : event_graph_collection) (g : event_graph) =
  (* wire declarations *)
  let codegen_wire_decl = fun (w: WireCollection.wire) ->
    Printf.fprintf out "  %s %s;\n" (Format.format_dtype graphs.typedefs w.dtype) @@ Format.format_wirename w.id
  in List.iter codegen_wire_decl g.wires;
  List.iter (codegen_wire_assignment out) g.wires
  (* set send signals *)
  (* StringMap.iter (fun _ {msg_spec; select} ->
    let data_wires = gather_data_wires_from_msg ctx proc msg_spec
    and msg_prefix = format_msg_prefix msg_spec.endpoint msg_spec.msg in
    match select with
    | [ws] ->
        (* just assign without mux *)
        List.iter2 (fun (res_wire : identifier) ((data_wire, _) : (wire_def * _)) ->
          print_assign {wire = data_wire.name; expr_str = res_wire}) ws data_wires
    | _ ->
        (* need to use mux *)
        let ws_bufs = List.map (fun ((w, _) : (wire_def * _)) -> Printf.sprintf "  assign %s =" w.name |>
          String.to_seq |> Buffer.of_seq) data_wires
        and cur_idx = ref @@ List.length select in
        List.iter (fun ws ->
          cur_idx := !cur_idx - 1;
          List.iter2 (fun buf w -> Buffer.add_string buf @@ Printf.sprintf " (%s_mux_n == %d) ? %s :"
              msg_prefix !cur_idx w) ws_bufs ws) select;
        List.iter (fun (buf : Buffer.t) -> Printf.printf "%s '0;\n" @@ Buffer.contents buf) ws_bufs
  ) ctx.sends *)

let codegen_regs out _ctx (graphs : event_graph_collection) (g : event_graph) =
  List.iter
    (
      fun (r : reg_def) ->
        let open CodegenFormat in
        Printf.fprintf out "  %s %s;\n" (format_dtype graphs.typedefs r.dtype)
          (format_regname_current r.name)
    )
    g.regs

let codegen_proc out (graphs : EventGraph.event_graph_collection) (g : event_graph) =
  let ctx = CodegenContext.create () in
  CodegenContext.generate_channels ctx g.channels;
  CodegenContext.generate_local_messages ctx graphs.channel_classes g.args;

  (* generate ports *)
  Printf.fprintf out "module %s (\n" g.name;
  codegen_ports out graphs ctx.endpoints;
  Printf.fprintf out ");\n";

  codegen_endpoints out ctx graphs;
  codegen_spawns out ctx graphs g;
  codegen_regs out ctx graphs g;
  codegen_post_declare out ctx graphs g;
  CodegenStates.codegen_states out ctx graphs g;

  Printf.fprintf out "endmodule\n"

let codegen_preamble out =
  Printf.fprintf out "/* verilator lint_off UNOPTFLAT */\n"

let generate (out : out_channel)
             (_config : Config.compile_config)
             (graphs : EventGraph.event_graph_collection) : unit =
  codegen_preamble out;
  List.iter (codegen_proc out graphs) graphs.event_graphs

