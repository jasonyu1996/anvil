open Lang

type event_graph = EventGraph.event_graph
type proc_graph = EventGraph.proc_graph  (* Ensure proc_graph is defined in EventGraph *)
type event_graph_collection = EventGraph.event_graph_collection

module Format = CodegenFormat

type port_def = CodegenPort.t

let codegen_ports printer (graphs : event_graph_collection)
                      (endpoints : endpoint_def list) =
  let port_list = CodegenPort.gather_ports graphs.channel_classes endpoints in
  let rec print_port_list port_list =
  match port_list with
  | [] -> ()
  | port :: [] ->
      CodegenPort.format graphs.typedefs graphs.macro_defs port |> CodegenPrinter.print_line printer
  | port :: port_list' ->
      CodegenPort.format graphs.typedefs graphs.macro_defs port |> Printf.sprintf "%s," |> CodegenPrinter.print_line printer;
      print_port_list port_list'
  in
  print_port_list ([CodegenPort.clk; CodegenPort.rst] @ port_list);
  port_list

let codegen_spawns printer (graphs : event_graph_collection) (g : proc_graph) =
  let gen_connect = fun (dst : string) (src : string) ->
    Printf.sprintf ",.%s (%s)" dst src |> CodegenPrinter.print_line printer
  in
  let gen_spawn = fun (idx : int) ((module_name, spawn) : string * spawn_def) ->
    Printf.sprintf "%s _spawn_%d (" module_name idx|> CodegenPrinter.print_line printer ~lvl_delta_post:1;
    CodegenPrinter.print_line printer ".clk_i,";
    CodegenPrinter.print_line printer ".rst_ni";
    (* connect the wires *)
    let proc_other = CodegenHelpers.lookup_proc graphs.external_event_graphs module_name |> Option.get in
    let connect_endpoints = fun (arg_endpoint : endpoint_def) (param_ident : identifier) ->
      let endpoint_local = MessageCollection.lookup_endpoint g.messages param_ident |> Option.get in
      let endpoint_name_local = CodegenFormat.canonicalize_endpoint_name param_ident (List.hd g.threads) in
      let cc = MessageCollection.lookup_channel_class graphs.channel_classes endpoint_local.channel_class |> Option.get in
      let print_msg_con = fun (msg : message_def) ->
        let msg = ParamConcretise.concretise_message cc.params endpoint_local.channel_params msg in
        if CodegenPort.message_has_valid_port msg then
          gen_connect (Format.format_msg_valid_signal_name arg_endpoint.name msg.name)
            (Format.format_msg_valid_signal_name endpoint_name_local msg.name)
        else ();
        if CodegenPort.message_has_ack_port msg then
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
    in
    List.iter2 connect_endpoints proc_other.messages.args spawn.params;
    CodegenPrinter.print_line printer ~lvl_delta_pre:(-1) ");"
  in List.iteri gen_spawn g.spawns

let codegen_endpoints printer (graphs : event_graph_collection) (g : event_graph) =
  let print_port_signal_decl = fun (port : port_def) ->
    Printf.sprintf "%s %s;" (Format.format_dtype graphs.typedefs graphs.macro_defs port.dtype) (port.name) |>
      CodegenPrinter.print_line printer
  in
  List.filter (fun (p : endpoint_def) -> p.dir = Left) g.messages.endpoints |>
  CodegenPort.gather_ports graphs.channel_classes |>
  List.iter print_port_signal_decl

let codegen_wire_assignment printer (graphs : event_graph_collection) (g : event_graph) (w : WireCollection.wire) =
  match w.source with
  | Cases (vw, sw, d) -> (
    CodegenPrinter.print_line ~lvl_delta_post:1 printer
      @@ Printf.sprintf "always_comb begin: _%s_assign" @@ Format.format_wirename w.thread_id w.id;
    CodegenPrinter.print_line ~lvl_delta_post:1 printer
      @@ Printf.sprintf "unique case (%s)" @@ Format.format_wirename vw.thread_id vw.id;
    List.iter (fun ((wp, wb) : WireCollection.wire * WireCollection.wire) ->
      CodegenPrinter.print_line ~lvl_delta_post:1 printer
        @@ Printf.sprintf "%s:" @@ Format.format_wirename wp.thread_id wp.id;
      CodegenPrinter.print_line ~lvl_delta_post:(-1) printer
        @@ Printf.sprintf "%s = %s;" (Format.format_wirename w.thread_id w.id)
        @@ (Format.format_wirename wb.thread_id wb.id);
    ) sw;
    (* generate default case *)
    CodegenPrinter.print_line ~lvl_delta_post:1 printer "default:";
    CodegenPrinter.print_line ~lvl_delta_post:(-1) printer
      @@ Printf.sprintf "%s = %s;" (Format.format_wirename w.thread_id w.id)
      @@ (Format.format_wirename d.thread_id d.id);
    CodegenPrinter.print_line ~lvl_delta_pre:(-1) printer "endcase";
    CodegenPrinter.print_line ~lvl_delta_pre:(-1) printer "end"
  )
  | _ -> (
    let expr =
      match w.source with
      | Literal lit -> Format.format_literal lit
      | Binary (binop, w1, w2) ->
        Printf.sprintf "%s %s %s"
          (Format.format_wirename w1.thread_id w1.id)
          (Format.format_binop binop)
          (Format.format_wirename w2.thread_id w2.id)
      | Unary (unop, w') ->
        Printf.sprintf "%s%s"
          (Format.format_unop unop)
          (Format.format_wirename w'.thread_id w'.id)
      | Switch (sw, d) ->
        let conds = List.map
          (fun ((cond, v) : WireCollection.wire * WireCollection.wire) ->
            Printf.sprintf "(%s) ? %s : "
              (Format.format_wirename cond.thread_id cond.id)
              (Format.format_wirename v.thread_id v.id)
          )
          sw
        |> String.concat "" in
        Printf.sprintf "%s%s" conds (Format.format_wirename d.thread_id d.id)
      | RegRead reg_ident -> Format.format_regname_current reg_ident
      | Concat ws ->
        List.map (fun (w' : WireCollection.wire) -> Format.format_wirename w'.thread_id w'.id) ws |>
          String.concat ", " |> Printf.sprintf "{%s}"
      | MessagePort (msg, idx) ->
        let msg_endpoint = CodegenFormat.canonicalize_endpoint_name msg.endpoint g in
        Format.format_msg_data_signal_name msg_endpoint msg.msg idx
      | Slice (w', base_i, len) ->
        Printf.sprintf "%s[%s +: %d]" (Format.format_wirename w'.thread_id w'.id)
          (Format.format_wire_maybe_const base_i)
          len
      | MessageValidPort msg ->
        (* FIXME: sync pat *)
        CodegenFormat.format_msg_valid_signal_name (CodegenFormat.canonicalize_endpoint_name msg.endpoint g) msg.msg
      | Cases _ -> failwith "Something went wrong!"
    in
    CodegenPrinter.print_line printer
      @@
      if w.is_const then
        Printf.sprintf "localparam %s %s = %s;"
          (Format.format_dtype graphs.typedefs graphs.macro_defs (`Array (`Logic, ParamEnv.Concrete w.size)))
          (Format.format_wirename w.thread_id w.id) expr
      else
        Printf.sprintf "assign %s = %s;" (Format.format_wirename w.thread_id w.id) expr
  )

let codegen_post_declare printer (graphs : event_graph_collection) (g : event_graph) =
  (* wire declarations *)
  let codegen_wire_decl = fun (w: WireCollection.wire) ->
    if not w.is_const then (* constants do not correspond to wires *)
      Printf.sprintf "%s %s;" (Format.format_dtype graphs.typedefs graphs.macro_defs (`Array (`Logic, ParamEnv.Concrete w.size))) (Format.format_wirename w.thread_id w.id) |>
        CodegenPrinter.print_line printer
  in List.iter codegen_wire_decl g.wires.wire_li;
  List.iter (codegen_wire_assignment printer graphs g) @@ List.rev g.wires.wire_li (* reversed to generate from wire0 *)
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

let codegen_regs printer (graphs : event_graph_collection) (g : event_graph) =
  Utils.StringMap.iter
    (
      fun _ (r : reg_def) ->
        let open CodegenFormat in
        Printf.sprintf "%s %s;" (format_dtype graphs.typedefs graphs.macro_defs r.dtype)
          (format_regname_current r.name) |>
          CodegenPrinter.print_line printer
    )
    g.regs

let codegen_proc printer (graphs : EventGraph.event_graph_collection) (g : proc_graph) =
  (* generate ports *)
  Printf.sprintf "module %s (" g.name |> CodegenPrinter.print_line printer ~lvl_delta_post:1;

  (* Generate ports for the first thread *)
  let _ = codegen_ports printer graphs g.messages.args in
  CodegenPrinter.print_line printer ~lvl_delta_pre:(-1) ~lvl_delta_post:1 ");";

  (
    match g.extern_module, g.proc_body with
    | (Some extern_mod_name, Lang.Extern (_, body)) ->
      (* spawn the external module *)
      (* TODO: simplify *)
      let open Lang in
      Printf.sprintf "%s _extern_mod (" extern_mod_name
        |> CodegenPrinter.print_line printer ~lvl_delta_post:1;
      let first_port = ref true in
      let connected_ports = ref Utils.StringSet.empty in
      let print_binding ext local =
        let fmt = if !first_port then (
          first_port := false;
          Printf.sprintf ".%s (%s)"
        ) else
          Printf.sprintf ",.%s (%s)"
        in
        connected_ports := Utils.StringSet.add local !connected_ports;
        fmt ext local
          |> CodegenPrinter.print_line printer
      in
      List.iter (fun (port_name, extern_port_name) ->
        print_binding extern_port_name port_name
      ) body.named_ports;
      List.iter (fun (msg, extern_data_opt, extern_vld_opt, extern_ack_opt) ->
        let print_msg_port_opt formatter extern_opt =
          match extern_opt with
          | None -> ()
          | Some extern_port ->
            formatter msg.endpoint msg.msg |> print_binding extern_port
        in
        let open Format in
        print_msg_port_opt (fun e m -> format_msg_data_signal_name e m 0) extern_data_opt;
        print_msg_port_opt format_msg_valid_signal_name extern_vld_opt;
        print_msg_port_opt format_msg_ack_signal_name extern_ack_opt
      ) body.msg_ports;
      CodegenPrinter.print_line printer  ~lvl_delta_pre:(-1) ");";
      let ports = CodegenPort.gather_ports graphs.channel_classes g.messages.args in
      List.iter (fun p ->
        let open CodegenPort in
        match p.dir with
        | In -> ()
        | Out -> (
          if Utils.StringSet.mem p.name !connected_ports |> not then (
            (* not connected, assign default values *)
            Printf.sprintf "assign %s = 'b1;" p.name
              |> CodegenPrinter.print_line printer
          )
        )
      ) ports
    | _ ->
      (* Generate endpoints, spawns, regs, and post-declare for the first thread *)
      let initEvents = List.hd g.threads in

      codegen_endpoints printer graphs initEvents;

      codegen_spawns printer graphs g;

      codegen_regs printer graphs initEvents;

      (* the init event register is shared across threads *)
      CodegenPrinter.print_line printer "logic _init;";

      CodegenStates.codegen_proc_states printer g;

      (* Iterate over all threads to print states *)
      List.iter (fun thread ->
        codegen_post_declare printer graphs thread;
        CodegenStates.codegen_states printer graphs g thread;
      ) g.threads
  );

  CodegenPrinter.print_line printer ~lvl_delta_pre:(-1) "endmodule"

let generate_preamble out =
  [
    "/* verilator lint_off UNOPTFLAT */";
    "/* verilator lint_off WIDTHTRUNC */";
    "/* verilator lint_off WIDTHEXPAND */";
    "/* verilator lint_off WIDTHCONCAT */"
  ] |> List.iter (Printf.fprintf out "%s\n")

let generate_extern_import out file_name =
  In_channel.with_open_text file_name
    (fun in_channel ->
      let eof = ref false in
      while not !eof do
        match In_channel.input_line in_channel with
        | Some line ->
          Out_channel.output_string out line;
          Out_channel.output_char out '\n'
        | None -> eof := true
      done
    )

let generate (out : out_channel)
             (config : Config.compile_config)
             (graphs : EventGraph.event_graph_collection) : unit =
  if config.verbose then (
    Printf.eprintf "==== CodeGen Details ====\n";
    List.iter (fun (pg : proc_graph) ->
      List.iter (fun g ->
        EventGraphOps.print_dot_graph g Out_channel.stderr
      ) pg.threads
    ) graphs.event_graphs;
  );
  let printer = CodegenPrinter.create out 2 in
  List.iter (codegen_proc printer graphs) graphs.event_graphs

