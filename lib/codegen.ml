open Lang

type event_graph = EventGraph.event_graph
type proc_graph = EventGraph.proc_graph  (* Ensure proc_graph is defined in EventGraph *)
type event_graph_collection = EventGraph.event_graph_collection

module Format = CodegenFormat

type port_def = CodegenPort.t


let codegen_ports printer (graphs : event_graph_collection)
                      (endpoints : endpoint_def list) (is_mod_comb : bool)=
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
  if is_mod_comb then
    print_port_list port_list
  else
    print_port_list ([CodegenPort.clk; CodegenPort.rst] @ port_list);
  port_list

let codegen_spawns printer (graphs : event_graph_collection) (g : proc_graph) =
  
  let gen_connect = fun (dst : string) (src : string) ->
    Printf.sprintf ",.%s (%s)" dst src |> CodegenPrinter.print_line printer
  in
  let gen_connect_post = fun (dst : string) (src : string) ->
    Printf.sprintf ".%s (%s)," dst src |> CodegenPrinter.print_line printer 
  in
  
  let gen_connect_blank = fun (dst : string) (src : string) ->
    Printf.sprintf ".%s (%s)" dst src |> CodegenPrinter.print_line printer 
  in

  let gen_spawn = fun (idx : int) ((module_name, spawn) : string * (spawn_def ast_node)) ->
    let proc_other = CodegenHelpers.lookup_proc graphs.external_event_graphs module_name |> Option.get in

    let is_not_extern = match proc_other.extern_module ,proc_other.proc_body with
    | None, _ -> true
    | (Some _ , Lang.Extern (_,body)) -> (
        if List.is_empty body.named_ports then true
        else false
      )
    | _ -> true in
    let is_spawn_comb = (List.for_all (fun (thread , _) ->
    (thread : EventGraph.event_graph).comb
  ) proc_other.threads) && (is_not_extern) in

    Printf.sprintf "%s _spawn_%d (" module_name idx|> CodegenPrinter.print_line printer ~lvl_delta_post:1;
    if (not is_spawn_comb) then(
        CodegenPrinter.print_line printer ".clk_i,";
        CodegenPrinter.print_line printer ".rst_ni";
    );
    (* connect the wires *)
    
    let connect_endpoints = fun (arg_endpoint : endpoint_def) (param_ident : identifier) ->
      let endpoint_local = MessageCollection.lookup_endpoint g.messages param_ident |> Option.get in
      let endpoint_name_local = CodegenFormat.canonicalize_endpoint_name param_ident (List.hd g.threads |> fst) in
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
          if(is_spawn_comb) then (  
            gen_connect_post (fmt arg_endpoint.name msg.name idx)
              (fmt endpoint_name_local msg.name idx)
          )
          else (
            gen_connect (fmt arg_endpoint.name msg.name idx)
              (fmt endpoint_name_local msg.name idx)
          )
        in begin
          List.iteri (print_data_con Format.format_msg_data_signal_name) msg.sig_types;
        end
      in 
      let print_msg_con_last = fun (msg : message_def) ->
        let msg = ParamConcretise.concretise_message cc.params endpoint_local.channel_params msg in
        if CodegenPort.message_has_valid_port msg then
            gen_connect_post (Format.format_msg_valid_signal_name arg_endpoint.name msg.name)
              (Format.format_msg_valid_signal_name endpoint_name_local msg.name)
        else ();
        if CodegenPort.message_has_ack_port msg then
          gen_connect_post (Format.format_msg_ack_signal_name arg_endpoint.name msg.name)
            (Format.format_msg_ack_signal_name endpoint_name_local msg.name)
        else ();
        let len = List.length msg.sig_types in
        let print_data_con_last = fun fmt idx _ ->
          if (is_spawn_comb) then (
            if (idx == len - 1) then (
              gen_connect_blank (fmt arg_endpoint.name msg.name idx)
                (fmt endpoint_name_local msg.name idx)
            )
            else (
              gen_connect_post (fmt arg_endpoint.name msg.name idx)
                (fmt endpoint_name_local msg.name idx)
            )
          )
          else (gen_connect (fmt arg_endpoint.name msg.name idx)
            (fmt endpoint_name_local msg.name idx))
        in begin
          List.iteri (print_data_con_last Format.format_msg_data_signal_name) msg.sig_types;
        end
      in 
      let first_msg = List.hd cc.messages in
      (* remove first msg from the list *)
      let new_list = List.tl cc.messages in

      List.iter print_msg_con new_list;
      if is_spawn_comb then
        print_msg_con_last first_msg
      else print_msg_con first_msg

    in
    let param_count = List.fold_left (fun acc p -> match p with Lang.SingleEp _ -> acc + 1 | Lang.RangeEp (_,_ ,sz) -> acc + sz) 0 spawn.d.params in
    if List.length proc_other.messages.args <> param_count then
      raise (Failure (Printf.sprintf "Invalid number of arguments for spawn of proc %s"  proc_other.name));
    List.iter2 connect_endpoints proc_other.messages.args (Lang.preprocess_ep_spawn_args spawn.d.params);
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
  let lookup_msg_def msg = MessageCollection.lookup_message g.messages msg graphs.channel_classes in
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
  | Update (base_w, updates) -> (
    CodegenPrinter.print_line ~lvl_delta_post:1 printer
      @@ Printf.sprintf "always_comb begin: _%s_assign" @@ Format.format_wirename w.thread_id w.id;
    CodegenPrinter.print_line printer
      @@ Printf.sprintf "%s = %s;" (Format.format_wirename w.thread_id w.id)
      @@ (Format.format_wirename base_w.thread_id base_w.id);
    List.iter (fun (offset, size, (update_w : EventGraph.wire)) ->
      CodegenPrinter.print_line printer
      @@ Printf.sprintf "%s[%d +: %d] = %s;" (Format.format_wirename w.thread_id w.id)
        offset size @@ (Format.format_wirename update_w.thread_id update_w.id);
    ) updates;
    CodegenPrinter.print_line ~lvl_delta_pre:(-1) printer "end"
  )
  | _ -> (
    let expr =
      match w.source with
      | Literal lit -> Format.format_literal lit
      | Binary (binop, w1, w2) ->
        (
          match w2 with
          | `List ws2 ->
            Printf.sprintf "%s %s {%s}"
              (Format.format_wirename w1.thread_id w1.id)
              (Format.format_binop binop)
              (List.map (fun (w' : WireCollection.wire) -> Format.format_wirename w'.thread_id w'.id) ws2 |>
            String.concat ", ")
          | `Single w2n ->
              Printf.sprintf "%s %s %s"
                (Format.format_wirename w1.thread_id w1.id)
                (Format.format_binop binop)
                (Format.format_wirename w2n.thread_id w2n.id)
        )
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
        let m = Option.get @@ lookup_msg_def msg in
        if CodegenPort.message_has_valid_port m then
          CodegenFormat.format_msg_valid_signal_name (CodegenFormat.canonicalize_endpoint_name msg.endpoint g) msg.msg
        else "1'b1"
      | MessageAckPort msg ->
        let m = Option.get @@ lookup_msg_def msg in
        if CodegenPort.message_has_valid_port m then
          CodegenFormat.format_msg_ack_signal_name (CodegenFormat.canonicalize_endpoint_name msg.endpoint g) msg.msg
        else "1'b1"
      | Cases _ | Update _ -> failwith "Something went wrong!"
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
        Printf.sprintf "%s %s;" (format_dtype graphs.typedefs graphs.macro_defs r.d_type)
          (format_regname_current r.name) |>
          CodegenPrinter.print_line printer
    )
    g.regs

let codegen_proc printer (graphs : EventGraph.event_graph_collection) (g : proc_graph) =

  let is_not_extern = match g.extern_module ,g.proc_body with
    | None, _ -> true
    | (Some _ , Lang.Extern (_,body)) -> (
        if List.is_empty body.named_ports then true
        else false
      )
    | _ -> true in
  let is_mod_comb = (List.for_all (fun (thread , _) ->
    (thread : EventGraph.event_graph).comb
  ) g.threads) && (is_not_extern) in


  (* generate ports *)
  Printf.sprintf "module %s (" g.name |> CodegenPrinter.print_line printer ~lvl_delta_post:1;

  (* Generate ports for the first thread *)
  let _ = codegen_ports printer graphs g.messages.args is_mod_comb in
  CodegenPrinter.print_line printer ~lvl_delta_pre:(-1) ~lvl_delta_post:1 ");";

  (
    match g.extern_module, g.proc_body with
    | (Some extern_mod_name, Lang.Extern (_, body)) ->
      (* spawn the external module *)
      (* TODO: simplify *)
      let open Lang in
      Printf.sprintf "%s _extern_mod (" extern_mod_name
        |> CodegenPrinter.print_line printer ~lvl_delta_post:1;
      let connected_ports = ref Utils.StringSet.empty in
      let print_binding ext i len local =
        let fmt = if (i = (len-1)) then (
          Printf.sprintf ".%s (%s)"
        ) else
          Printf.sprintf ".%s (%s),"
        in connected_ports := Utils.StringSet.add local !connected_ports;
        fmt ext local
          |> CodegenPrinter.print_line printer
      in
      let len1 = List.length body.named_ports in
      let len2 = (List.length body.msg_ports)*3 in
      let len = len1 + len2 in
      List.iteri (fun i (port_name, extern_port_name) ->
        print_binding extern_port_name i len port_name 
      ) body.named_ports;
      let i_sync = ref 0 in
      List.iter (fun (msg, extern_data_opt, extern_vld_opt, extern_ack_opt) ->
        let print_msg_port_opt formatter extern_opt =
        (  
          match extern_opt with
          | None -> ()
          | Some extern_port ->
            let i_off = len1 + !i_sync in
            formatter msg.endpoint msg.msg |> print_binding extern_port i_off len
        );
          i_sync := !i_sync + 1;
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
        | Inp -> ()
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
      let initEvents = fst @@ List.hd g.threads in

      codegen_endpoints printer graphs initEvents;

      codegen_spawns printer graphs g;

      codegen_regs printer graphs initEvents;
      if (not is_mod_comb) then CodegenStates.codegen_proc_states printer g;
      
      (* Iterate over all threads to print states *)
      List.iter (fun (thread, reset_by) ->
          codegen_post_declare printer graphs thread;
          CodegenStates.codegen_states printer graphs g thread reset_by;
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
      List.iter (fun (g, _) ->
        EventGraphOps.print_dot_graph g Out_channel.stderr
      ) pg.threads
    ) graphs.event_graphs;
  );
  let printer = CodegenPrinter.create out 2 in
  List.iter (codegen_proc printer graphs) graphs.event_graphs

