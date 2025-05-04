open Lang

type t = {
  dir: message_direction;
  dtype: data_type;
  name: identifier;
}

let message_has_valid_port (msg : message_def) : bool = msg.send_sync = Dynamic
let message_has_ack_port (msg : message_def) : bool = msg.recv_sync = Dynamic

let gather_ports_from_endpoint (channel_classes : channel_class_def list) (endpoint : endpoint_def) : t list =
  let cc = Option.get (MessageCollection.lookup_channel_class channel_classes endpoint.channel_class) in
  let gen_endpoint_ports = fun (msg : message_def) ->
    let msg = ParamConcretise.concretise_message cc.params endpoint.channel_params msg in
    let folder_inner = fun fmt msg_dir (n, port_list) (stype : sig_type_chan_local) ->
      let new_port : t = {
        name = fmt endpoint.name msg.name n;
        dir = msg_dir;
        dtype = stype.dtype;
      } in (n + 1, new_port::port_list)
    in
    let msg_data_dir = get_message_direction msg.dir endpoint.dir in
    let (_, res) = List.fold_left (folder_inner CodegenFormat.format_msg_data_signal_name msg_data_dir) (0, []) msg.sig_types in
    let res =
      if message_has_valid_port msg then
        let valid_port = { name = CodegenFormat.format_msg_valid_signal_name endpoint.name msg.name; dir = msg_data_dir; dtype = `Logic} in
        valid_port::res
      else res
    in
    if message_has_ack_port msg then
      let ack_port = {name = CodegenFormat.format_msg_ack_signal_name endpoint.name msg.name; dir = reverse msg_data_dir; dtype = `Logic} in
      ack_port::res
    else res
  in List.concat_map gen_endpoint_ports cc.messages

let gather_ports (channel_classes : channel_class_def list) (endpoints : endpoint_def list) : t list =
  List.concat_map (gather_ports_from_endpoint channel_classes) endpoints

let clk = {dir = Inp; dtype = `Logic; name = "clk_i"}
let rst = {dir = Inp; dtype = `Logic; name = "rst_ni"}

let format (typedefs : TypedefMap.t) (macro_defs: macro_def list) port =
  let inout = match port.dir with
    | Inp -> "input"
    | Out -> "output"
  in Printf.sprintf "%s %s %s" inout (CodegenFormat.format_dtype typedefs macro_defs port.dtype) port.name
