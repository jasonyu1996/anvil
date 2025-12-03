open Lang

type t = {
  endpoints : endpoint_def list;
  args : endpoint_def list;
  local_messages : (endpoint_def * message_def * message_direction) list;
}

let lookup_channel_class (channel_classes : channel_class_def list) (name : identifier) : channel_class_def option =
  List.find_opt (fun (cc : channel_class_def) -> cc.name = name) channel_classes

let lookup_endpoint_internal (args : endpoint_def list)
                    (endpoints : endpoint_def list) (endpoint_name : identifier) : endpoint_def option =
  let match_fun = fun (p : endpoint_def) -> p.name = endpoint_name in
  let local_endpoint_opt = List.find_opt match_fun endpoints in
  if Option.is_none local_endpoint_opt then
    List.find_opt match_fun args
  else
    local_endpoint_opt

let lookup_endpoint (mc : t) =
  lookup_endpoint_internal mc.args mc.endpoints

let lookup_message (mc : t) (msg_spec : message_specifier) (channel_classes : channel_class_def list) =
  let ( let* ) = Option.bind in
  let* endpoint = lookup_endpoint mc msg_spec.endpoint in
  if endpoint.foreign then
    None
  else (
    let* cc = lookup_channel_class channel_classes endpoint.channel_class in
    let* msg = List.find_opt (fun (m : message_def) -> m.name = msg_spec.msg) cc.messages in
    (* adjust the direction of the message to account for the direction of the endpoint *)
    let* msg = Some {msg with dir = get_message_direction msg.dir endpoint.dir} in
    Some (ParamConcretise.concretise_message cc.params endpoint.channel_params msg)
  )

let message_sync_mode_allowed = function
| Static (n, m) -> n >= 0 && m > 0
| Dependent (_, n) -> n >= 0
| Dynamic -> true

let create (channels : channel_def ast_node list)
           (args : endpoint_def ast_node list)
           (spawns : spawn_def ast_node list)
           (channel_classes : channel_class_def list) =
  let endpoint_in_spawns =
    List.map data_of_ast_node spawns
    |> List.concat_map (fun (spawn : spawn_def) -> spawn.params)
    |> Utils.StringSet.of_list in
  let get_foreign name = Utils.StringSet.mem name endpoint_in_spawns in
  let codegen_chan = fun span (chan : channel_def) ->
    (* let (left_foreign, right_foreign) =
      match chan.visibility with
      | BothForeign -> (true, true)
      | LeftForeign -> (true, false)
      | RightForeign -> (false, true)
    in *)
    (* We ignore the annotated foreign *)
    let left_endpoint = { name = chan.endpoint_left; channel_class = chan.channel_class;
                          channel_params = chan.channel_params;
                          dir = Left; foreign = get_foreign chan.endpoint_left; opp = Some chan.endpoint_right } in
    let right_endpoint = { name = chan.endpoint_right; channel_class = chan.channel_class;
                          channel_params = chan.channel_params;
                          dir = Right; foreign = get_foreign chan.endpoint_right; opp = Some chan.endpoint_left } in
    [(left_endpoint, span); (right_endpoint, span)]
  in
  let endpoints = List.concat_map (fun (ch : channel_def ast_node) -> codegen_chan ch.span ch.d) channels in
  let gather_from_endpoint ((endpoint, span): endpoint_def * code_span) =
    match lookup_channel_class channel_classes endpoint.channel_class with
    | Some cc ->
        let msg_map = fun (msg: message_def) ->
          (* these should have been checked earlier *)
          assert ((message_sync_mode_allowed msg.send_sync) && (message_sync_mode_allowed msg.recv_sync));
          let msg_dir = get_message_direction msg.dir endpoint.dir in
          (endpoint, ParamConcretise.concretise_message cc.params endpoint.channel_params msg, msg_dir)
        in List.map msg_map cc.messages
    | None ->
        raise (Except.TypeError [
          Text (Printf.sprintf "Channel class %s not found" endpoint.channel_class);
          Except.codespan_local span
        ])
  in
  (* override the user-specified foreign in args *)
  let args = List.map (fun ({d = ep; span} : endpoint_def ast_node) -> ({ep with foreign = get_foreign ep.name}, span)) args in
  let local_messages = List.filter (fun ((p, _) : endpoint_def * code_span) -> not p.foreign) (args @ endpoints) |>
  List.concat_map gather_from_endpoint in
  {endpoints = List.map fst endpoints; args = List.map fst args; local_messages}


let endpoint_owned t endpoint_name =

  if Option.is_some (lookup_endpoint t endpoint_name) then 
    let ep = Option.get (lookup_endpoint t endpoint_name) in
    not ep.foreign
  else
    false