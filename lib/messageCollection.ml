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
  let ( >>= ) = Option.bind in
  lookup_endpoint mc msg_spec.endpoint >>=
    (fun endpoint -> lookup_channel_class channel_classes endpoint.channel_class) >>=
    (fun cc -> List.find_opt (fun (m : message_def) -> m.name = msg_spec.msg) cc.messages)

let create (channels : channel_def list)
           (args : endpoint_def list)
           (channel_classes : channel_class_def list) =
  let codegen_chan = fun (chan : channel_def) ->
    let (left_foreign, right_foreign) =
      match chan.visibility with
      | BothForeign -> (true, true)
      | LeftForeign -> (true, false)
      | RightForeign -> (false, true)
    in
    let left_endpoint = { name = chan.endpoint_left; channel_class = chan.channel_class;
                          dir = Left; foreign = left_foreign; opp = Some chan.endpoint_right } in
    let right_endpoint = { name = chan.endpoint_right; channel_class = chan.channel_class;
                          dir = Right; foreign = right_foreign; opp = Some chan.endpoint_left } in
    [left_endpoint; right_endpoint]
  in
  let endpoints = List.concat_map codegen_chan channels in
  let gather_from_endpoint (endpoint: endpoint_def) =
    let cc = Option.get (lookup_channel_class channel_classes endpoint.channel_class) in
    let msg_map = fun (msg: message_def) ->
      let msg_dir = get_message_direction msg.dir endpoint.dir in
      (endpoint, msg, msg_dir)
    in List.map msg_map cc.messages
  in
  let local_messages = List.filter (fun p -> not p.foreign) (args @ endpoints) |>
  List.concat_map gather_from_endpoint in
  {endpoints; args; local_messages}
