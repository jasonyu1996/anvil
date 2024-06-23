open Lang

type t = {
  mutable endpoints : endpoint_def list;
  mutable local_messages : (endpoint_def * message_def * message_direction) list;
}

let create () =
  { endpoints = []; local_messages = [] }

let generate_channels (ctx: t) (channels : channel_def list) =
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
  ctx.endpoints <- List.concat_map codegen_chan channels

let generate_local_messages (ctx : t) (channel_classes : channel_class_def list) (args : endpoint_def list) =
  let gather_from_endpoint (endpoint: endpoint_def) =
    let cc = Option.get (CodegenHelpers.lookup_channel_class channel_classes endpoint.channel_class) in
    let msg_map = fun (msg: message_def) ->
      let msg_dir = get_message_direction msg.dir endpoint.dir in
      (endpoint, msg, msg_dir)
    in List.map msg_map cc.messages
  in
  let local_messages = List.filter (fun p -> not p.foreign) (args @ ctx.endpoints) |>
  List.concat_map gather_from_endpoint in
  ctx.local_messages <- local_messages