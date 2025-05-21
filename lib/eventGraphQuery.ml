open Lang
open EventGraph

let lookup_channel_class (gc : event_graph_collection) (name : identifier) : channel_class_def option =
  Utils.StringMap.find_opt name gc.channel_classes

let lookup_endpoint_internal (args : endpoint_def list)
                    (endpoints : endpoint_def list) (endpoint_name : identifier) : endpoint_def option =
  let match_fun = fun (p : endpoint_def) -> p.name = endpoint_name in
  let local_endpoint_opt = List.find_opt match_fun endpoints in
  if Option.is_none local_endpoint_opt then
    List.find_opt match_fun args
  else
    local_endpoint_opt

let lookup_endpoint (_gc : event_graph_collection) (pg : proc_graph) =
  lookup_endpoint_internal pg.messages.args pg.messages.endpoints

let lookup_message (gc : event_graph_collection) (pg : proc_graph) (msg_spec : message_specifier) =
  let ( let* ) = Option.bind in
  let* endpoint = lookup_endpoint gc pg msg_spec.endpoint in
  if endpoint.foreign then
    None
  else (
    let* cc = lookup_channel_class gc endpoint.channel_class in
    let* msg = List.find_opt (fun (m : message_def) -> m.name = msg_spec.msg) cc.messages in
    (* adjust the direction of the message to account for the direction of the endpoint *)
    let* msg = Some {msg with dir = get_message_direction msg.dir endpoint.dir} in
    Some (ParamConcretise.concretise_message cc.params endpoint.channel_params msg)
  )
