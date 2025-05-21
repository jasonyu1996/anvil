open Lang

let format_msg_prefix (endpoint_name : identifier) (message_name : identifier) : identifier =
  Printf.sprintf "_%s_%s" endpoint_name message_name

let format_msg_data_signal_name (endpoint_name : identifier) (message_name : identifier) (data_idx : int) : string =
  Printf.sprintf "_%s_%s_%d" endpoint_name message_name data_idx

let format_msg_valid_signal_name (endpoint_name : identifier) (message_name : identifier) : string =
  Printf.sprintf "_%s_%s_valid" endpoint_name message_name

let format_msg_ack_signal_name (endpoint_name : identifier) (message_name : identifier) : string =
  Printf.sprintf "_%s_%s_ack" endpoint_name message_name


let format_wirename (thread_id : int) (id : int) : string = Printf.sprintf "thread_%d_wire$%d" thread_id id

let format_dtype (typedefs : TypedefMap.t) (macro_defs : Lang.macro_def list) (dtype : data_type) =
  match dtype with
  | `Logic -> "logic[0:0]"
  | `Opaque typename -> typename
  | _ ->
    let size = (TypedefMap.data_type_size typedefs macro_defs dtype) in
    if size = 0 then
      "int" (* TODO: hacky *)
    else
      Printf.sprintf "logic[%d:0]" @@ size - 1

let format_literal = function
  | Binary (len, b) -> Printf.sprintf "%d'b%s" len (List.map string_of_digit b |> List.rev |> String.concat "")
  | Decimal (len, d) -> Printf.sprintf "%d'd%s" len (List.map string_of_digit d |> List.rev |> String.concat "")
  | Hexadecimal (len, h) -> Printf.sprintf "%d'h%s" len (List.map string_of_digit h |> List.rev |> String.concat "")
  | WithLength (len, v) -> Printf.sprintf "%d'd%d" len v
  | NoLength n -> string_of_int n

let format_binop = string_of_binop
let format_unop = string_of_unop

let format_regname_current (regname : identifier) =
  Printf.sprintf "%s_q" regname
let format_regname_next (regname : identifier) =
  Printf.sprintf "%s_n" regname

let format_wire_maybe_const (v : WireCollection.wire MaybeConst.maybe_int_const) =
  let open MaybeConst in
  match v with
  | Const n -> Printf.sprintf "%d" n
  | NonConst w -> format_wirename w.thread_id w.id

module Endpoint = struct
  open EventGraph
  let canonicalize (endpoint : endpoint_def) : identifier =
    match endpoint.dir with
    | Left -> endpoint.name
    | Right -> Option.value ~default:endpoint.name endpoint.opp

  let canonicalize_endpoint_name (endpoint_name : identifier) (gc : event_graph_collection) (proc : proc_graph) : identifier =
    match EventGraphQuery.lookup_endpoint gc proc endpoint_name with
    | Some endpoint_local -> canonicalize endpoint_local
    | None -> endpoint_name
end

let canonicalize_endpoint_name = Endpoint.canonicalize_endpoint_name
