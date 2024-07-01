open Lang

let format_msg_prefix (endpoint_name : identifier) (message_name : identifier) : identifier =
  Printf.sprintf "_%s_%s" endpoint_name message_name

let format_msg_data_signal_name (endpoint_name : identifier) (message_name : identifier) (data_idx : int) : string =
  Printf.sprintf "_%s_%s_%d" endpoint_name message_name data_idx

let format_msg_valid_signal_name (endpoint_name : identifier) (message_name : identifier) : string =
  Printf.sprintf "_%s_%s_valid" endpoint_name message_name

let format_msg_ack_signal_name (endpoint_name : identifier) (message_name : identifier) : string =
  Printf.sprintf "_%s_%s_ack" endpoint_name message_name


let format_wirename (id : int) : string = Printf.sprintf "_wire$%d" id

let format_dtype (typedefs : TypedefMap.t) (dtype : data_type) =
  match dtype with
  | `Logic -> "logic"
  | `Opaque typename -> typename
  | _ -> (TypedefMap.data_type_size typedefs dtype) - 1 |> Printf.sprintf "logic[%d:0]"

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
