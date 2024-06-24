val format_msg_prefix : string -> string -> string
val format_msg_data_signal_name : string -> string -> int -> string
val format_msg_valid_signal_name : string -> string -> string
val format_msg_ack_signal_name : string -> string -> string
val format_wirename : int -> string
val format_dtype : TypedefMap.t -> Lang.data_type -> string
val format_literal : Lang.literal -> string
val format_binop : Lang.binop -> string
val format_unop : Lang.unop -> string
val format_regname_current : string -> string
val format_regname_next : string -> string
