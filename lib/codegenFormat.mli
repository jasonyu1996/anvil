(** Formatter functions for different constructs during the code generation process. *)

val format_msg_prefix : string -> string -> string
val format_msg_data_signal_name : string -> string -> int -> string
val format_msg_valid_signal_name : string -> string -> string
val format_msg_ack_signal_name : string -> string -> string
val format_wirename : int -> int -> string
val format_dtype : TypedefMap.t -> Lang.data_type -> string
val format_literal : Lang.literal -> string
val format_binop : Lang.binop -> string
val format_unop : Lang.unop -> string
val format_regname_current : string -> string
val format_regname_next : string -> string
val format_wire_maybe_const : WireCollection.wire MaybeConst.maybe_int_const -> string

(** When a pair of endpoints are instantiated within a process, the process has access to
both of them although they merely mirror each other. This function {i canonicalises} a given endpoint
name so in such cases both endpoints are transformed into the same name. *)
val canonicalize_endpoint_name : Lang.identifier -> EventGraph.event_graph -> Lang.identifier
