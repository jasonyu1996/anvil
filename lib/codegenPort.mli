(** Codegen related to ports. *)

(** A port to be generated. *)
type t = {
  dir : Lang.message_direction;
  dtype : Lang.data_type;
  name : string;
}

(** Check if a message has a [valid] port. *)
val message_has_valid_port : Lang.message_def -> bool

(** Check if a message has an [ack] port. *)
val message_has_ack_port : Lang.message_def -> bool

val message_has_data_port : Lang.message_def -> int -> bool

(** Produce a list of all ports to be generated for a list of end points. *)
val gather_ports :
  Lang.channel_class_def list -> Lang.endpoint_def list -> t list

(** Clock port. *)
val clk : t

(** Reset port. *)
val rst : t

(** Format the definition of a port. This involves translating the
    type and direction into their equivalents in Verilog. *)
val format : TypedefMap.t -> Lang.macro_def list -> t -> string
