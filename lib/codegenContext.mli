type t = {
  mutable endpoints : Lang.endpoint_def list;
  mutable local_messages :
    (Lang.endpoint_def * Lang.message_def * Lang.message_direction) list;
}
val create : unit -> t
val generate_channels : t -> Lang.channel_def list -> unit
val generate_local_messages :
  t -> Lang.channel_class_def list -> Lang.endpoint_def list -> unit
