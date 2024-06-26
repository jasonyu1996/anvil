type t = {
  endpoints : Lang.endpoint_def list;
  args : Lang.endpoint_def list;
  local_messages :
    (Lang.endpoint_def * Lang.message_def * Lang.message_direction) list;
}
val create : Lang.channel_def list -> Lang.endpoint_def list -> Lang.channel_class_def list -> t
val lookup_channel_class : Lang.channel_class_def list -> Lang.identifier -> Lang.channel_class_def option
val lookup_endpoint : t -> string -> Lang.endpoint_def option
val lookup_message : t -> Lang.message_specifier -> Lang.channel_class_def list -> Lang.message_def option
