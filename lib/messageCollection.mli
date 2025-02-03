(** This module provides definitions for managing a collection of {{!Lang.message_def}message types}. *)

(** A collection of message types from different origins. *)
type t = {
  endpoints : Lang.endpoint_def list; (** locally-created endpoints *)
  args : Lang.endpoint_def list; (** endpoints passed from outside *)
  local_messages :
    (Lang.endpoint_def * Lang.message_def * Lang.message_direction) list;
    (** all message types associated with locally-created endpoints *)
}

(** [create channels args channel_classes] create a message type collection from given
channels, passed-in endpoints, and channel classes.*)
val create : Lang.channel_def list -> Lang.endpoint_def list -> Lang.spawn_def list -> Lang.channel_class_def list -> t

val lookup_channel_class : Lang.channel_class_def list -> Lang.identifier -> Lang.channel_class_def option
val lookup_endpoint : t -> string -> Lang.endpoint_def option

(** This only includes non-foreign messages. The message direction is adjusted to reflect the endpoint direction. *)
val lookup_message : t -> Lang.message_specifier -> Lang.channel_class_def list -> Lang.message_def option
