(** This module provides definitions for managing a collection of {{!Lang.message_def}message types}. *)

(** A collection of message types from different origins. *)
type t = {
  endpoints : Lang.endpoint_def list; (** locally-created endpoints *)
  args : Lang.endpoint_def list; (** endpoints passed from outside *)
  local_messages :
    (Lang.endpoint_def * Lang.message_def * Lang.message_direction) list;
    (** all message types associated with locally-created endpoints *)
}

