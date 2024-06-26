type t

val create : out_channel -> int -> t
val print_line : ?lvl_delta_pre : int -> ?lvl_delta_post : int -> t -> string -> unit
val print_lines : ?lvl_delta_pre : int -> ?lvl_delta_post : int -> t -> string list -> unit
val adjust_level : int -> t -> unit
