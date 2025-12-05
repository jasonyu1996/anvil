
type t = {
  buffer : bytes;
  mutable pos : int64;
  len : int64;
}

val with_open : string -> (t -> 'a) -> 'a
val with_open_aliased : string -> string -> (t -> 'a) -> 'a
val input : t -> bytes -> int64 -> int64 -> int64
val input_line : t -> string option
val seek : t -> int64 -> unit
val length : t -> int64
val pos : t -> int64
