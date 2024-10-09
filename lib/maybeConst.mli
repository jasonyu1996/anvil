(** A value that may be an int const or something else of type ['a]. *)
type 'a maybe_int_const =
  | Const of int
  | NonConst of 'a

(** Add a constant integer to {!maybe_int_const}. *)
val add_const : int -> (int -> 'a -> 'a) -> 'a maybe_int_const -> 'a maybe_int_const

(** Multiply {!maybe_int_const} by a constant integer. *)
val mul_const : int -> (int -> 'a -> 'a) -> 'a maybe_int_const -> 'a maybe_int_const

val map : ('a -> 'b) -> 'a maybe_int_const -> 'b maybe_int_const

val add : (int -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a maybe_int_const -> 'a maybe_int_const -> 'a maybe_int_const

val mul : (int -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a maybe_int_const -> 'a maybe_int_const -> 'a maybe_int_const

val string_of : ('a -> string) -> 'a maybe_int_const -> string
