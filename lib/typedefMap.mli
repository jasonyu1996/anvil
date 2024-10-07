(** This module provides typedef map {!t}, a data structure that maintains
data type definitions. *)

type t

(** An empty typedef map. *)
val empty : t

(** Create a typedef map from a list of type definitions. *)
val of_list : Lang.type_def list -> t

(** Name-resolve a given data type. *)
val data_type_name_resolve : t -> Lang.data_type -> Lang.data_type option

(** Look up and calculate the size of a data type in bits. *)
val data_type_size : t -> Lang.data_type -> int

(** Look up a field in a data type. If found, return [Some (l, r, dtype)] where
[l] and [r] are the boundaries of the offset of the field within the data type,
and [dtype] is the data type of the field.*)
val data_type_indirect :
  t -> Lang.data_type -> string -> (int * int * Lang.data_type) option

(** Resolve the indexing of a data type. If valid, return [Some (l, r, dtype)],
the meaning of which is the same as in {!data_type_indirect}.
*)
val data_type_index :
  t -> Lang.data_type -> Lang.index -> (int * int * Lang.data_type) option

(** Look up whether a data type is integral (logic or logic arrays). *)
val type_is_integral : t -> Lang.data_type -> bool

(** Compute the resulting data type of a binary operation. *)
val type_check_binop :
  t ->
  Lang.binop -> Lang.data_type -> Lang.data_type -> Lang.data_type option
