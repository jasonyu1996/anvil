type t
val empty : t
val of_list : Lang.type_def list -> t
val data_type_name_resolve : t -> Lang.data_type -> Lang.data_type option
val data_type_size : t -> Lang.data_type -> int
val data_type_indirect :
  t -> Lang.data_type -> string -> (int * int * Lang.data_type) option
val data_type_index :
  t -> Lang.data_type -> Lang.index -> (int * int * Lang.data_type) option
val type_is_integral : t -> Lang.data_type -> bool
val type_check_binop :
  t ->
  Lang.binop -> Lang.data_type -> Lang.data_type -> Lang.data_type option
