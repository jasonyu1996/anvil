(** Concretise the parameters in a process. This includes concretising the parameters
    used for spawning new processes or defining data types. *)
val concretise_proc : Lang.param_value list -> Lang.proc_def -> Lang.proc_def

(** Concretise data type body. *)
val concretise_dtype : Lang.param list -> Lang.param_value list -> Lang.data_type -> Lang.data_type
