(** This module provides the {!maybe_param} type for describing
a value that may come from a parameter. The {!param_env} type
describes a parameter environment use during concretisation. *)

type 'a maybe_param = Param of string | Concrete of 'a
type 'a param_env

(** Create an empty parameter environment. *)
val create_env : unit -> 'a param_env

(** Add a new value binding to the environment.*)
val add_value : string -> 'a -> 'a param_env -> unit

(** Try to concretise a parameterised value in the give parameter environment. *)
val concretise : 'a param_env -> 'a maybe_param -> 'a maybe_param option

(** Get the concrete value behind a {!maybe_param}. *)
val get_opt : 'a maybe_param -> 'a option

(** Try to concretise and then get the concrete value. *)
val concretise_and_get : 'a param_env -> 'a maybe_param -> 'a option
