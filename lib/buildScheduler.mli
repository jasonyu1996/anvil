(** This module provides {!build_scheduler}, a scheduler that
maintains the modules to build. It produces {!build_proc_task} which
describes a build task. This is used in {!CompileDriver}. *)

(** Describes a build task for a module. The module may need
concretisation/monomorphisatoin first. *)
type build_proc_task = {
  module_name : string; (** name of the module, can be different from
  that of the process due to monomorphisation, typically
  a mangled form of the process name *)
  proc_name : string; (** name of the process *)
  param_values : Lang.param_value list; (** concrete parameter values *)
}

type build_scheduler

(** Create an empty scheduler. *)
val create : unit -> build_scheduler

(** Fetch the next task to build. *)
val next : build_scheduler -> build_proc_task option

(** Add a build task. Return the generated module name. *)
val add_proc_task :
  build_scheduler ->
  string -> Lang.param_value list -> string
