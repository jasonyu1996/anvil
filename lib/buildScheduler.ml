type build_proc_task = {
  module_name: string;
  proc_name: string;
  param_values: Lang.param_value list;
}

module StringHashTbl = Hashtbl.Make(String)

type build_scheduler = {
  task_queue : build_proc_task Queue.t;
  proc_instance_counter : int StringHashTbl.t
}

let create () =
  {
    task_queue = Queue.create ();
    proc_instance_counter = StringHashTbl.create 8;
  }

let next sched =
  if Queue.is_empty sched.task_queue then None
  else Some (Queue.pop sched.task_queue)

let add_proc_task sched proc_name param_values =
  let module_name = if param_values = [] then
    proc_name
  else (
    (* mangling the process name *)
    let n = StringHashTbl.find_opt sched.proc_instance_counter proc_name
      |> Option.value ~default:0 in
    StringHashTbl.replace sched.proc_instance_counter proc_name (n + 1);
    Printf.sprintf "%s_%d" proc_name n
  ) in
  Queue.add {module_name; proc_name; param_values} sched.task_queue;
  module_name
