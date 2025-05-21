open Lang

type t = {
  endpoints : endpoint_def list;
  args : endpoint_def list;
  local_messages : (endpoint_def * message_def * message_direction) list;
}

