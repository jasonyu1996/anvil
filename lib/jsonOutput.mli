(** JSON output module for anvil compiler results *)

type json_position = {
  line: int;
  col: int;
}

type json_trace = {
  path: string option;
  start_pos: json_position;
  end_pos: json_position;
}

type json_fragment = {
  kind: string;  (* "text" | "codespan" *)
  text: string option;
  trace: json_trace option;
}

type json_error = {
  error_type: string;  (* "warning" | "error" *)
  path: string option;
  description: json_fragment list;
}

type json_output = {
  success: bool;
  errors: json_error list;
  output: string option;
}

(** Convert error message to JSON errors *)
val error_message_to_json_errors : string -> Except.error_message -> json_error list

(** Convert JSON output to string *)
val json_output_to_string : json_output -> string

(** Create successful JSON output *)
val success_output : string -> json_output

(** Create failed JSON output *)
val failure_output : json_error list -> json_output
