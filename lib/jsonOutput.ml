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
let error_message_to_json_errors (error_type : string) (msg : Except.error_message) =
  let open Except in
  let description =
    List.map (function
      | Text desc -> { kind = "text"; text = Some desc; trace = None }
      | Codespan (path, span) ->
        let open Lang in
        let trace = {
          path;
          start_pos = { line = span.st.pos_lnum; col = span.st.pos_cnum - span.st.pos_bol };
          end_pos = { line = span.ed.pos_lnum; col = span.ed.pos_cnum - span.ed.pos_bol };
        } in
        let str =
          match path with
          | None -> None
          | Some filename -> SpanPrinter.string_of_code_span filename span
        in
        { kind = "codespan"; text = str; trace = Some trace }
    ) msg
  in
  let rec find_first_codespan = function
    | [] -> None
    | Codespan (path, span) :: _ -> Some (path, span)
    | _ :: rest -> find_first_codespan rest
  in
  let path = match find_first_codespan msg with
    | None -> None
    | Some (path, _) -> path
  in
  [{ error_type; path; description }]



module Y = Yojson.Basic

let json_position_to_yojson (p : json_position) =
  `Assoc [
    ("line", `Int p.line);
    ("col", `Int p.col)
  ]

let json_trace_to_yojson (t : json_trace) =
  `Assoc [
    ("path", match t.path with None -> `Null | Some p -> `String p);
    ("start", json_position_to_yojson t.start_pos);
    ("end", json_position_to_yojson t.end_pos)
  ]

let json_fragment_to_yojson (f : json_fragment) =
  let base = [
    ("kind", `String f.kind);
    ("text", match f.text with None -> `Null | Some t -> `String t)
  ] in
  match f.trace with
  | None -> `Assoc base
  | Some tr -> `Assoc (base @ [ ("trace", json_trace_to_yojson tr) ])

let json_error_to_yojson (err : json_error) =
  `Assoc [
    ("type", `String err.error_type);
    ("path", match err.path with None -> `Null | Some p -> `String p);
    ("description", `List (List.map json_fragment_to_yojson err.description))
  ]

let json_output_to_yojson (json_out : json_output) =
  `Assoc [
    ("success", `Bool json_out.success);
    ("errors", `List (List.map json_error_to_yojson json_out.errors));
    ("output", match json_out.output with None -> `Null | Some s -> `String s)
  ]

let json_output_to_string (json_out : json_output) : string =
  Y.to_string (json_output_to_yojson json_out)

(** Create successful JSON output *)
let success_output output_str =
  { success = true; errors = []; output = Some output_str }

(** Create failed JSON output *)
let failure_output errors =
  { success = false; errors; output = None }
