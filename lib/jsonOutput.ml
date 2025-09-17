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
let error_message_to_json_errors error_type msg =
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

(** Escape string for JSON *)
let escape_json_string s =
  let buffer = Buffer.create (String.length s * 2) in
  String.iter (function
    | '"' -> Buffer.add_string buffer "\\\""
    | '\\' -> Buffer.add_string buffer "\\\\"
    | '\n' -> Buffer.add_string buffer "\\n"
    | '\r' -> Buffer.add_string buffer "\\r"
    | '\t' -> Buffer.add_string buffer "\\t"
    | c when Char.code c < 32 -> Printf.bprintf buffer "\\u%04x" (Char.code c)
    | c -> Buffer.add_char buffer c
  ) s;
  Buffer.contents buffer

(** Convert JSON error to string *)
let json_fragment_to_string = function
  | { kind = "codespan"; text = desc_opt; trace = Some trace } ->
      let path = trace.path in
      Printf.sprintf "{\"kind\":\"codespan\",\"text\":%s,\"path\":%s,\"trace\":{\"start\":{\"line\":%d,\"col\":%d},\"end\":{\"line\":%d,\"col\":%d}}}"
        (match desc_opt with None -> "null" | Some d -> Printf.sprintf "\"%s\"" (escape_json_string d))
        (match path with None -> "null" | Some p -> Printf.sprintf "\"%s\"" (escape_json_string p))
        trace.start_pos.line trace.start_pos.col trace.end_pos.line trace.end_pos.col

  | { kind; text; trace = _ } ->
      match text with
      | Some t -> Printf.sprintf "{\"kind\":\"%s\",\"text\":\"%s\"}" (escape_json_string kind) (escape_json_string t)
      | None -> Printf.sprintf "{\"kind\":\"%s\",\"text\":\"\"}" (escape_json_string kind)

let json_error_to_string err =
  let parts = [
    Printf.sprintf "\"type\":\"%s\"" (escape_json_string err.error_type);
    (match err.path with
     | None -> "\"path\":null"
     | Some path -> Printf.sprintf "\"path\":\"%s\"" (escape_json_string path));
    Printf.sprintf "\"description\":[%s]" (String.concat "," (List.map json_fragment_to_string err.description))
  ] in
  "{" ^ String.concat "," parts ^ "}"

(** Convert JSON output to string *)
let json_output_to_string json_out =
  let errors_str = "[" ^ String.concat "," (List.map json_error_to_string json_out.errors) ^ "]" in
  let output_str = match json_out.output with
    | None -> "null"
    | Some out -> Printf.sprintf "\"%s\"" (escape_json_string out)
  in
  let parts = [
    Printf.sprintf "\"success\":%s" (if json_out.success then "true" else "false");
    Printf.sprintf "\"errors\":%s" errors_str;
    Printf.sprintf "\"output\":%s" output_str
  ] in
  "{" ^ String.concat "," parts ^ "}"

(** Create successful JSON output *)
let success_output output_str =
  { success = true; errors = []; output = Some output_str }

(** Create failed JSON output *)
let failure_output errors =
  { success = false; errors; output = None }
