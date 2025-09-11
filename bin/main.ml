let compile_with_json_output config =
  let open Anvil.Config in
  if config.input_filenames = [] then begin
  let json_errors = [Anvil.JsonOutput.{
    error_type = "error";
    path = None;
    description = [ { kind = "text"; text = Some "Error: a file name must be supplied!"; trace = None } ]
  }] in
    let json_result = Anvil.JsonOutput.failure_output json_errors in
    print_endline (Anvil.JsonOutput.json_output_to_string json_result);
    exit 1
  end else begin
    let temp_file = Filename.temp_file "anvil_output" ".sv" in
    try
      let temp_out = open_out temp_file in
      (try
        Anvil.CompileDriver.compile temp_out config;
        close_out temp_out;
        let output_content = In_channel.with_open_text temp_file In_channel.input_all in
        let json_result = Anvil.JsonOutput.success_output output_content in
        print_endline (Anvil.JsonOutput.json_output_to_string json_result)
      with
        | exn ->
          close_out_noerr temp_out;
          raise exn
      );
      Sys.remove temp_file
    with
    | Anvil.CompileDriver.CompileError msg ->
      (try Sys.remove temp_file with _ -> ());
      let json_errors = Anvil.JsonOutput.error_message_to_json_errors "error" msg in
      let json_result = Anvil.JsonOutput.failure_output json_errors in
      print_endline (Anvil.JsonOutput.json_output_to_string json_result)
  end

let compile_with_normal_output config =
  let open Anvil.Config in
  if config.input_filenames = [] then begin
    Printf.eprintf "Error: a file name must be supplied!\n";
    exit 1
  end else begin
    try
      Anvil.CompileDriver.compile stdout config
    with
    | Anvil.CompileDriver.CompileError msg ->
      let open Anvil.Lang in
      Printf.eprintf "Compilation failed!\n";
      let open Anvil.Except in
      List.iter (
        function
        | Text msg_text -> Printf.eprintf "%s\n" msg_text
        | Codespan (file_name, span) -> (
          let file_name = Option.get file_name in
          Printf.eprintf "%s:%d:%d:\n" file_name span.st.pos_lnum (span.st.pos_cnum - span.st.pos_bol);
          SpanPrinter.print_code_span ~indent:2 ~trunc:(-5) stderr file_name span
        )
      ) msg;
      exit 1
  end

let () =
  let config = Anvil.Config.parse_args() in
  if config.json_output then
    compile_with_json_output config
  else
    compile_with_normal_output config
