let () =
  let config = Anvil.Config.parse_args() in
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
          SpanPrinter.print_code_span stderr file_name span
        )
      ) msg;
      exit 1
  end
