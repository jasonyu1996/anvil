let () =
  let config = Anvil.Config.parse_args() in
  if config.input_filenames = [] then begin
    Printf.eprintf "Error: a file name must be supplied!\n";
    exit 1
  end else begin
    try
      Anvil.CompileDriver.compile stdout config
    with
    | Anvil.CompileDriver.CompileError (Some (file_name, span), msg) ->
      let open Anvil.Lang in
      Printf.eprintf "%s:%d:%d: error: %s\n" file_name span.st.pos_lnum (span.st.pos_cnum - span.st.pos_bol) msg;
      SpanPrinter.print_code_span stderr file_name span;
      exit 1
    | Anvil.CompileDriver.CompileError (None, msg) ->
      Printf.eprintf "error: %s\n" msg;
      exit 1
  end
