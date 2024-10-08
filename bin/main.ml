let () =
  let handle_syntax_error filename lexbuf msg_opt =
    let pos = Lexing.lexeme_start_p lexbuf
    and msg = Option.value ~default:"Parsing error" msg_opt in
    Printf.eprintf "%s:%d:%d: syntax error: %s\n" filename pos.pos_lnum (pos.pos_cnum - pos.pos_bol) msg;
    SpanPrinter.print_code_span stderr filename (let open Anvil.Lang in {st = pos; ed = Lexing.lexeme_end_p lexbuf});
    exit 1
  in
  let config = Anvil.Config.parse_args() in
  if config.input_filenames = [] then begin
    Printf.eprintf "Error: a file name must be supplied!\n";
    exit 1
  end else begin
    let filename = List.hd config.input_filenames in
    let in_channel = open_in filename in
    let lexbuf = Lexing.from_channel in_channel in
    let cunit =
      try Parser.cunit Lexer.read lexbuf
      with
      | Lexer.SyntaxError msg -> handle_syntax_error filename lexbuf (Some msg)
      | _ -> handle_syntax_error filename lexbuf None
    in
    close_in in_channel;
    try
      let event_graphs = Anvil.EventGraph.build config cunit in
      Anvil.Codegen.generate stdout config event_graphs
    with
    | Anvil.EventGraph.LifetimeCheckError msg ->
      Printf.eprintf "Borrow checking failed: %s\n" msg;
      exit 1
    | Anvil.Except.TypeError msg -> Printf.eprintf "Type error: %s\n" msg; exit 1
    | Anvil.Except.UnimplementedError msg -> Printf.eprintf "Unimplemented error: %s\n" msg; exit 1
    | Anvil.EventGraph.EventGraphError (msg, span) ->
      let open Anvil.Lang in
      Printf.eprintf "%s:%d:%d: error: %s\n" filename span.st.pos_lnum (span.st.pos_cnum - span.st.pos_bol) msg;
      SpanPrinter.print_code_span stderr filename span;
      exit 1
  end
