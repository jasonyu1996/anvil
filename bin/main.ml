let () =
  let handle_syntax_error lexbuf msg_opt =
    let pos = Lexing.lexeme_start_p lexbuf
    and msg = Option.value ~default:"Parsing error" msg_opt in
    Printf.eprintf "Syntax error: %s (line %d, col %d)\n" msg pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
    exit 1
  in
  let config = Anvil.Config.parse_args() in
  if config.input_filenames = [] then begin
    Printf.eprintf "Error: a file name must be supplied!\n";
    exit 1
  end else begin
    let in_channel = List.hd config.input_filenames |> open_in in
    let lexbuf = Lexing.from_channel in_channel in
    let cunit =
      try Parser.cunit Lexer.read lexbuf
      with
      | Lexer.SyntaxError msg -> handle_syntax_error lexbuf (Some msg)
      | _ -> handle_syntax_error lexbuf None
    in
    close_in in_channel;
    try
      let event_graphs = Anvil.EventGraph.build cunit in
      Anvil.Codegen.generate stdout config event_graphs
    with
    | Anvil.EventGraph.BorrowCheckError (msg, lt, req) ->
      Printf.eprintf "Borrow checking failed: %s\n  Lifetime = %s\n  Required = %s\n" msg
        (Anvil.EventGraph.string_of_plain_lifetime lt)
        (Anvil.EventGraph.string_of_plain_lifetime req);
      exit 1
    | Anvil.Except.TypeError msg -> Printf.eprintf "Type error: %s\n" msg; exit 1
    | Anvil.Except.UnimplementedError msg -> Printf.eprintf "Unimplemented error: %s\n" msg; exit 1
  end
