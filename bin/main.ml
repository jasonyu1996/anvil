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
    let open Anvil.Codegen in
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
      codegen cunit config
    with
    | Anvil.Codegen.BorrowCheckError msg -> Printf.eprintf "Borrow checking failed: %s\n" msg; exit 1
    | Anvil.Lang.TypeError msg -> Printf.eprintf "Type error: %s\n" msg; exit 1
    | Anvil.Codegen.CodegenError msg -> Printf.eprintf "Error during code generation: %s\n" msg; exit 1
    ;
  end
