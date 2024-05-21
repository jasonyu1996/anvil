let () =
  let config = Config.parse_args() in
  if config.input_filenames = [] then begin
    Printf.eprintf "Error: a file name must be supplied!\n";
    exit 1
  end else begin
    let open Codegen in
    let in_channel = List.hd config.input_filenames |> open_in in
    let lexbuf = Lexing.from_channel in_channel in
    let cunit = Parser.cunit Lexer.read lexbuf in
    close_in in_channel;
    try
      codegen cunit config
    with
    | Codegen.BorrowCheckError msg -> Printf.eprintf "Borrow checking failed: %s\n" msg; exit 1
    | Lang.TypeError msg -> Printf.eprintf "Type error: %s\n" msg; exit 1
    | Codegen.CodegenError msg -> Printf.eprintf "Error during code generation: %s\n" msg; exit 1
    ;
  end
