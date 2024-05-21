let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: anvil <source-file>\n";
    exit 1
  end else begin
    let open Codegen in
    let in_channel = open_in Sys.argv.(1) in
    let lexbuf = Lexing.from_channel in_channel in
    let cunit = Parser.cunit Lexer.read lexbuf in
    close_in in_channel;
    try
      codegen cunit
    with
    | Codegen.BorrowCheckError msg -> Printf.eprintf "Borrow checking failed: %s\n" msg; exit 1
    | Lang.TypeError msg -> Printf.eprintf "Type error: %s\n" msg; exit 1
    | Codegen.CodegenError msg -> Printf.eprintf "Error during code generation: %s\n" msg; exit 1
    ;
  end
