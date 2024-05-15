let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: anvil <source-file>\n";
    exit 1
  end else begin
    let open Codegen in
    let in_channel = open_in Sys.argv.(1) in
    let lexbuf = Lexing.from_channel in_channel in
    let cunit = Parser.cunit Lexer.read lexbuf in
    codegen cunit;
    close_in in_channel
  end
