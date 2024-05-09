let () =
  let open Codegen in
  let lexbuf = Lexing.from_channel stdin in
  let cunit = Parser.cunit Lexer.read lexbuf in
  codegen cunit
