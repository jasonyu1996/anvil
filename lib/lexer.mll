{
  open Parser
  exception SyntaxError of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
(* only non-negatives *)
let int = ['0'-'9']+
let bit_literal = int "\'b" ['0'-'1']+
let dec_literal = int "\'d" ['0'-'9']+
let hex_literal = int "\'h" ['0'-'9' 'a'-'f']+

rule read =
  parse
  | white     { read lexbuf }
  | newline   { Lexing.new_line lexbuf; read lexbuf }
  | '{'       { LEFT_BRACE }
  | '}'       { RIGHT_BRACE }
  | '['       { LEFT_BRACKET }
  | ']'       { RIGHT_BRACKET }
  | '('       { LEFT_PAREN }
  | ')'       { RIGHT_PAREN }
  | ','       { COMMA }
  | ':'       { COLON }
  | ';'       { SEMICOLON }
  | '#'       { SHARP }
  | '*'       { ASTERISK }
  | '='       { EQUAL }
  | '+'       { PLUS }
  | '-'       { MINUS }
  | "--"      { DOUBLE_MINUS }
  | '^'       { XOR }
  | '&'       { AND }
  | '|'       { OR }
  | '@'       { AT }
  | '~'       { TILDE }
  | '.'       { PERIOD }
  | ":="      { COLON_EQ }
  | "::"      { DOUBLE_COLON }
  | "<"       { LEFT_ABRACK }
  | ">"       { RIGHT_ABRACK }
  | "<="      { LEFT_ABRACK_EQ }
  | ">="      { RIGHT_ABRACK_EQ }
  | "=>"      { EQ_GT }
  | ">>"      { DOUBLE_GT }
  | "<<"      { DOUBLE_LEFT_ABRACK }
  | ">>"      { DOUBLE_RIGHT_ABRACK }
  | "=="      { DOUBLE_EQ }
  | "!="      { EXCL_EQ }
  | "ready"   { KEYWORD_READY }
  | "probe"   { KEYWORD_PROBE }
  | "put"     { KEYWORD_PUT }
  | "shared"  { KEYWORD_SHARED }
  | "assigned" { KEYWORD_ASSIGNED }
  | "by"      { KEYWORD_BY }
  | "func"    { KEYWORD_FUNCTION }
  | "call"    { KEYWORD_CALL }
  | "loop"    { KEYWORD_LOOP }
  | "proc"    { KEYWORD_PROC }
  | "chan"    { KEYWORD_CHAN }
  | "left"    { KEYWORD_LEFT }
  | "right"   { KEYWORD_RIGHT }
  | "logic"   { KEYWORD_LOGIC }
  | "foreign" { KEYWORD_FOREIGN }
  | "let"     { KEYWORD_LET }
  | "if"      { KEYWORD_IF }
  | "else"    { KEYWORD_ELSE }
  | "send"    { KEYWORD_SEND }
  | "recv"    { KEYWORD_RECV }
  | "eternal" { KEYWORD_ETERNAL }
  | "type"    { KEYWORD_TYPE }
  | "set"     { KEYWORD_SET }
  | "match"   { KEYWORD_MATCH }
  | "dyn"     { KEYWORD_DYN }
  | "sync"    { KEYWORD_SYNC }
  | "cycle"   { KEYWORD_CYCLE }
  | "reg"     { KEYWORD_REG }
  | "const"   { KEYWORD_CONST }
  | "spawn"   { KEYWORD_SPAWN }
  | "dprint"  { KEYWORD_DPRINT }
  | "dfinish" { KEYWORD_DFINISH }
  | "import"  { KEYWORD_IMPORT }
  | "extern"  { KEYWORD_EXTERN }
  | "int"     { KEYWORD_INT }
  | "generate" { KEYWORD_GENERATE }
  | "generate_seq" { KEYWORD_GENERATE_SEQ }
  | "recursive" { KEYWORD_RECURSIVE }
  | "recurse" { KEYWORD_RECURSE }
  | "struct"  { KEYWORD_STRUCT }
  | "enum"    { KEYWORD_ENUM }
  | "with"    { KEYWORD_WITH }
  | "try"     { KEYWORD_TRY }
  | int       { let n = Lexing.lexeme lexbuf |> int_of_string in INT n }
  | ident     { IDENT (Lexing.lexeme lexbuf) }
  | bit_literal { BIT_LITERAL (Lexing.lexeme lexbuf) }
  | dec_literal { DEC_LITERAL (Lexing.lexeme lexbuf) }
  | hex_literal { HEX_LITERAL (Lexing.lexeme lexbuf) }
  | eof       { EOF }
  | "/*"      { skip_comments lexbuf }
  | "//"      { skip_inline_comments lexbuf }
  | '"'       { read_string (Buffer.create 32) lexbuf }
  | _         { raise (SyntaxError (Lexing.lexeme lexbuf |> Printf.sprintf "Unknown character: %s"))}
and skip_comments =
  parse
  | newline   { Lexing.new_line lexbuf; skip_comments lexbuf }
  | "*/"      { read lexbuf }
  | _         { skip_comments lexbuf }
  | eof       { raise (SyntaxError "Missing */") }
and skip_inline_comments =
  parse
  | newline   { Lexing.new_line lexbuf; read lexbuf }
  | _         { skip_inline_comments lexbuf }
  | eof       { EOF }
and read_string buf =
  parse
  | newline { raise (SyntaxError "End of line in string") }
  | eof     { raise (SyntaxError "End of file in string") }
  | '"'     { STR_LITERAL (Buffer.contents buf) }
  | "\\\""  { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | _       { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
