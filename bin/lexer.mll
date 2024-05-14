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
let no_length_literal = int

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
  | '='       { EQUAL }
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '^'       { XOR }
  | '&'       { AND }
  | '|'       { OR }
  | '@'       { AT }
  | '~'       { TILDE }
  | "->"      { POINT_TO }
  | "=>"      { D_POINT_TO }
  | ":="      { COLON_EQ }
  | "::"      { DOUBLE_COLON }
  | "<"       { LEFT_ABRACK }
  | ">"       { RIGHT_ABRACK }
  | "<="      { LEFT_ABRACK_EQ }
  | ">="      { RIGHT_ABRACK_EQ }
  | "<<"      { DOUBLE_LEFT_ABRACK }
  | ">>"      { DOUBLE_RIGHT_ABRACK }
  | "=="      { DOUBLE_EQ }
  | "!="      { EXCL_EQ }
  | "proc"    { KEYWORD_PROC }
  | "chan"    { KEYWORD_CHAN }
  | "in"      { KEYWORD_IN }
  | "out"     { KEYWORD_OUT }
  | "left"    { KEYWORD_LEFT }
  | "right"   { KEYWORD_RIGHT }
  | "logic"   { KEYWORD_LOGIC }
  | "foreign" { KEYWORD_FOREIGN }
  | "let"     { KEYWORD_LET }
  | "if"      { KEYWORD_IF }
  | "then"    { KEYWORD_THEN }
  | "else"    { KEYWORD_ELSE }
  | "while"   { KEYWORD_WHILE }
  | "send"    { KEYWORD_SEND }
  | "recv"    { KEYWORD_RECV }
  | "return"  { KEYWORD_RETURN }
  | "ref"     { KEYWORD_REF }
  | "eternal" { KEYWORD_ETERNAL }
  | "done"    { KEYWORD_DONE }
  | "try_send"{ KEYWORD_TRYSEND }
  | "try_recv"{ KEYWORD_TRYRECV }
  | int       { let n = Lexing.lexeme lexbuf |> int_of_string in INT n }
  | ident     { IDENT (Lexing.lexeme lexbuf) }
  | bit_literal { BIT_LITERAL (Lexing.lexeme lexbuf) }
  | dec_literal { DEC_LITERAL (Lexing.lexeme lexbuf) }
  | hex_literal { HEX_LITERAL (Lexing.lexeme lexbuf) }
  | no_length_literal { NO_LEN_LITERAL (Lexing.lexeme lexbuf) }
  | eof       { EOF }
  | "/*"      { skip_comments lexbuf }
  | "//"      { skip_inline_comments lexbuf }
and skip_comments =
  parse
  | "*/"      { read lexbuf }
  | _         { skip_comments lexbuf }
  | eof       { raise (SyntaxError "Missing */") }
and skip_inline_comments =
  parse
  | newline  { Lexing.new_line lexbuf; read lexbuf }
  | _         { skip_inline_comments lexbuf }
  | eof       { EOF }
