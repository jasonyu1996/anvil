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
  | '.'       { PERIOD }
  | '='       { EQUAL }
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '^'       { XOR }
  | '&'       { AND }
  | '|'       { OR }
  | '@'       { AT }
  | "->"      { POINT_TO }
  | "=>"      { D_POINT_TO }
  | "<-"      { POINT_BACK }
  | "::"      { DOUBLE_COLON }
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
  | int       { let n = Lexing.lexeme lexbuf |> int_of_string in INT n }
  | ident     { IDENT (Lexing.lexeme lexbuf) }
  | bit_literal { LITERAL (Lexing.lexeme lexbuf) }
  | eof       { EOF }
  | "/*"      { skip_comments lexbuf }
and skip_comments =
  parse
  | "*/"      { read lexbuf }
  | _         { skip_comments lexbuf }
  | eof       { raise (SyntaxError "Missing */") }
