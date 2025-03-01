### Lexical Conventions

Whitespace characters separate identifiers, literals, and keywords but
are otherwise ignored, except in string literals.

Two styles of comments are available. A block comment starts with `/*` and ends
with `*/`. It can be located in the middle of a line or span across multiple lines.
An inline comment starts with `//` and extends to the end of the line.

Identifiers consist of letters, digits, and underscores, but must start with either
a letter or an underscore (non-terminals are annotated with preceding `$`):
```
identifier ::= ( $letter | _ ) { $letter | $digit | _ }
letter ::= a...z | A...Z
digit ::= 0...9
```

A numeric literal starts with a sequence of digits specifying the number of bits, followed
by a quotation mark `'`, then by a `b`, `o`, `d` or `h` specifying the base, and finally by
a sequence specifying the value itself.
```
numeric-literal ::= $binary-literal | $octal-literal | $decimal-literal | $hexadecimal-literal
binary-literal ::= { $digit }+ 'b { 0 | 1 }+
octal-literal ::= { $digit }+ 'o { 0...7 }+
decimal-literal ::= { $digit }+ 'd { $digit }+
hexadecimal-literal ::= { $digit }+ 'h { $digit | a...f | A...F }+
```

A string literal is enclosed by a pair of `"`:
```
string-literal ::= " { $string-char } "
```


