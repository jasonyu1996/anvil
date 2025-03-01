### Functions

Functions provide a means of code reuse.
Although we call them functions, in the current version,
they are more akin to macros at the expression AST level.

```
function-definition ::= func $identifier ( [$identifier {, $identifier}] ) { $expression }
```

Calling a function simply substitutes the call in place with the function body,
with the extra bindings specified in the parameters:

```
call-expression ::= call $identifier ( [$expression {, $expression}] )
```

