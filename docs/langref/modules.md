### Modules

```
import-statement ::= import $string-literal
```

The `import` statement imports all definitions
in another source file. It also recursively imports
definitions the file imports. If a relative path
is provided, it is resolved relative to the current
source file path.
