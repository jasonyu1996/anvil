### Compiler

The compiler can be invoked using the command line.

```
anvil [-verbose] [-disable-lt-checks] [-O <opt-level>] [-two-round] [-json] <file1> [<file2>] ...
```

The compiler processes the specified source files. If the compilation succeeds, it outputs the generated SystemVerilog code to stdout.
It writes errors and other messages to stderr.

The command-line flags adjust the compilation process:
* `-verbose`: print out verbose details for debugging purposes
* `-disable-lt-checks`: suppress all lifetime checks (note that the compilation result may no longer be timing-safe)
* `-O <opt-level>`: set the optimisation level for codegen to `opt-level`, effective values being 0, 1, and 2, with higher optimisation level enabling more aggressive optimisations
* `-two-round`: generate two iterations of each loop thread in the output (deprecated, no more useful)
* `-json`: output compilation results in JSON format instead of plain text

#### JSON Output Format

When the `-json` flag is used, the compiler outputs a JSON object with the following structure:

```json
{
  "success": boolean,
  "errors": [
    {
      "type": "warning" | "error",
      "path": string | null,
      "trace": {
        "line": number,
        "col": number,
        "len": number
      } | null,
      "desc": string
    }
  ],
  "output": string | null
}
```

* `success`: `true` if compilation succeeded, `false` otherwise
* `errors`: array of error/warning objects
* `output`: the generated SystemVerilog code (only present on success)
