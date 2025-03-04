### Compiler

The compiler can be invoked using the command line.

```
anvil [-verbose] [-disable-lt-checks] [-two-round]  <file1> [<file2>] ...
```

The compiler processes the specified source files. If the compilation succeeds, it outputs the generated SystemVerilog code to stdout.
It writes errors and other messages to stderr.

The command-line flags adjust the compilation process:
* `-verbose`: print out verbose details for debugging purposes
* `-disable-lt-checks`: suppress all lifetime checks (note that the compilation result may no longer be timing-safe)
* `-two-round`: generate two iterations of each loop thread in the output (deprecated, no more useful)
