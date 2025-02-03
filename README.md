## Anvil HDL

### Dependencies

Development: OCaml 5.2.0

Software simulation tested with Verilator 5.024.

### Usage

```
dune exec anvil -- [-verbose] [-disable-lt-checks] [-two-round] <anvil-source-file>
```

NOTE: To disable lifetime-related checks, pass `-disable-lt-checks`. The `-two-round` flag
generates code for two rounds of each thread to work around some combinational loop issues
codegen currently has (see https://github.com/jasonyu1996/anvil/issues/33).

### Examples

Example designs are located in the `examples` directory.
To try them out, make sure Verilator is installed and in
your `$PATH`.

To build an example design,
```
make MODULE_NAME=<name>
```

To run a design,
```
make run MODULE_NAME=<name>
```

To clean up,
```
make clean MODULE_NAME=<name>
```

`MODULE_NAME` defaults to `top` if unspecified.


### Tests

To run all tests, use
```
python3 run-tests.py
```

Alternatively, you can run type checking tests and simulation tests separately with
```
cd examples
sh typecheck-test.sh
bash test-all.sh
```

### Documentations

To build the documentations,
```
dune build @doc
```

To host them locally through Python's embedded web server:
```
sh host-docs.sh
```
