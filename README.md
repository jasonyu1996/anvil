## Anvil HDL

### Dependencies

Development: OCaml 5.2.0

Software simulation tested with Verilator 5.024.

### Usage

```
dune exec anvil -- [-verbose] [-disable-lt-checks] <anvil-source-file>
```

NOTE: To disable lifetime-related checks, pass `-disable-lt-checks`.

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

To run tests on type checking, go to the `example` directory
and
```
sh typecheck-test.sh
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
