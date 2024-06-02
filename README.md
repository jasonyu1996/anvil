## Anvil HDL

### Dependencies

Development: OCaml 4.13.1

Software simulation tested with Verilator 5.024.

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
