## Anvil HDL

Anvil is a hardware description language (HDL) which
describes digital circuit designs at register-transfer level (RTL).
Anvil has the following design goals:

- **Timing-safety:** Any value that the designer intends to reference across multiple
    cycles is always _stable_ and _meaningful._ Anvil prevents mistakes in the RTL design
    such as using a value before it is ready and mutating a register when a value that
    depends on it is intended to be kept stable.
- **Composability:** The timing-safety guarantee that Anvil provides is _composable._
    The designer can create modular designs separately and compose them together.
    Anvil ensures that the design that results from composition is timing-safe.
    It achieves this with _dynamic timing contracts,_ which specify the expected timing
    properties of values across modules.
- **Expressiveness:** Anvil provides the timing-safety guarantee through its type system without abstracting
    away distinct elements of RTL designs such as registers, wires, and clock cycles.
    It thus allows the designer to retain low-level control and
    is suitable for general purposes.

### Status

Currently, AnvilHDL is _experimental._

- Many aspects of the language might change in
  the future. Use at your own risk.
- Seen something you don't like? Feel free to contribute your ideas or code!

### Dependencies

Development: OCaml 5.2.0

Software simulation tested with Verilator 5.024.

### Usage

```
dune exec anvil -- [-verbose] [-disable-lt-checks] [-two-round] <anvil-source-file>
```

NOTE: To disable lifetime-related checks, pass `-disable-lt-checks`. The `-two-round` flag
generates code for two rounds of each thread (previously used to
work around some [combinational loop issues](https://github.com/jasonyu1996/anvil/issues/33) codegen had, now no more useful).

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

### Editor Support

#### Visual Studio Code

We have an extension for Visual Studio Code that provides syntax highlighting.
See how to install [here](editors/vscode/README.md).

### Documentations

#### User Documentation

* [Tutorial](docs/tutorial/README.md)
* [Code examples](examples/README.md)
* [Language reference manual](docs/langref/README.md)

#### API Documentation

The API documentation of the compiler can be built with
```
dune build @doc
```

To host them locally through Python's embedded web server:
```
sh host-docs.sh
```

### Contributing

We welcome anyone to
**report bugs, propose new ideas, and request features**
using [Github Issues](https://github.com/jasonyu1996/anvil/issues).
Also feel free to submit your code contribution [here](https://github.com/jasonyu1996/anvil/compare).
