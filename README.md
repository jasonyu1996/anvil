# AnvilHDL: A Timing-Safe Hardware Description Language

**Anvil** is a hardware description language (HDL) that describes digital circuit designs at the register-transfer level (RTL). It introduces a novel type system to guarantee timing safety without abstracting away the low-level control required for efficient hardware design.

> **Note** The motivation and design are discussed and evaluated in depth in our research paper: [**Anvil: A General-Purpose Timing-Safe Hardware Description Language**](https://arxiv.org/abs/2503.19447) which will apear at ASPLOS 2026.

-----

## Quick Start

### 1. Online Playground

The easiest way to experience AnvilHDL is in your browser. 
[**Try the Online Playground**](https://anvil.capstone.kisp-lab.org/) (No installation required!)

### 2. Interactive Tutorial

Learn the language basics through our guided tour : [**Short Tutorial with interactive examples**](https://project-starch.github.io/Anvil-Docs/helloWorld.html)


### 3. Full Documentation
Up to date documentation is available online at [**AnvilHDL Docs**](https://project-starch.github.io/Anvil-Docs/)

-----

## Design Goals

Anvil addresses critical challenges in RTL design through three core pillars:

| Feature | Description |
| :--- | :--- |
| **Timing-safety** | Any value referenced across cycles is guaranteed to be **stable** and **meaningful**. Anvil prevents common RTL mistakes, such as using a value before it is ready or mutating a register while its dependent values must remain stable. |
| **Composability** | Timing safety is preserved across module boundaries. Designers can create modular components and compose them with confidence. Anvil uses **dynamic timing contracts** to enforce timing properties between modules. |
| **Expressiveness** | Safety is achieved via the type system, not by hiding the hardware. Anvil exposes registers, wires, and clock cycles, allowing the designer to retain **low-level control** suitable for general-purpose hardware development. |

-----

## Local Installation

For in-depth exploration or contribution, install AnvilHDL locally.

### Prerequisites

- **OCaml**: Version 5.2.0
- **Verilator**: Version 5.024 (Required for software simulation)
- **Opam**: [Installation Guide](https://opam.ocaml.org/)

### Build Instructions

Clone the repository and install dependencies:

```bash
# Install dependencies
opam install . --deps-only

# Build the project
eval $(opam env) && dune build  # or for release build: dune build --release
```

This will set up the Anvil compiler locally.

For global installation, you can run:

```bash
# To install AnvilHDL binary globally
opam install .

# To uninstall AnvilHDL
opam uninstall .

# To update AnvilHDL binary to the latest commits (HEAD)
opam reinstall .
```


-----

## Compiler Usage

Run the Anvil compiler using `dune`. **Note**: For a global installation, replace `dune exec anvil --` with `anvil`.

```bash
dune exec anvil -- [OPTIONS] <anvil-source-file>
```

**Common Options:**

  - `-disable-lt-checks`: Disable lifetime-related checks.
  - `-O <opt-level>`: Specify optimization level. (Currently : 0, 1, 2)
  - `-verbose`: Enable verbose output.
  - `-o <output-file>`: Specify output file name.
  - `-just-check`: Only type-check the source file without generating code.
  - `-json-output` : Output compilation results in JSON format.
  - `-strict-dtc`: Enable strict data type checks (prevents abstract data types conversion).
  - `-help`: Display help information.



### Running Examples

Example designs are located in the `examples` directory. Ensure Verilator is in your `$PATH`.

```bash
cd examples

# Build a specific module (defaults to 'top' if unspecified)
make MODULE_NAME=<name>

# Simulate the design
make run MODULE_NAME=<name> TIMEOUT=1000

# Clean up build artifacts
make clean
```

### Running Tests

```bash
# Run all tests
python3 run-tests.py

# Run specific test suites
cd examples
sh typecheck-test.sh   # Type checking tests
bash test-all.sh       # Simulation tests
```


-----

## Contributing

AnvilHDL is currently **experimental** and under active development. We welcome feedback and contributions\!

  - **Found a bug?** [Open an Issue](https://github.com/jasonyu1996/anvil/issues/new?assignees=&labels=bug&template=bug_report.md&title=)
  - **Need a feature?** [Request a Feature](https://github.com/jasonyu1996/anvil/issues/new?assignees=&labels=enhancement&template=feature_request.md&title=)
  - **Want to contribute?** [Submit a Pull Request](https://github.com/jasonyu1996/anvil/compare)
  - **Editor Support:** We provide a [Visual Studio Code extension](editors/vscode/README.md) for syntax highlighting.

-----

## Citation

If you use AnvilHDL in your research, please cite the following paper:

```text
J. Z. Yu, A. R. Jha, U. Mathur, T. E. Carlson, and P. Saxena, ‘Anvil: A General-Purpose Timing-Safe Hardware Description Language’, Mar. 25, 2025, arXiv: arXiv:2503.19447. doi: 10.48550/arXiv.2503.19447.
```

or in BibTeX format:

```bibtex
@article{yu2025anvil,
   title={Anvil: A General-Purpose Timing-Safe Hardware Description Language},
   author={Yu, Jason Zhijingcheng and Jha, Aditya Ranjan and Mathur, Umang and Carlson, Trevor E and Saxena, Prateek},
   journal={arXiv preprint arXiv:2503.19447},
   year={2025}
}
```