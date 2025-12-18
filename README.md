<p align="center">
  <a href="https://anvil.kisp-lab.org/" aria-label="AnvilHDL website">
    <picture>
      <img alt="AnvilHDL" src="docs/logo/anvil-horizontal.png" height="84">
    </picture>
  </a>
</p>

# A General-Purpose Timing-Safe Hardware Description Language

**AnvilHDL** is a hardware description language (HDL) that describes digital circuit designs at the register-transfer level (RTL). It introduces a novel type system to guarantee timing safety without abstracting away the low-level control required for efficient hardware design. Designs created in AnvilHDL interface easily with those in SystemVerilog.

> **Note** The motivation and design are discussed and evaluated in depth in our research paper: [**Anvil: A General-Purpose Timing-Safe Hardware Description Language**](https://arxiv.org/abs/2503.19447), which will appear at ASPLOS 2026.

-----

## Quick Start

### 1. Online Playground

The easiest way to experience AnvilHDL is in your browser.
[**Try the Online Playground**](https://anvil.kisp-lab.org/) (No installation required!)

### 2. Interactive Tutorial

Learn the language basics through our guided tour : [**Short Tutorial with interactive examples**](https://docs.anvil.kisp-lab.org/helloWorld.html)


### 3. Full Documentation
Up to date documentation is available online at [**AnvilHDL Docs**](https://docs.anvil.kisp-lab.org/)

-----

## Design Goals

AnvilHDL addresses critical challenges in RTL design through three core pillars:

| Feature | Description |
| :--- | :--- |
| **Timing-safety** | Any value referenced across cycles is guaranteed to be **stable** and **meaningful**. AnvilHDL prevents common RTL mistakes, such as using a value before it is ready or mutating a register while its dependent values must remain stable. |
| **Composability** | Timing safety is preserved across module boundaries. Designers can create modular components and compose them with confidence. AnvilHDL uses **dynamic timing contracts** to enforce timing properties between modules. |
| **Expressiveness** | Safety is achieved via the type system, not by hiding the hardware. AnvilHDL exposes registers, wires, and clock cycles, allowing the designer to retain **low-level control** suitable for general-purpose hardware development. |

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

This will set up the AnvilHDL compiler locally.

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

Run the AnvilHDL compiler using `dune`. **Note**: For a global installation, replace `dune exec anvil --` with `anvil`.

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

# Build with tracing enabled
# After simulation, you can find the waveform at logs/trace.vcd
make MODULE_NAME=<name> TRACE_ON=1

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

## Citing AnvilHDL

If you plan to cite AnvilHDL in your work, please use the following reference:

```text
J. Z. Yu, A. R. Jha, U. Mathur, T. E. Carlson, and P. Saxena, “Anvil: a general-purpose timing-safe hardware description language,” in Proceedings of the 31st ACM international conference on architectural support for programming languages and operating systems, in ASPLOS ’26. Pittsburgh, PA, USA: Association for Computing Machinery, 2026. [Online]. Available: https://arxiv.org/abs/2503.19447
```

or in BibTeX format:

```bibtex
@inproceedings{yu2026anvil,
   title={Anvil: A General-Purpose Timing-Safe Hardware Description Language},
   author={Yu, Jason Zhijingcheng and Jha, Aditya Ranjan and Mathur, Umang and Carlson, Trevor E and Saxena, Prateek},
   year={2026},
   publisher={Association for Computing Machinery},
   booktitle={Proceedings of the 31st ACM International Conference on Architectural Support for Programming Languages and Operating Systems},
   series={ASPLOS '26},
   address={Pittsburgh, PA, USA},
   url={https://arxiv.org/abs/2503.19447},
   note={To appear. Preprint available at arXiv:2503.19447}
}
```

## Discussing AnvilHDL

If you use AnvilHDL, we would love to hear your feedback!
Please join https://anvilhdl.zulipchat.com/ for discussions.

> **Acknowledgement**: We want to thank <img src="https://github.com/zulip/zulip/blob/main/static/images/logo/zulip-org-logo.svg" height="20" alt="Zulip" /> for sponsoring the community chat channel and allowing us to encourage discussions. Zulip is an organized team chat app designed for efficient communication.
