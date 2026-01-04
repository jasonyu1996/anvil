# AnvilHDL Examples

This directory contains AnvilHDL code examples that demonstrate core language features, idioms, and common design patterns. The examples are organized by category to make it easier to explore specific aspects of the language.

---

## Running the Examples

```bash
# Run tests for all examples
bash test-all.sh

# Get info about make reciepes
make help

# Build a single example (AnvilHDL + Verilator)
make MODULE_NAME=<example-name>

# Run the Verilator simulation for an example
make MODULE_NAME=<example-name> run

# Clean all build artifacts
make clean
```

---

## Examples by Category

Below is a categorized list of the available examples, along with brief descriptions of the language features they demonstrate.

---

### Language Constructs Examples

| Example                                                | Features                                                 |
| ------------------------------------------------------ | -------------------------------------------------------- |
| [counter2](counter2.anvil)                             | `reg`, `loop`, `set`, `dprint`, `dfinish`                |
| [if_comb](if_comb.anvil)                               | Combinational `if` / `else if` / `else` chains           |
| [if_else_val](if_else_val.anvil)                       | `if` as an expression returning a value                  |
| [match_test](match_test.anvil)                         | `match` pattern matching, `const`, `func` definitions    |
| [match_test_set](match_test_set.anvil)                 | `match` with `set` statements                            |
| [generate_test](generate_test.anvil)                   | Parallel `generate` unrolling                            |
| [gen_seq_test](gen_seq_test.anvil)                     | Sequential `generate_seq`                                |
| [general_recurse_simple](general_recurse_simple.anvil) | `recursive` / `recurse` blocks                           |
| [general_recurse_assign](general_recurse_assign.anvil) | Recursive definitions with assignments                   |
| [concurrent](concurrent.anvil)                         | Parallel fork-join using `join`                          |
| [import_example](import_example.anvil)                 | Module `import` syntax                                   |
| [simple_usereg_pipeline](simple_usereg_pipeline.anvil) | Pipeline example using registers                         |
| [subreg](subreg.anvil)                                 | Sub-register / bit-slice borrowing with constant indices |
| [subreg_var](subreg_var.anvil)                         | Sub-register / bit-slice borrowing with variable indices |
| [test_in](test_in.anvil)                               | `in` construct                                           |
| [test_match_endpoint](test_match_endpoint.anvil)       | State machine encoding using `match` and `func`          |

> **Total:** 16 examples

---

### Data-Type Examples

| Example                              | Features                                        |
| ------------------------------------ | ----------------------------------------------- |
| [enum_test](enum_test.anvil)         | `enum` types, `const` declarations              |
| [record](record.anvil)               | `struct` definitions and field initialization   |
| [record_update](record_update.anvil) | Struct update syntax `{v with field = value}`   |
| [list](list.anvil)                   | Array literals, register arrays, tuple creation |
| [assign_repeat](assign_repeat.anvil) | Record construction and repeated assignment     |

> **Total:** 5 examples

---

### Parametrization Examples

| Example                                        | Features                                    |
| ---------------------------------------------- | ------------------------------------------- |
| [param_int](param_int.anvil)                   | Integer parameters `<n : int>`              |
| [param_type](param_type.anvil)                 | Type parameters `<T : type>`                |
| [param_int_chan](param_int_chan.anvil)         | Parametric channels with integer parameters |
| [param_type_chan](param_type_chan.anvil)       | Parametric channels with type parameters    |
| [param_int_typedef](param_int_typedef.anvil)   | Parametric struct definitions               |
| [param_type_typedef](param_type_typedef.anvil) | Parametric type aliases                     |

> **Total:** 6 examples

---

### Channel Examples

| Example                                  | Features                                        |
| ---------------------------------------- | ----------------------------------------------- |
| [RCATop](RCATop.anvil)                   | Multi-channel communication, `shared` variables |
| [static_sync](static_sync.anvil)         | Static synchronization mode                     |
| [static_dyn_sync](static_dyn_sync.anvil) | Mixed static and dynamic synchronization        |
| [try_recv](try_recv.anvil)               | Non-blocking `try recv ... else`                |
| [test_ready](test_ready.anvil)           | `ready` channel state checks                    |
| [reset_msg](reset_msg.anvil)             | Built-in channel flush/reset                    |
| [test_void](test_void.anvil)             | `void` messages for pure synchronization        |
| [multi_send_recv](multi_send_recv.anvil) | Multiple sends/receives from the same endpoint  |
| [test_chan_array](test_chan_array.anvil) | Channel arrays and indexed access               |
| [test_channel_param_array](test_channel_param_array.anvil) | Test channel array range with parameters          |

> **Total:** 10 examples

---

### Lifetime Examples

| Example                                  | Features                               |
| ---------------------------------------- | -------------------------------------- |
| [test_borrow](test_borrow.anvil)         | Register field and slice borrowing     |
| [branch_borrow](branch_borrow.anvil)     | Borrowing across control-flow branches |
| [test_reg_borrow](test_reg_borrow.anvil) | Register borrow semantics              |

> **Total:** 3 examples

---

### FFI Examples

| Example                                                      | Features                                      |
| ------------------------------------------------------------ | --------------------------------------------- |
| [extern_fifo_top](extern_fifo_top.anvil), [fifo.sv](fifo.sv) | External FIFO integration using SystemVerilog |

> **Total:** 1 example

---

### Component Examples

| Example                                 | Features                            |
| --------------------------------------- | ----------------------------------- |
| [queue](queue.anvil)                    | Queue data-structure implementation |
| [cache](cache.anvil)                    | FIFO cache implementation           |
| [Arithmetic Processor](processor.anvil) | Simple arithmetic processor         |
| [AluExample](AluExample.anvil)          | Pipelined ALU                       |
| [RCA Example](imported_example.anvil)   | Ripple-carry adder using imports    |

> **Total:** 5 examples

---

## Example Subdirectories

* `edge_cases/` — Regression tests for bug fixes and corner cases
* `pipeline/` — Reusable pipeline component library
* `should_fail/` — Examples expected to fail type or borrow checking
* `should_pass/` — Examples expected to pass type and borrow checking

---

## Contributing Guidelines

When adding a new feature or example:

1. Add the example to the appropriate category above, along with a brief description of the demonstrated features.
2. Update the example counts for the affected category.
3. Update the **Current Example Count** below.
4. If fixing a bug, add a corresponding regression test to `edge_cases/`.
5. For type or borrow-checking demonstrations, place examples in `should_fail/` or `should_pass/` as appropriate.

Keeping this README up to date helps maintain clarity and usability for future users.

---

> **Current Example Count:** 46
