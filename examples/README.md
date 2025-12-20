# AnvilHDL Examples

This directory contains AnvilHDL code examples demonstrating language features and idioms.

## Running Examples

```bash
# Test all examples
bash test-all.sh

# Build a single example (AnvilHDL + Verilator)
make MODULE_NAME=<example-name>

# Run Verilator simulation
make MODULE_NAME=<example-name> run

# Clean build artifacts
make clean
```

---

## Examples by Category

### Basic Examples : Language Constructs

| Example | Features |
|---------|----------|
| [counter2](counter2.anvil) | `reg`, `loop`, `set`, `dprint`, `dfinish` |
| [multicycle_counter](multicycle_counter.anvil) | `cycle N` multi-cycle delays |
| [if_comb](if_comb.anvil) | Combinational `if`/`else if`/`else` chains |
| [if_else_val](if_else_val.anvil) | `if` as expression returning value |
| [match_test](match_test.anvil) | `match` pattern matching, `const`, `func` definitions |
| [generate_test](generate_test.anvil) | Parallel `generate` unrolling |
| [gen_seq_test](gen_seq_test.anvil) | Sequential `generate_seq` |
| [general_recurse_simple](general_recurse_simple.anvil) | `recursive`/`recurse` blocks |
| [general_recurse_assign](general_recurse_assign.anvil) | Recursive with assignments |
| [concurrent](concurrent.anvil) | Tests parallel fork-join with `join` |
| [import_export](import_export.anvil) | Module `import` syntax |
| [simple_usereg_pipeline](simple_usereg_pipeline.anvil) | Another Pipeline Example |
| [AluExample](AluExample.anvil) | Pipelined ALU example |


### Data-Types & Structures

| Example | Features |
|---------|----------|
| [enum_test](enum_test.anvil) | `enum` types, `const` declarations |
| [record](record.anvil) | `struct` definition, field initialization syntax |
| [record_update](record_update.anvil) | Struct update `{v with field = value}` |
| [list](list.anvil) | Array literals, register arrays, Tupple Creation |
| [assign_repeat](assign_repeat.anvil) | record construction |

### Parametric Data-Types

| Example | Features |
|---------|----------|
| [param_int](param_int.anvil) | Integer parameter `<n : int>` |
| [param_type](param_type.anvil) | Type parameter `<T : type>` |
| [param_int_chan](param_int_chan.anvil) | Parametric channels with int |
| [param_type_chan](param_type_chan.anvil) | Parametric channels with type |
| [param_int_typedef](param_int_typedef.anvil) | Parametric struct definition |
| [param_type_typedef](param_type_typedef.anvil) | Parametric type aliases (type) |

### Channels

| Example | Features |
|---------|----------|
| [RCATop](RCATop.anvil) | Multi-channel communication, `shared` variables |
| [static_sync](static_sync.anvil) | Sync Mode Example |
| [static_dyn_sync](static_dyn_sync.anvil) | Sync Mode Example  |
| [try_recv](try_recv.anvil) | Non-blocking `try recv ... else` |
| [test_ready](test_ready.anvil) | `ready` channel state check |
| [reset_msg](reset_msg.anvil) | Language inbuilt flush feature |
| [test_void](test_void.anvil) | `void` messages in channels for pure sync |
| [multi_send_recv](multi_send_recv.anvil) | Multiple sends/receives from save ep |
| [test_chan_array](test_chan_array.anvil) | Channel arrays, indexed access |

### Lifetime Examples

| Example | Features |
|---------|----------|
| [test_borrow](test_borrow.anvil) | Register field borrowing |
| [branch_borrow](branch_borrow.anvil) | Borrowing in control flow branches |
| [test_reg_borrow](test_reg_borrow.anvil) | Register borrow semantics |
| [subreg](subreg.anvil) | Sub-register/bit slice borrow |

### FFI Examples

| Example | Features |
|---------|----------|
| [extern_fifo_top](extern_fifo_top.anvil) | External FIFO integration |
| [fifo.sv](fifo.sv) | SystemVerilog FIFO module | FIFO module written in SystemVerilog |


### Language Constructs

| Example | Features |
|---------|----------|





### Component Examples (Inefficient Implementations)

| Example | Features |
|---------|----------|
| [queue](queue.anvil) | Queue data structure implementation |
| [cache](cache.anvil) | FIFO cache implementation |
| [Arithmetic Processor](processor.anvil) | Arithmetic Processor | 

---

## Example Subdirectories

- `edge_cases/` - Regression tests for bug fixes and edge cases
- `pipeline/` - Pipeline component library
- `should_fail/` - Examples that should fail type/borrow checking
- `should_pass/` - Examples that should pass checking

