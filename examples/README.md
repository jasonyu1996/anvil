## AnvilHDL Examples

This directory contains AnvilHDL code examples
which serve both demonstration and testing purposes.

To test all examples, use
```
bash test-all.sh
```

To build a single example (AnvilHDL + Verilator),
```
make MODULE_NAME=<example-name>
```
Run Verilator simulation with
```
make MODULE_NAME=<example-name> run
```

### Interesting Examples

| Example name | Features demonstrated |
| -------------|-----------------------|
| [RCATop](RCATop.anvil) | Channel communication |
| [subreg](subreg.anvil) | Sub-register assignment |
| [subreg_var](subreg_var.anvil) | Sub-register assignment |
| [match_test](match_test.anvil) | `match` expression |
| [test_ready](test_ready.anvil) | `ready` expression |
| [generate_test](generate_test.anvil) | `generate` block |
| [param_int](param_int.anvil) | Parameterised processes |
| [param_type](param_type.anvil) | Parameterised processes |
| [param_int_chan](param_int_chan.anvil) | Parameterised channels |
| [param_type_chan](param_type_chan.anvil) | Parameterised channels |
| [param_int_typedef](param_int_typedef.anvil) | Parameterised types |
| [param_type_typedef](param_type_typedef.anvil) | Parameterised types |
| [import_example](import_example.anvil) | Importing modules |
| [extern_proc](extern_proc.anvil) | Importing SystemVerilog modules |
| [extern_proc2](extern_proc2.anvil) | Importing SystemVerilog modules |
| [static_dyn_sync](static_dyn_sync.anvil) | Static-dynamic sync modes |
| [static_sync](static_sync.anvil) | Static-static sync modes |
| [general_recurse_simple](general_recurse_simple.anvil) | Pipelining, recursive threads |
| [general_recurse_assign](general_recurse_assign.anvil) | Pipelining, recursive threads |
| [simple_usereg_pipeline](simple_usereg_pipeline.anvil) | Pipelining, recursive threads |
| [AluExample](AluExample.anvil) | Pipelining, recursive threads |

