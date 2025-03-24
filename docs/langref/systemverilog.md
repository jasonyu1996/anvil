### Interfacing with SystemVerilog

#### Importing SystemVerilog Modules To AnvilHDL

SystemVerilog modules can be imported through `extern import` statements.
```
extern-import ::= extern import $string-literal
```

The string literal is the path to a SystemVerilog source file containing
modules to be imported.

Once imported, a SystemVerilog module needs to be wrapped in an AnvilHDL
process to be used. One can define an AnvilHDL process that wraps a
SystemVerilog module:

```
extern-proc-def ::= proc $identifier [$params] (($identifier : (left | right) $identifier [$param-vals]) | ()) extern($string-literal) { { (clk_i($string-literal) | rst_ni($string-literal) | $identifier.$identifier($string-literal : $string-literal : $string-literal) ) ; } }
```

The string literal after `extern` is the name of the SystemVerilog module.
Inside the body, each line defines a mapping between ports of the SystemVerilog
module and entities in the AnvilHDL process, where string literals are
the names of the ports in SystemVerilog.
Clock and reset signals are mapped with `clk_i(<port>)` and `rst_ni(<port>)`, whereas
messages in endpoints are mapped with `<endpoint>.<message>(<data-port> : <valid-port> : <ack-port>)`.

