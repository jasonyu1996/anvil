### Introduction

#### Purpose

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
- **Expressiveness:** Anvil provides the timing-safety guarantee without abstracting
    away distinct elements of RTL designs such as registers, wires, and clock cycles.
    It thus allows the designer to retain low-level control and
    is suitable for general purposes.

#### Comparison with Other HDLs

##### General-purpose HDLs: SystemVerilog, VHDL, Chisel, SpinalHDL, Bluespec

##### HDLs for pipelined designs: Filament, Spade

##### High-level HDLs: XLS
