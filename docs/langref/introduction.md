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
- **Expressiveness:** Anvil provides the timing-safety guarantee through its type system without abstracting
    away distinct elements of RTL designs such as registers, wires, and clock cycles.
    It thus allows the designer to retain low-level control and
    is suitable for general purposes.

#### Comparison with Other HDLs

##### General-purpose HDLs: SystemVerilog, VHDL, Chisel, SpinalHDL, Bluespec, ...

Anvil provides _timing safety_ based on
the abstraction of stable and meaningful values.
For example, the following
AnvilHDL snippet indicates that the value of `r` needs to remain stable
across the two uses of `x` that are two cycles apart.

```
reg r : logic[8];
// ...
let x = *r;
set r2 := x >>
cycle 2 >>
set r3 := x
```

If the code attempts to mutate `r` between the two uses, the compiler will reject it.

```
reg r : logic[8];
// ...
let x = *r;
set r2 := x >>
cycle 2 >>
cycle r := 8'd1 >> // fails type checking
set r3 := x
```

AnvilHDL upholds such timing safety properties across multiple concurrent modules
through its type system.

Additionally, AnvilHDL adopts a concurrent programming model inspired by
communicating sequential processes (CSP) and the async-await paradigm, allowing
the design logic to explicitly encode control flow, advance time, and perform
synchronisation. The existing general-purpose HDLs, in contrast, require the designer
to manually encode a finite state machine (FSM) by specifying the computation
in a single clock cycle. The notion of time therein is implicit in the computation
being repeated for every clock cycle.

##### HDLs for pipelined designs: Filament, Spade, ...

Anvil is designed to be general-purpose and
therefore does not specifically target specific types of pipelined designs.
The designer can express pipelined designs by explicitly modelling stages as
concurrent modules as in most existing general-purpose HDLs (e.g., SystemVerilog
and VHDL). Alternatively, AnvilHDL can express certain pipelined
designs more conveniently through recursive threads. The example below shows
a pipelined design that can accept a request and start serving it every two cycles.

```
recursive {
    let req = recv e.req >>
    {
        // serve the request
    };
    cycle 2 >> recurse // wait for two cycles and start waiting for the next request
}
```

##### High-level HDLs: XLS, SystemC, Clash, ...

While providing timing safety, Anvil does _not_ abstract away the distinct elements
in hardware design such as registers, wires, and clock cycles.
Instead, the designer keeps control of whether a value is registered,
when a register is registered, when data is passed between modules, and so on.
This allows the designer to specify precise timing behaviour of the design, avoid
unnecessary cost, and fine-tune its resource usage.
This makes AnvilHDL suitable for general purposes.
