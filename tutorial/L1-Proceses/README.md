# Lesson 1: Hello World in Anvil


## 1. Hello World in Anvil!
In Anvil, hardware components are modeled as processes. These processes store state in registers, which are programming abstractions of actual hardware registers. Each process description defines the behavior of a hardware component inside the process body. With this background, you’re ready to write your first program in Anvil!

```rust
proc hello_world() {
    reg counter : logic[8];
    loop {
        set counter := *counter + 1;
        dprint"[Cycle %d] Hello World in Anvil!" (*counter)
    }
}
```

### Understanding the Program  

- The `proc` keyword defines a process.  
- `reg counter : logic[8];` declares an 8-bit register named `counter`.  
- `set counter := *counter + 1;` increments the register.  
- `dprint` prints the value of `counter` to the console.  
- The `*` operator dereferences the register value.  

If you run this program using the Makefile in your workspace directory:

```bash
make run MODULE_NAME=hello_world
```

You will see the following output:

```bash
[Cycle   0] Hello World in Anvil!
[Cycle   1] Hello World in Anvil!
[Cycle   2] Hello World in Anvil!
...
[Cycle 255] Hello World in Anvil!
[Cycle   0] Hello World in Anvil!
```

This program runs indefinitely, which might feel strange if you're new to hardware programming. Unlike software, hardware processes are concurrent and do not terminate unless explicitly stopped. The `loop` keyword describes a concurrent process, meaning the behavior defined inside it repeats when all expressions inside it have finished evaluation.

### Common Questions

**1. Are the expressions evaluated sequentially?** \
No. In Anvil, expressions separated by `;` are evaluated concurrently. This means that the register increment and the print statement start in the same clock cycle.  

**2. How long does each iteration of the loop take?** \
A loop iteration completes when all expressions in the loop body have finished executing. In this case:  
- The register update takes one cycle to complete (related to actual semantics of RTL).
- The evalutation of `dprint` statement completes immediately.

Thus, the iteration time is determined by the longest operation, which is one cycle in this case.

**3. Are the registers initialized to 0?** \
Yes all the registers are by default initialized to zero. 




## 2. Using Intermediate Values  

In the previous example, we used the `*` operator to dereference a register’s value. This means register values persist across cycles unless explicitly updated.  

To work with immediate values, Anvil provides the `let` construct. It creates a new variable that holds the result of an expression. Consider the following Anvil process:  

```rust
proc foo() {
    reg counter : logic[8];
    loop {
        let temp = *counter + 1;
        set counter := temp;
        dprint"[Cycle %d] Hello World in Anvil!" (temp);
    }
}
```

When you run this, the output looks similar to the previous example:  

```bash
[Cycle   1] Hello World in Anvil!
[Cycle   2] Hello World in Anvil!
[Cycle   3] Hello World in Anvil!
...
[Cycle 255] Hello World in Anvil!
[Cycle   0] Hello World in Anvil!
```

However, notice the difference: This time, the simulation starts from Cycle 1 instead of Cycle 0.  

This happens because `let id = expr` depends on how long `expr` takes to evaluate. In this case, `*counter + 1` is immediate, so `temp` gets its value within the same cycle. Also, Anvil infers the data type of `temp` from the expression. Since `counter` is `logic[8]`, `temp` overflows back to `0` after `255`. 



## 3. Understanding Timing in RTL designs 

In hardware design or RTL design, timing is measured in clock cycles. When we say an expression takes one cycles, it means the new value is available in the next cycle. If a value is immediate, it means the result is available at the beginning of the same cycle.  

For example, consider this SystemVerilog version of the same program:

```verilog
module hello_world();
    logic[7:0] counter;
    logic[7:0] temp;
    assign temp = counter + 1;
    always_ff @(posedge clk or negedge rst_ni) begin
        if (~rst_ni) begin
            counter <= 0;
        end else begin
            counter <= temp;
            $display("[Cycle %d] Hello World in SV!", temp);
        end
    end
endmodule
```  

The `always_ff` block executes on the positive edge of the clock or the negative edge of the reset signal. While the specific details of these signals aren't important right now, it's essential to understand that hardware timing is discrete, meaning all expressions inside this block are evaluated concurrently, starting at the begining of each clock cycle.The variable temp is assigned the value of counter + 1 through a continuous assignment, meaning its value updates whenever counter + 1 changes.


### Key Takeaways  

- Hardware components in Anvil are modeled as processes.  
- Hardware processes run indefinitely unless explicitly stopped.  
- Registers store the state of a process.  
- Processes are defined using the `proc` keyword.  
- Process behavior is described inside a `loop`.  
- Each loop iteration takes as long as the longest operation inside it.  
- Registers persist unless modified, while imediate values (`let`) update within the same cycle and are semantically meaningful for one iteration of the loop.
- Lastly we learned about the syntax to declare registers and intermediate values in Anvil, along with debug printing statements.