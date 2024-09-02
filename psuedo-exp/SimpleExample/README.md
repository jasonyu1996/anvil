## A Simple Explaination of the anvil functionality

The verilog program `top.v` is a example of where you can establish the pitfalls in verilog programming

```verilog
    always @(posedge clk) begin
            temp = a;  // Blocking assignment
            b <= temp; // Non-blocking assignment
            c <= b;    // Non-blocking assignment
    end
```

Here with the rising edge of clock we are first assigning a `temp`register the value `a` and then we do 2 non blocking assignments `b <= temp` and `c <= b` now the values of c and b would be updated at  the end of the clock cycle but the RHS would be evaluated to a value before that, now since they happen in the same clock cycle due to the nature of the assignment operations, the value of `c` will be assigned the old value of `b` which is a garbage value, hence the designer will only notice this when simulating it, but on the other hand


```ocaml
    ch?in =>
         
        tock =>
            temp[0|->ch?req]!in;
            b[1|->ch?req]!temp
            c[0|->ch?req]!b
        then
    then
```

Here we wait for the input message (`ch?in =>`) after recieving the input we wait for  `tock =>` then we assign the value to `temp` register, after execution of this there are two concurrent commands `b[1|->ch?req]!temp` writes the value of temp to register `b`  but the register can only be borrowed from the next cycle untill next time there is a request from the channel with new input, hence when `c` is assigned in the same cycle as `b` (concurrently) we get the error on our imaginative compiler that the register hasn't been loaned yet.
