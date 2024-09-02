## Ripple Carry Ahead Adder


In this example we talk about 4 bit Ripple Carry Ahead Adder, which is a sequential adder that adds each bit one by one and thus, carry forwards the result of addition to the next bit, as visible in `RCA.v`.


```verilog
    module full_adder (
        input a, b, cin,  
        output sum, cout  
    );
        assign #5 sum = a ^ b ^ cin;  // Sum with a delay of 5 units
        assign #5 cout = (a & b) | (b & cin) | (a & cin); // Carry out with a delay of 5 units
    endmodule
```

This is the code for full adder as visible here delay has been created with delays to avoid timing error, but this is not practical in synthesis, is just for simulation purposes.

Hence in the instantiation of RCA

```verilog
    module RCA (
    input [3:0] a,    
    input [3:0] b,    
    input cin,        
    output [3:0] sum, 
    output cout       
);
    wire c1, c2, c3;  

    // Full adder for bit 0
    full_adder fa0 (
        .a(a[0]),
        .b(b[0]),
        .cin(cin),
        .sum(sum[0]),
        .cout(c1)
    );

    // Full adder for bit 1
    full_adder fa1 (
        .a(a[1]),
        .b(b[1]),
        .cin(c1),
        .sum(sum[1]),
        .cout(c2)
    );

    // Full adder for bit 2
    full_adder fa2 (
        .a(a[2]),
        .b(b[2]),
        .cin(c2),
        .sum(sum[2]),
        .cout(c3)
    );

    // Full adder for bit 3
    full_adder fa3 (
        .a(a[3]),
        .b(b[3]),
        .cin(c3),
        .sum(sum[3]),
        .cout(cout)
    );

endmodule

```

The instantiation of 4 full adders where value is forwarded, there can be timing errors.


Now coming to the anvil implementation:


```ocaml
    proc full adder(ch)
    {
        ch?in =>
        sum[ch!res |-> inf] !(in[0]^in[1]^in[2])
        carry[ch!res |-> inf]!(in[0]&in[1] | in[1]&in[2] | in[2]&in[0])
        then
        ch!{sum, carry};

    }
```

The full adder process is created by first recieving the input through the channel `ch?in =>` and then we assign the value of `sum` and `carry` with lifetime that begins when the channel sends a message of type resp `ch!resp` till the end, this makes the value of sum and carry only usable when its available.


```ocaml
    proc RCA(ch)
    {
        reg [3:0] a = #Some value with a valid lifetime
        reg [3:0] b = #Some value with a valid lifetime
        wire cin = # Some value with a valid lifetime

        full adder fa0(ch0)
        full adder fa1(ch1)
        full adder fa2(ch2)
        full adder fa3(ch3)

        ch0!{a[0],b[0],cin};
        ch0?{sum[0],c1} =>
        ch1!{a[1],b[1],c1};
        ch1?{sum[1],c2} =>
        ch2!{a[2],b[2],c2};
        ch2?{sum[2],c3} =>
        ch3!{a[3],b[3],c3};
        ch1?{sum[1],cout} =>

        ch!{sum,cout};
    }

```

Hence the top level process or `RCA` creates 4 instantiations of the full adder, writes the input sequentially, takes the output, which is only writable when the lifetimes start