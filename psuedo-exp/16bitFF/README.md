### Flip Flop

Lets see a verilog example of a simple flip flop that takes as input one read/write (`wr`) and one `sel` wire which helps to validate the validity of output or input

```verilog
module FF16( input clk,
            input wr,
            input sel,
            input [15:0] wdata,
            input [15:0] rdata);
```

Now we have a continuous assignment that assigns the `rdata` output as register, whenever it is requested to be read else it assigns it `0` similarly register value has to be rewritten, incase there is no request to write or select the process
```verilog
    reg [15:0] register;
        always @(posedge clk) begin
                if (sel & wr)
                register <= wdata;
                else
                register <= register;
        end

        assign rdata = (sel & ~wr) ? register: 0;
```
Coming to anvil....


Here instead of two inputs sel and read/write we can just use channel to communicate the value of input and output as we need them and therefore a lot of synchronizing can be done simoultaneously

For example:

```ocaml
    out [ch!right |-> 1]:=r
```

This command assigns the wire `out` as register `r` and sets the lifetime from the momment this process writes the output through the channel till one clock cycle


Now we poll for the recieving of the message from the channel

```ocaml
    ch?in => 

```
As soon as we recieve, we see if the message is of type `req` (read), if it is we send the output to the channel, else we wait for `tock` (`posedge clk`) and then we write the value to register with the lifetime, that makes it borrowed as soon as it executes till message of type `resp` (write) is recieved on the channel

```ocaml
        if(type(in)=req) ch!out
        else{
            tock =>
                r[0|->ch?res]:=in[1];
            then
        }
```
