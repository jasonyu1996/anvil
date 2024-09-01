### Flip Flop

Here instead of two inputs sel and read/write we can just use channel to communicate the value of input and output as we need them and therefore a lot of synchronizing can be done simoultaneously

For example:

```ocaml
    out [ch!right |-> 1]!r
```

This command assigns the wire `out` as register `r` and sets the lifetime from the momment this process writes the output through the channel till one clock cycle


Now we poll for the recieving of the message from the channel

```ocaml
    ch?in => 

```
As soon as we recieve, we see if the message is of type `req` (read), if it is we send the output to the channel, else we wait for `tock` (`posedge clk`) and then we write the value to register with the lifetime, that makes it borrowed as soon as it executes till message of type `resp` (write) is recieved on the channel

```ocaml
        if(ch?req) ch!out
        else{
            tock =>
                r[0|->ch?res]!in[1];
            then
        }
```
