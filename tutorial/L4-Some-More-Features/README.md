# Some More Features

We have covered the primary features of the language. However, Anvil is intended to be a general-purpose HDL, and therefore, it includes some accessibility features that can be useful for designers. In this lesson, we will explore these features.

We will continue with our memory module example and write the top module (a testbench) to interface with it and test its functionality.

Our top module will take 8-bit numbers, store their complementary values in local storage, send encrypted copies to memory, and then retrieve the values to verify correctness.

See the below definition of this top module.

```rust
proc top(){
    chan le_mem -- ri_mem : memory_ch;
    spawn memory_safe(le_mem);
    reg local_mem : byte[256];
    reg read_address : byte;
    reg write_address : byte;
    reg write_complete : logic;
    loop{
        generate (i : 0,255,1) {
            set local_mem[i] := 255-i
        }
    }
    loop{
        let key = 8'b11101001;
        send ri_mem.write_req(address_data_pair::{addr=*write_address;data=(call encrypt(*local_mem[*write_address],key))}) >>
        cycle 1 >>
        let _ = recv ri_mem.write_resp >>
        set write_address := *write_address + 1;
        if(*write_address == 8'd255){
            set write_complete := 1
        }
    }
    loop{
        if(*write_complete == 1'd1){
                send ri_mem.read_req(*read_address) >>
                cycle 1 >>
                let data = recv ri_mem.read_resp ;
                let key = 8'b11101001;
                let  decrypt_data = call decrypt(data,key);
                if (*local_mem[*read_address] == decrypt_data){
                    dprint "Data (%b) Matched for address: %d" (decrypt_data,*read_address)
                }
                else{
                    dprint "Data (%b) Mismatched for address: %d" (decrypt_data,*read_address)
                };
                if(*read_address == 8'd255){
                    dfinish
                };
                set read_address := *read_address + 1
                
        }
        else {cycle 1}
    }
}
```

Now, let’s break down the process definition and understand some of the accessibility features of Anvil.

## Process Prologue
```rust
proc top(){
    chan le_mem -- ri_mem : memory_ch;
    spawn memory_safe(le_mem);
    reg local_mem : byte[256];
    reg read_address : byte;
    reg write_address : byte;
    reg write_complete : logic;
```
- The module first creates an instance of the `memory_ch` channel, generating two endpoints: `le_mem` and `ri_mem`, corresponding to the left and right endpoints of the channel.
- It then instantiates the `memory_safe` process, passing the `le_mem` endpoint to it.
- The necessary register declarations follow:
  - `local_mem` serves as local storage.
  - `read_address` and `write_address` act as iterators for reading and writing.
  - `write_complete` is a flag to indicate when the write operation has finished, allowing the read operation to begin.

## Process Body
This process contains three concurrent loops, each handling a specific operation.

### 1. Updating Local Storage with Complementary Values
```rust
    loop{
        generate (i : 0,255,1) {
            set local_mem[i] := 255 - i
        }
    }
```
- The `generate (id : start, end, step){expr[i]}` construct allows concurrent execution of expressions over a range of values, i.e the semantic of this operation is expr[id];expr[id+1]; expr[id+2]...expr[end]
- This means all assignments to `local_mem` occur in parallel.

### 2. Writing to Memory
```rust
    loop{
        let key = 8'b11101001;
        send ri_mem.write_req(address_data_pair::{addr=*write_address;data=(call encrypt(*local_mem[*write_address], key))}) >>
        cycle 1 >>
        let _ = recv ri_mem.write_resp >>
        set write_address := *write_address + 1;
        if (*write_address == 8'd255) {
            set write_complete := 1
        }
    }
```
- The loop sends a write request containing an address and encrypted data to the memory process.
- The `address_data_pair::{addr=*write_address;data=(call encrypt(*local_mem[*write_address], key))}` is used to create a struct of `address_data_pair` type (from previous lesson) with two fields: `addr` and `data`.
- The `encrypt` function performs a bitwise XOR with the key, then reverses the bit string. 
- After sending the request, the process waits for one cycle to respect the sync patter of `write_resp` (`@#write_req+1-@#write_req+1`).
- After writing we increment the `write_address` and check if we have written all the values. If so, we set `write_complete` to 1, so as to indicate that memory is initialized.

#### Functions
```rust
func encrypt(data, key) {
    let temp = data ^ key;
    #{temp[0], temp[1], temp[2], temp[3], temp[4], temp[5], temp[6], temp[7]}
}

func decrypt(data, key) {
    let rev_data = #{data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7]};
    let temp = rev_data ^ key;
    temp
}
```
- Functions in Anvil follow the `func id(args) { body }` syntax.
- Functions are a primitive feature to avoid repetitive expressions.
- The `call` keyword is used to invoke a function.
- The concatenation operator `#{}` is used to concatenate multiple values.

### 3. Reading from Memory
```rust
    loop{
        if (*write_complete == 1'd1) {
            send ri_mem.read_req(*read_address);
            cycle 1 >>
            let data = recv ri_mem.read_resp;
            let key = 8'b11101001;
            let decrypt_data = call decrypt(data, key);
            if (*local_mem[*read_address] == decrypt_data) {
                dprint "Data (%b) Matched for address: %d" (decrypt_data, *read_address)
            }
            else {
                dprint "Data (%b) Mismatched for address: %d" (decrypt_data, *read_address)
            };
            if (*read_address == 8'd255) {
                dfinish
            }
            set read_address := *read_address + 1
        }
        else { cycle 1 }
    }
```

- This loop starts reading from memory once writing is complete.
- A read request is sent, followed by a cycle wait to comply with the sync pattern of `read_resp`.
- The decrypted data is decrypted and compared with `local_mem` copy.
- If all 256 addresses have been read, the process terminates using `dfinish`.
- Note that everything from receiving the read_resp until the end of the loop starts concurrently and executes in parallel. The next iteration begins only after the `read_address` register update is complete. Since the register update takes one cycle and we waited one cycle to receive `read_resp`, the interval between two consecutive `read_req` operations is 2 cycles, aligning with the sync pattern.
- The `cycle 1` in the else block is used so that next iteration of loop is in the next cycle else it will keep on iterating without the clock cycle changing, when the write is not complete.



## Key Takeaways

- Anvil provides a `generate` construct to generate expressions with indexing, kind of for loop in software programming, however, the expressions are executed in parallel.
- Functions are a primitive feature to avoid repetitive expressions.
- The `call` keyword is used to invoke a function.
- The concatenation operator `#{}` is used to concatenate multiple values.
- The `dprint` and `dfinish` functions are used for debugging purposes.
- The `identifier::{field1=value1;field2=value2...}` syntax is used to create a struct of a specific type with field values.
- Similarly Anvil provides other language construct such as `match`, `recursive` expressions for pipeline behaviour and parametrization of types, channels, processes etc. Check out the language reference for more details.



<!-- ## Some Questions to think about: -->

