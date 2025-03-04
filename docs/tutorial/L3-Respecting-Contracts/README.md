# Lesson 3: Respecting Contracts

In the last lesson, we saw that the contract of a channel between the memory module and the interfacing top module encodes the timing constraints for both modules. In this lesson, we will look at some language primitives that simplify synchronization between modules and examine the checker that ensures these contracts are respected.  

Let's look at the following definition of the memory module in Anvil:  

```rust
type byte = logic[8];
proc memory(endp: left memory_ch) {
    reg mem : byte[256];
    reg rd_addr : byte;
    loop {
            let addr = recv endp.read_req >>
            send endp.read_resp(*mem[*rd_addr])>>
            cycle 1
    }
    loop{
            let addr_data = recv endp.write_req>>
            set mem[addr_data.addr] := addr_data.data >> 
            send endp.write_resp(1)
    }
}
```

## Understanding the Process Definition

- The process `memory` has a single endpoint `left memory_ch`, which is the channel connecting the memory module to the top module. It is identified by `endp` in the process definition.  
- The process declares a register `mem`, which is an array of 256 elements of type `logic[8]`, using a custom type `byte`.  
- The behavior of `memory` is defined  inside `loop`, where we have 2 instances of the loop construct.
- The first loop handles read requests, while the second loop handles write requests. They both run concurrently and independently.

Now let’s break it down case by case:  

**1. Handling a Read Request:**  
   - We receive the address using `recv`, binding it to `addr`.  
   - We then use the `>>` wait primitive to ensure that the message is available (valid) before proceeding.
   - The response message `read_resp` is sent back with the data at the received address in the memory array, accessed using the `*` operator.  
   - Finally, we advance time by one cycle using `cycle 1`, ensuring that the next recv operation is in the next cycle.
   
**2. Handling a Write Request:**  
   - We receive the address and data pair using `recv`, binding it to `addr_data`.  
   - We then use `>>` to ensure the message is valid and as soon as it is valid it is acknowledged immediately without consuming any extra cycles.
   - The memory at `addr_data.addr` is updated with `addr_data.data`. Since register updates take one cycle (as seen in L1), we use `>>` to synchronize (advance time by one cycle).  
   - An acknowledgment is then sent using `send endp.write_resp(1)`.  

---

## The `>>` Wait Primitive

The `>>` primitive ensures correct synchronization. It **pauses execution until all the previous expressions are fully evaluated**, acting as a barrier that enforces ordering. It also helps the verifier determine the sequence of events in the process.

- It guarantees that a received message is valid and acknowledged before it is used.  
- `>>` does not introduce extra delay, it only ensures that the previous operations are completed before proceeding, for example `cycle 4>>` would mean that all operations that follow will be delayed by 4 cycles.
- This is similar to `thread.join()` in Python or `async/await` in Go, ensuring correct ordering between dependent operations.  

Infact the loop semantics follow the pattern `loop_body >> loop_body` This means all expressions in the loop must complete before the next iteration starts.


## Verifying adherence to the contract
To check if this module adheres to the expected contract, we run the Anvil checker:  

```bash
make MODULE_NAME=memory
```  

You should receive the following error message:  

```bash
Static sync mode mismatch (actual gap = 0 < expected gap 1)!
memory.anvil:20:12:
     20|             send endp.read_resp(*mem[*rd_addr])>>
```  

Now this implies that the `read_resp` is sent in the same cycle as the `read_req` is received, which violates the contract.

```rust
right read_resp : (logic[8]@#read_req) @read_req+1 - @read_req+1
```  

The `read_resp` should be sent one cycle after the `read_req` is received.


To fix this, we introduce a `cycle 1` after the `send` statement:  

```rust
            recv endp.read_req >>
            cycle 1 >>
            send endp.read_resp(*mem[*rd_addr])>>
            
```


Now when we run the program again:  

```bash
make MODULE_NAME=memory_fix_1
```

we would see the following error:

```bash
Value not live long enough in message send!
memory_fix1.anvil:21:12:
     21|             send endp.read_resp(*mem[addr])>>
```
The checker indicates that `*mem[addr]` cannot be guaranteed to be live until the next `read_req` message is received, which is at least 2 cycles away.  

Now, the issue arises because the value of `addr` is only stable for one cycle, as dictated by the contract of `read_req`:  

```rust
left read_req : (logic[8]@#1) @#0~2 - @dyn;
```  

This means `addr` could change in the next cycle, potentially leading to an incorrect memory read. To ensure stability, we need to register the value of `addr` so that it remains unchanged until the next `read_req` message arrives.  

```rust
            let addr = recv endp.read_req >>
            set rd_addr := addr;
            cycle 1 >>
            send endp.read_resp(*mem[*rd_addr])>>
            cycle 1
```  

Now if u run the program again
```bash
make MODULE_NAME=memory_fix_2
```


you will see the following error:
```bash
Value not live long enough in message send!
memory_fix1.anvil:21:12:
     21|             send endp.read_resp(*mem[*rd_addr])>>

```

The issue here is that after a successful `read_resp` message send, the value is expected to remain stable until the next `read_req` as per the contract. However, since `read_req` is sent dynamically (`@dyn`), there could be an intervening `write_req` that modifies the same memory address, altering the value in `mem` register before the next `read_req`. This violates the contract since `read_resp` would no longer hold the same value as when it was sent.
  

To fix this we need to register the read data itself before sending it, ensuring it remains stable:  

```rust
            let addr = recv endp.read_req >>
            set rd_data := *mem[addr];
            cycle 1 >>
            send endp.read_resp(*rd_data)>>
            cycle 1
```

Now, running the checker on this updated version:  

```bash
make MODULE_NAME=memory_safe
```  

Results in no errors, meaning the program fully respects the timing contract.  

This is a small demonstration of how Anvil helps ensure your process definition adheres to the contract. It detects violations in multiple ways and guarantees that the process statically satisfies all timing constraints dictated by the channel contracts.  The key principle is that if the contract is statically verified for both interfacing components, the design runs without any timing violations and without introducing additional latency.


## Key Takeaways

- The wait `>>` primitive is used to create synchronization points in the process definition, ensuring all operations before it have completed
- The value sent by channels is intermediate value implemented using signals, process needs to guarantee to keep that value stable untill the lifetime in the contract.
- Once again register store state, but everything cannot be registered (resource expensive), so we need to be careful with the contract and use intermediate value only when it is guaranteed to be stable.
- The Anvil checker ensures that the process definition respects the timing constraints of the channel contract
- Violations in the process definition are detected by the checker, helping you fix them before running the design
- By adhering to the channel contract, you can guarantee that the design runs without timing violations while incurring no latency overhead


## Some Excercises to think about:

**1. What happens when you change the read_req contract sync pattern to `#1 - @dyn` (implying ready to recv every cycle) ?**
<details>
<summary>Answer</summary>

When you run the program you will see the following error:
```bash
Borrow checking failed:
Static sync mode mismatch between endp@read_req and endp@read_req (actual gap = 2 > expected gap 1)!
memory_safe.anvil:21:23:
  21|             let addr = recv endp.read_req >>
```

This is because the read thread (loop) waits 1 cycle before looping back to the next cycle hence, the gap between `read_req` and `read_res` is 1 cycle and therefore the gap between `read_req` and next `read_req` will be 1+1 = 2 cycles. This violates the contract of the channel. This will be fixed by removing the 1 cycle delay in the read thread at the end.

This is already happening in the write thread as the write thread is ready to receive every cycle.
</details>


**2. What happens when there are multiple endpoints of same `memory_ch` type in the memory module ? How would a safe implementation look like ?**

<details>
<summary>Answer</summary>

When you have multiple endpoints of the same type in the memory module, the read operations can happen in parallel, therefore implemented over multiple threads. However the write operation needs to be 1 at a time, as they can be to the same memory location.

A safe implemnentation would look like this:

```rust
proc memory_multiple_ports(endp1: left memory_ch, endp2: left memory_ch) {
    reg mem : byte[256];
    reg rd_data1 : byte;
    reg rd_data2 : byte;
    loop {
            let addr = recv endp1.read_req >>
            set rd_data1 := *mem[addr];
            cycle 1 >>
            send endp1.read_resp(*rd_data1)>>
            cycle 1
    }
    loop {
            let addr = recv endp2.read_req >>
            set rd_data2 := *mem[addr];
            cycle 1 >>
            send endp2.read_resp(*rd_data2)>>
            cycle 1
    }
    loop{
      if (ready endp1.write_req) {
            let addr_data = recv endp1.write_req>>
            set mem[addr_data.addr] := addr_data.data >> 
            send endp1.write_resp(1)
      }
      else{
          if (ready endp2.write_req) {
                let addr_data = recv endp2.write_req>>
                set mem[addr_data.addr] := addr_data.data >> 
                send endp2.write_resp(1)
          }
          else {cycle 1}      
      } 
    }
}
```
Anvil Provides the `ready` construct to check if the endpoint has a valid message. In this case we are checking if either of the endpoints has a valid message to process. 
</details>

---

[L4: Some More Features =>](../L4-Some-More-Features/README.md)\
[<= L2: Communication](../L2-Communication/README.md)
