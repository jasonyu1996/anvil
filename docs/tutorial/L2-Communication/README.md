# Lesson 2: Communication

We didn’t write any actual hardware last time. We just wrote a component that increments its internal state and prints it to the console. But hardware components usually communicate with each other, exchanging values through interfacing signals.  

For example, a processor might read data from memory or write data to it. In widely used HDLs like SystemVerilog or VHDL, this communication happens through wires or ports. Consider this simple memory module interface in SystemVerilog:  

```verilog
module memory(
    input logic clk_i,
    input logic rst_i,
    input logic[7:0] addr_i,
    input logic[7:0] data_i,
    input mode, // write or read mode
    output logic[7:0] data_o
);
```

<!-- [To Do: Add diagram here]   -->

To a software programmer, this interface might seem sufficient for exchanging values between two components. But there's a problem. These ports or wires carry values all the time. So how do we know when the input address is valid for a lookup? Or when the input data is valid for a write?  

The short answer: we need extra control signals to indicate when a value on a port is valid. But even then, how does the top module know how long to keep the value stable? There’s a missing contract here. To determine how long the value should remain stable, we need to check the memory module’s implementation.


## Channels in Anvil

In Anvil, we abstract communication using channels. Channels facilitate value exchange between processes. Instead of requiring explicit control signals, channels encode the necessary timing constraints directly into the interface handling the control signals implicitly.

Consider the following channel definition in Anvil for the same memory module:  

```rust
struct address_data_pair
{
    addr : logic[8];
    data : logic[8]
}

chan memory_ch {
    left read_req : (logic[8]@#1) @#0~2 - @dyn,
    right read_resp : (logic[8]@read_req) @#read_req+1 - @#read_req+1,
    left write_req :  (address_data_pair@#1),
    right write_resp : (logic[1]@#1) @#write_req+2 - @#write_req+2
}
```

This channel definition encodes a lot of information about the communication between the memory module and the top module—enough information for the top module to function without needing to know the memory module’s internal implementation.



### Breaking Down the Definition  

- A channel is defined using the keyword `chan`, followed by the channel name (`memory_ch` in this case).  
- The channel definition contains two endpoints, each corresponding to one of the interfacing proesses for example in this case the memory module(`left` endpoint) and the top module(`right` endpoint).
- Each endpoint is specified using `left` or `right`, followed by:  
  - A **message identifier** (e.g., `read_req`, `write_req`).  
  - A **message contract** (data type@lifetime).  
  - A **synchronization pattern** (eg `@read_req-@read_req` for the `read_resp`) (Primitive to define static timing contracts).

### Understanding the contract

Each message in a channel comes with a contract specifying:  

1. **Message Contract (`data_type@lifetime`)**  
   Defines the data type of the message and its lifetime. The lifetime specifies how long the message remains stable after being acknowledged or received.  

2. **Synchronization Pattern (`@left-pat - @right-pat`)**  
   Specifies a static agreement between the `left` and `right` endpoint on the frequency of message exchange.  

   - A time pattern can be a **constant** number of clock cycles (e.g., `@#1`) or a **variable** clock cycle (e.g., `@#msg_id`, where `msg_id` corresponds to the reception of a specific message in the channel).   

   - In the time pattern `@#N`, where `N` is constant, `(I~N)` defines the initial delay. This delay applies **only to the first occurrence** of the message. For example, in the case of a `read_req` message, the pattern `#0~2` means the left endpoint is ready to receive a message every 2 cycles. However, the first message can be received immediately after reset.  
      - This initial offset is optional when it is not mentioned it means `N~N`, where the first message is also recieved after `N` cycles after reset.
   - A time pattern can be **dynamic** (`@dyn`), indicating that the corresponding endpoint doesnt agree to a fixed frequency of message exchange.

In Anvil, a message contract is implicitly handled by generating control signals for a two-way handshake. However, in cases where the sender and receiver are already synchronized on the message exchange frequency, explicit valid/acknowledgment signals are not needed for synchronization.  

This leads to different synchronization patterns:  

**1. `@dyn - @#N` (or `@#N - @dyn`)**  

One side cannot guarantee a fixed message exchange frequency (`@dyn`), while the other side operates at a fixed frequency (`@#N`). This results in two cases:  

For example in the case of sync pattern (`@dyn - @#N`):

- **When the right endpoint is the receiver**
  - The sender cannot guarantee a fixed frequency.  
  - The receiver expects messages at intervals of `N`.  
  - Since the sender is unsynchronized, a **valid signal is required** to indicate when a message is available for the receiver.
  - However, an **acknowledgment signal is not needed**, as the sender already knows the receiver will be ready after `N` cycles.

- **When the right endpoint is the sender:**  
  - The sender transmits messages, at a fixed interval `N`.  
  - The receiver cannot guarantee it will be ready at that exact interval.  
  - To ensure the message is received, an **acknowledgment signal is needed**.  
  - However, a **valid signal is not required**, since the sender always sends at a fixed interval post reciept of the previous message.  

**2. `@#N - @#N`**  

Both sender and receiver agree on a fixed message exchange frequency (`N`). Since they are already synchronized, **no valid or acknowledgment signals are needed**.



### Generalization for Synchronization Patterns

| Synchronization Pattern | Valid Signal Needed? | Acknowledgment Needed? |  
|-------------------------|----------------------|------------------------|  
| (No sync pattern mentioned)| ✅ No | ✅ No |
| (Sender = `@dyn`, Receiver = `@#N`) | ✅ Yes | ❌ No |  
| (Sender = `@#N`, Receiver = `@dyn`) | ❌ No | ✅ Yes |  
| `@#N - @#N` (Both synchronized) | ❌ No | ❌ No |  




Now lets see if we can understand each of the message contracts in the above channel definition:

**For  `read_req`**  

```rust
left read_req : (logic[8]@#1) @#0~2 - @dyn
```

- This message is recieved by the `left` endpoint (memory module).
- The value (semantically the lookup address) is valid for 1 cycle after being recieved.  
- The sender (right endpoint) can send the message at any time (`@dyn`) however the reciever `left` endpoint is ready to recieve at interval of `#2` clock cycles from the previous message reciept. The first message can be recieved immediately after reset.
- Since the sender is dynamic(`dyn`) but reciever has a fixed pattern, a valid signal is required to indicate when the message is available for the reciever. However, an acknowledgment signal is not needed, as the sender already knows the reciever will be ready after 2 cycles. (Line 2 from the table above)



**For `read_resp`**

```rust
right read_resp : (logic[8]@#read_req) @read_req+1 - @read_req+1
```
- This message is recieved by the `right` endpoint (top module).
- The response value (data at the lookup address) is valid from the time of being recieved till the next `read_req` message is sent.  
- It will be sent (by memory module) 1 cycle after the `read_req` is recieved by the memory module. The reciever (top module) is also expected to be ready to recieve the message 1 cycle after the `read_req` is sent.
- Since both sides agree on the frequency of message exchange, an explicit acknowledgment as well as valid control signal is avoided. (Line 4 from the table above)



**For `write_req`**

```rust
left write_req : (address_data_pair@#1)
```

- The message is recieved by the `left` endpoint (memory module).
- The value (semantically the address and data to be written) is valid for 1 cycle after being recieved.
- Since there is no sync pattern described here, it means the sender (right endpoint) has to wait for the reciever (left endpoint) to acknowledge the message, to confirm the message exchange.
- Since both sides dont have no fixed agreement on the frequency of message exchange, a two way handshake is required to ensure the message is recieved. (Line 1 from the table above)


**For `write_resp`**

```rust
right write_resp : (logic[1]@#1) @#write_req+1 - @#write_req+1
```

- The message is recieved by the `right` endpoint (top module).
- The value (acknowledgement of the write operation) is valid for 1 cycle after being recieved.
- The message is expected to be sent 1 cycles after the `write_req` is acknowledged. Similarly the reciever (top module) is expected to be ready to recieve the message 1 cycle after the `write_req` is acknowledged.
- Since both sides agree on the frequency of message exchange, an explicit acknowledgment as well as valid control signal can be avoided. (Line 4 from the table above)



## Key Takeaways

- Anvil abstracts communication between components using channels.
- Channel definitions in anvil encode the necessary timing constraints directly into the interface.
- Each channel contains two endpoints (`left` and `right`), each corresponding to one of the interfacing processes.
- Each message (identified by a message identifier) in a channel comes with a contract specifying the data type, lifetime, and possibly synchronization pattern.
- In anvil synchonous exchange of messages (as per the contract) are implicitly handled by generating control signals for a two-way handshake
- Synchronization patterns are used to avoid handshake in cases where the sender and receiver are already synchronized in terms of message exchange frequency.


## Some Questions to think about:

**1. What is the way to define timing contracts when the behaviour is unknown, or the timing is not fixed?**

<details>
<summary>Answer</summary>

The simplest contract is a dynamic contract which works with a two way handshake, for eg if you have to implement a memory system with cache, where the timing of the cache hit or miss determines how long the lookup will take. So a channel definition would look something like this:
```rust
chan cache_lookup_ch{
  left lookup_req : (logic[8]@lookup_res),
  right lookup_resp : (logic[8]@lookup_req)
}
```

This would imply that the value before being used has to be acknowledged, however is stable untill the following request/response is transmitted.

Now the code generated by Anvil, handles this handshake implicitly, so the user does not have to worry about the handshake, just wait for the value to be available and then use it and send the response back or next request when done using it.

The same can be true for modules with static contracts, where the handshake is not needed, the code generated by Anvil will handle the handshake implicitly, hence the handshake is immediate, so all in all in either case there is no latency overhead. 

However in cases when the anvil proc has to interface with external proc, the management of signals on other end becomes a responsibility of the external module. So in cases when this can be avoided we use sync patterns to avoid generating handshake signals.
</details>


**2. Define a channel for a pmp module (physical memory protection) which takes in the configuration and address and sends back the response, to allow memory access or not.**

<details>
<summary>Answer</summary>

```rust
chan pmp_ch{
  left pmp_req : (pmp_in@#1),
  right pmp_resp : (logic[1]@pmp_req)
}
```
Since we want to check address against configuration, we have to assume that the address is valid only in the clock cycle when request is sent and register it, to avoid TOCTOU type of attacks. Hence if pmp module takes more than one cycle to verify the address against configuration it has to register it.
</details>

**3 Define a contract for arbiter and a consumer which sends request every 2 cycles, but the arbiter has agreement of accepting the request at a fixed frequency. Use minimum signals possible**

<details>
<summary>Answer</summary>

```rust
chan arbiter_ch{
  left arbiter_req : (logic[1]@#2), @dyn - @#2
}
```
Considering arbiter is the `left` endpoint, now the consumer has a static pattern, now for consumer to know it request is granted, arbiter has to just acknowledge the request, and since it will send again in 2 cycles, the value should not be used post 2 cycles hence the lifetime is #2.

This is the way to generate minimum signals possible, however the arbiter has to be ready to accept the request at least at an interval of #2 cycles, can't be less than that.
</details>