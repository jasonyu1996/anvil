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
    left read_req : (logic[8]@#1) @#2 - @dyn,
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

Both sender and receiver agree on a fixed message exchange frequency (`N`).  
- Since they are already synchronized, **no valid or acknowledgment signals are needed**.



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
left read_req : (logic[8]@#1) @#2 - @dyn;
```

- This message is recieved by the `left` endpoint (memory module).
- The value (semantically the lookup address) is valid for 1 cycle after being recieved.  
- The sender (right endpoint) can send the message at any time (`@dyn`) however the reciever `left` endpoint is ready to recieve at interval of `#2` clock cycles from the previous message reciept.
- Since the sender is dynamic(`dyn`) but reciever has a fixed pattern, a valid signal is required to indicate when the message is available for the reciever. However, an acknowledgment signal is not needed, as the sender already knows the reciever will be ready after 2 cycles. (Line 2 from the table above)



**For `read_resp`**

```rust
right read_resp : (logic[8]@#read_req) @read_req+1 - @read_req+1;
```
- This message is recieved by the `right` endpoint (top module).
- The response value (data at the lookup address) is valid from the time of being recieved till the next `read_req` message is sent.  
- It will be sent (by memory module) 1 cycle after the `read_req` is recieved by the memory module. The reciever (top module) is also expected to be ready to recieve the message 1 cycle after the `read_req` is sent.
- Since both sides agree on the frequency of message exchange, an explicit acknowledgment as well as valid control signal is avoided. (Line 4 from the table above)



**For `write_req`**

```rust
left write_req : (address_data_pair@#1);
```

- The message is recieved by the `left` endpoint (memory module).
- The value (semantically the address and data to be written) is valid for 1 cycle after being recieved.
- Since there is no sync pattern described here, it means the sender (right endpoint) has to wait for the reciever (left endpoint) to acknowledge the message, to confirm the message exchange.
- Since both sides dont have no fixed agreement on the frequency of message exchange, a two way handshake is required to ensure the message is recieved. (Line 1 from the table above)


**For `write_resp`**

```rust
right write_resp : (logic[1]@#1) @#write_req+1 - @#write_req+1;
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


