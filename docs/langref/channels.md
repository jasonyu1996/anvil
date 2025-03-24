### Channels

A channel is the means of communication and sychronization between a pair of processes.

**Endpoints:** Each channel has two endpoints, corresponding to the two ends of the
communication. Conceptually, a channel resembles a pipe or tube that can transport
items between the two ends.

**Messages:** A channel has a defined set of message types that can be sent and received in
defined directions. A message carries values of specified types.

**Timing contracts:** Messages have defined timing contracts, which specify the timing
behaviour the sender and the receiver are expected to follow with respect to sending/receiving
the message, e.g., when the communicated value is usable and when the send/receive may
take place.

**Sychronisation:** All messages are _synchronous,_ meaning that to complete sending or receiving
a message, _both_ sides must be ready to do so. This in effect involves both sides agreeing
on a time to send or receive the message. Thus we also refer to this process that involves
one side sending and the other side receiving at the same time as synchronisation.
The time of synchronisation is the time a message send/receive completes.

#### Channel Classes

A channel class serves as a template for channels. It specifies the messages that
can travel on the channel, their directions, data types, as well as timing contracts.

```
channel-class-definition ::= chan [$params] { $message-definition {, $message-definition} }
message-definition ::= $message-direction $identifier : ($data-type-expression @ $lifetime-pattern) [@ $sync-mode - @ $sync-mode]
message-direction ::= left | right
lifetime-pattern ::= #{$digit}+ | $identifier
sync-mode ::= dyn | #{$digit}+ [~{$digit}+] | #$identifier [+ {$digit}+]
```

TODO: more info needed

#### Channel Creation

Channels can be created to obtain two endpoints.
```
channel-creation ::= chan $identifier -- $identifier : $identifier [$param-vals] ;
```

`chan le -- ri : chan_class;` creates a new channel from the channel class `chan_class`
and defines `le` and `ri` as identifiers for its left and right endpoints, respectively.

