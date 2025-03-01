### Registers

A register provides the means to maintain persistent states.

#### Register Creation

A register can be defined in a process.
```
reg-creation ::= reg $identifier : $data-type-expression [$param-vals] ;
```

The statement `reg r : dtype;` defines a new register with identifier `r` and
data type `dtype`.

#### Register Read

A register can be read through the `*` operator.
```
reg-read-expression ::= *$identifier
```

The expression `*r` evaluates immediately to the current value of the register `r`.
The value is available until the next write to `r`.

##### Register Write

A register can be written through the set expression.
```
set-expression ::= set $lval := $expression
lval ::= $identifier | $lval.$identifier | $lval [ $expression ] | $lval [ $expression+:{$digit}+ ]
```

The set expression evaluates to `()` delayed by one cycle. All expressions involved
must have been evaluated and have valid results.
The new value of the register will be visible in the next cycle.

