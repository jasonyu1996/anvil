### Expressions

#### Cycle

```
cycle-expression ::= cycle { $digit }+
```

The `cycle` expression evaluates to a unit value `()` delayed by a specified number
of cycles. Its sole purpose is to introduce the specified delay.
For example, `cycle 1` evaluates to `()` after one cycle.

#### Wait

```
wait-expression ::= $expression >> $expression
```

The wait expression is the main means of controlling time. The expression
`e1 >> e2` waits for `e1` evaluation to complete (if it has not already
completed) before starting the evaluation of `e2`. The expression
evaluates to the evaluation result of `e2` when evaluations of both `e1`
and `e2` complete.
For example, `cycle 1 >> cycle 2` is equivalent to `cycle 3`.

#### Join

```
join-expression ::= $expression ; $expression
```

The expression `e1; e2` starts evaluations of `e1` and `e2` immediately and
at the same time. It evaluates to the evaluation result of `e2` when both evaluations
complete. For example, `cycle 1; cycle 2` is equivalent to `cycle 2`.


#### Let

```
let-expression ::= let $identifier = $expression ; $expression
let-wait-expression ::= let $identifier = $expression >> $expression
```

The let expression `let x = e1; e2` binds `e1` to an identifier `x` which can be
referenced in `e2`. The expression evaluates to the evaluation result of `e2` when
evaluations of both `e1` and `e2` complete.
The difference between `let x = e1; e2` and `let x = e1 >> e2`
is that the former starts evaluating `e1` and `e2` at the same time, whereas the latter
waits for `e1` evaluation to complete before starting to evaluate `e2`, similar to how
the wait expression differs from the join expression.

#### If-else Expressions

```
if-else-expression ::= if $expression { $expression } [ else ( { $expression } | $if-else-expression ) ]
```

The expression `if e1 { e2 } else { e3 }` is evaluates to the evaluation result of
`e2` or `e3` depending on the evaluation result of `e1`. The evaluation of `e1` must already
be complete and the result must still be valid.
If `e1` evaluates to an all-zero value, the expression starts evaluating `e3`. Otherwise,
it starts evaluating `e2`. The `else` clause is optional, with `if e1 { e2 }` equivalent to
`if e1 { e2 } else { () }`. Multiple expressions can be chained: `if e1 { e2 } else if e3 { e4 } else ...`

#### Match Expressions

```
match-expression ::= match $expression { ($expression | _) => $expression {, ($expression | _) => $expression } }
```

The expression `match e { e1 => e1', e2 => e2', ..., en => en', _ => e' }` is a syntax sugar
for
```
if e == e1 { e1' } else if e == e2 { e2' } else if ... else if e == en { en' } else { e' }
```

The `_ => e'` arm must appear in the expression exactly once.

#### Arithmetic Expressions

```
binary-arith-expression ::= $expression $binary-arith-operator $expression
unary-arith-expression ::= $unary-arith-operator $expression

binary-arith-operator ::= + | - | & | | | ^ | < | > | <= | >= | == | !=
unary-arith-operator ::= - | ~
```

Those expressions evaluate to results according to the operators. The evaluation completes
when the one (unary) or two (binary) sub-expressions complete evaluations.

#### Concatenation Expressions

```
concat-expression ::= #{ $expression {, $expression} }
```

The expression `#{e1, e2, ..., en}` concatenates the evaluation results of `e1`, `e2`, ...,
`en` and completes evaluation when all evaluations of `e1`, `e2`, ..., `en` have completed.
Note `en` will be placed at the low bits in the result while `e1` will be placed at the high bits.
For example `#{2'b01, 5'b01101, 1'b1}` produces value `8'b01011011`.

#### Send Expressions

```
send-expression ::= send $identifier.$identifier ($expression)
```

When the evaluation of the expression `send ep.m (e)` starts, the process starts waiting
to send the evaluated result of `e` with message `ep.m`, where `ep` is an endpoint identifier
and `m` is a message identifier. The evaluation completes with result `()`
once the send occurs.

#### Receive Expressions

```
recv-expression := recv $identifier.$identifier
```

When the evaluation of the expression `recv ep.m (e)` starts, the process starts waiting
to receive the message `ep.m`, where `ep` is an endpoint identifier
and `m` is a message identifier.
Once the receive occurs,
the evaluation completes with the received value as the result.


#### Precedence and Associativity

The `>>` and `;` operators are right-associative and have the same precedence.
For example, `e1; e2 >> e3; e4 >> e5` is equivalent to `(e1; (e2 >> (e3; (e4 >> e5))))`.
