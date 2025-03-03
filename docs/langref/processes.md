### Processes

A process describes a module of the design.
A full design may consist of multiple processes which
communicate through channels.


#### Process Spawning

Inside a process definition,
an instance of another process is spawned through the `spawn` statement.

```
process-spawn ::= spawn $identifier [$param-vals] (($identifier {, $identifier}) | ()) ;
```

The statement `spawn p(ep1, ep2, ..., epn);` spawns an instance of the process
named `p` with endpoints `ep1`, `ep2`, ..., `epn`, which can either be endpoints
passed in to the current module or come from channels created in the process.

#### Process Definition

```
proc-definition ::= proc $identifier [$params] (($identifier : (left | right) $identifier [$param-vals]) | ()) { { $channel-creation | $process-spawn | $reg-creation | $loop-thread | $recursive-thread } }
```

#### Process Threads

A process consists of threads. Two types of threads are available: loop threads and recursive
threads.


##### Loop Threads

```
loop-thread ::= loop { $expression }
```


##### Recursive Threads

```
recursive-thread ::= recursive { $expression }
```

#### External Process

A process can also be defined to wrap an external module, i.e., implemented in
non-AnvilHDL code. See [this](systemverilog.md) for detail.
