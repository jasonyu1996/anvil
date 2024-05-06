let () =
  let open Lang in
  let open Codegen in
    let proc1 : proc_def = {
      name = "mod1";
      msgs = [
        { name = "ReadRequest"; dir = In; sig_types = [
          {dtype = Array ((Array (Logic, 2)), 3); lifetime = { b = Cycles 0; e = Cycles 1} }
        ] }
      ];
      regs = [
        { name = "counter"; dtype = Array (Logic, 10); init = None}
      ];
      body = Seq (Identifier "counter", Seq(Identifier "counter", EmptyProcBody))
    } in
    let proc2 : proc_def = {
      name = "mod2";
      msgs = [
        { name = "WriteRequest"; dir = Out; sig_types = [
          {dtype = Logic; lifetime = {b = Cycles 3; e = Cycles 3}}
        ] }
      ];
      regs = [
        { name = "counter"; dtype = Array (Logic, 10); init = None }
      ];
      body = EmptyProcBody
    } in
    codegen [proc1; proc2]
