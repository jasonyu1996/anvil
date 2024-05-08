let () =
  let open Lang in
  let open Codegen in
    let proc1 : proc_def = {
      name = "mod1";
      args = [];
      channels = [];
      regs = [
        { name = "counter"; dtype = Array (Logic, 10); init = None}
      ];
      body = [{delays = []; cycle = Identifier "counter"; transition = Seq};
        {delays = []; cycle = Identifier "counter"; transition = Seq};
        {delays = []; cycle = Assign ("counter", Literal [Zero; One]); transition = Seq}]
    } in
    let proc2 : proc_def = {
      name = "mod2";
      args = [
        {name = "a"; channel_class = "example_channel"; dir = Left; foreign = false; opp = None}
      ];
      channels = [{channel_class = "example_channel";
      endpoint_left = "ep0";
      endpoint_right = "ep1";
      visibility = BothForeign}];
      regs = [
        { name = "counter"; dtype = Array (Logic, 10); init = None }
      ];
      body = []
    } in
    codegen {channel_classes = [{
      name = "example_channel";
      messages = [ {name = "msg0"; dir = Out;
        sig_types = [{dtype = Logic; lifetime = {b = Cycles 0; e = Cycles 0}}]}]
    }]; procs = [proc1; proc2]}
