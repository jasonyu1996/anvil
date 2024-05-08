let () =
  let open Lang in
  let open Codegen in
    let proc1 : proc_def = {
      name = "mod1";
      args = [
        {name = "a"; channel_class = "example_channel"; dir = Left; foreign = false; opp = None}
      ];
      channels = [];
      regs = [
        { name = "counter"; dtype = Array (Logic, 10); init = None}
      ];
      spawns = [];
      body = [{delays = []; cycle = Identifier "counter"; transition = Seq};
        {delays = []; cycle = Identifier "counter"; transition = Seq};
        {delays = [Send (["g"], {endpoint = "a"; msg = "msg0"}, Literal [Zero])]; cycle = Assign ("counter", Literal [Zero; One]); transition = Seq}]
    } in
    let proc2 : proc_def = {
      name = "mod2";
      args = [
        {name = "a"; channel_class = "example_channel"; dir = Right; foreign = false; opp = None}
      ];
      channels = [];
      regs = [
        { name = "counter"; dtype = Array (Logic, 10); init = None }
      ];
      spawns = [];
      body = [{delays = [Recv (["g"], {endpoint = "a"; msg = "msg0"})];
          cycle = Return ("a", "msg0", Literal [Zero]); transition = Seq}]
    } in
    let top_proc : proc_def = {
      name = "top";
      args = [];
      channels = [
        {channel_class = "example_channel";
        endpoint_left = "ep0";
        endpoint_right = "ep1";
        visibility = BothForeign}
      ];
      regs = [];
      spawns = [
        {
          proc = "mod1";
          params = ["ep0"];
        };
        {
          proc = "mod2";
          params = ["ep1"];
        }
      ];
      body = [];
    } in
    codegen {channel_classes = [{
      name = "example_channel";
      messages = [ {name = "msg0"; dir = Out;
        sig_types = [{dtype = Logic; lifetime = {b = Cycles 0; e = Cycles 0}}];
        ret_types = [{dtype = Array (Logic, 3); lifetime = {b = Cycles 0; e = Cycles 0}}]}]
    }]; procs = [proc1; proc2; top_proc]}
