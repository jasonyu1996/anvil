type filament_val =
| ValIdent of string
| ValLiteral of int

type filament_expr =
| ExprInvoke of string * int * filament_val list


type filament_insn =
| InsnBind of string * filament_expr
| InsnNew of string * string
| InsnRet of filament_val

type filament_lifetime = {
  lt_start : int;
  lt_end : int;
}

let delay_of_lifetime lt =
  lt.lt_end - lt.lt_start

(* we omit the data type and assume all data is one bit *)
type filament_comp = {
  comp_ident : string;
  comp_interval : int;
  comp_input : (string * filament_lifetime) list;
  comp_output : filament_lifetime;
  comp_body : filament_insn list;
}

type filament_prog = {
  prog_comps : filament_comp list;
}

type filament_chan =
| ChanGo
| ChanOut of int
| ChanIn of string * int


let find_comp prog ident =
  if ident = "Reg" then
    {
      comp_ident = "Reg";
      comp_interval = 1;
      comp_input = [("v", {lt_start = 0; lt_end = 1})];
      comp_output = {lt_start = 1; lt_end = 2};
      comp_body = [];
    }
  else
    List.find (fun c -> c.comp_ident = ident) prog.prog_comps


(* returns the comp ident corresponding to the spawn *)
let find_spawn comp ident =
  List.find_map (
    function
    | InsnNew (ident', comp_ident) ->
      if ident = ident' then Some comp_ident else None
    | _ -> None
  ) comp.comp_body |> Option.get

let generate_chan_class comp chan =
  let (suffix, lifetime) =
    match chan with
    | ChanGo -> ("go", 1)
    | ChanOut n -> ("out", n)
    | ChanIn (ident, n) -> (Printf.sprintf "in_%s" ident, n)
  in
  Printf.printf "chan %s_%s = {\n" comp.comp_ident suffix;
  Printf.printf "  left data : (logic@#%d) @dyn-@#%d\n" lifetime comp.comp_interval;
  Printf.printf "}\n"

let generate_chan_classes comp =
  (* go first *)
  (* generate_chan_class comp ChanGo; *)
  generate_chan_class comp (ChanOut (delay_of_lifetime comp.comp_output));
  List.iter (fun (ident, lt) -> generate_chan_class comp (ChanIn (ident, delay_of_lifetime lt)))
    comp.comp_input

let generate_spawns prog comp =
  let generate_endpoints name chan_class_name =
    Printf.printf "  chan %s_left -- %s_right : %s\n" name name chan_class_name
  in
  List.iter (function
    | InsnNew (ident, comp_ident) ->
      let spawned_comp = find_comp prog comp_ident in
      (* generate_endpoints (Printf.sprintf "%s_go" ident) (Printf.sprintf "%s_go" spawned_comp.comp_ident); *)
      generate_endpoints (Printf.sprintf "%s_out" ident) (Printf.sprintf "%s_out" spawned_comp.comp_ident);
      List.iter (fun (inp_ident, _inp_lt) ->
        generate_endpoints (Printf.sprintf "%s_in_%s" ident inp_ident) (Printf.sprintf "%s_in_%s" spawned_comp.comp_ident inp_ident)
      ) spawned_comp.comp_input;
      (* Printf.printf "  spawn %s(%s_go_left, %s_out_right" spawned_comp.comp_ident ident ident; *)
      Printf.printf "  spawn %s(%s_out_right" spawned_comp.comp_ident ident;
      List.iter (fun (inp_ident, _inp_lt) ->
        Printf.printf ", %s_in_%s_left" ident inp_ident
      ) spawned_comp.comp_input;
      Printf.printf ")\n"
    | _ -> ()
  ) comp.comp_body

let translate_comp prog comp =
  (* generate channel class definitions *)
  (* go, out + inputs *)
  Printf.printf "proc %s (%s_out : right %s_out"
    comp.comp_ident comp.comp_ident comp.comp_ident;
  (* Printf.printf "proc %s (%s_go : left %s_go, %s_out : right %s_out"
    comp.comp_ident comp.comp_ident comp.comp_ident comp.comp_ident comp.comp_ident; *)
  List.iter (fun (in_ident, _in_lt) ->
    Printf.printf ", %s_in_%s : left %s_in_%s"
      comp.comp_ident in_ident comp.comp_ident in_ident
  ) comp.comp_input;
  Printf.printf ") =\n";
  (* generate spawns *)
  generate_spawns prog comp;
  (* FSM *)
  (* let len = List.fold_left (fun mx insn ->
    match insn with
    | InsnBind (_, ExprInvoke (ident, delay, _vals)) ->
      (* check when it returns *)
      let invoked_comp = find_spawn comp ident |> find_comp prog in
      invoked_comp.comp_output.lt_end + delay |> Int.max mx
    | _ -> mx
  ) 0 comp.comp_body in
  let len = List.fold_left (fun mx (_inp_ident, inp_lt) ->
    Int.max mx inp_lt.lt_start
  ) len comp.comp_input in () *)
  (* for i = 1 to len do
    Printf.printf "  reg tick%d : logic\n" i
  done; *)
  (* now add looping threads *)
  (* for go *)
  (* Printf.printf "  loop {\n";
  Printf.printf "    let r = recv %s_go in\n" comp.comp_ident;
  Printf.printf "    r => if r then (\n";
  if len > 0 then
    Printf.printf "      set tick1 := 1'b1\n";
  Printf.printf "    ) else (\n";
  if len > 0 then
    Printf.printf "      set tick1 := 1'b0\n";
  Printf.printf "    )\n";
  Printf.printf "  }\n"; *)

  let gather_uses lt_start ident =
    let res = ref [] in
    List.iter (
      function
      | InsnBind (bind_ident, ExprInvoke (invoked_ident, delay, vals)) ->
        let invoked_comp = find_spawn comp invoked_ident |> find_comp prog in
        List.iter2 (fun (inp_ident, input_lt) v ->
          if v = ValIdent ident then (
            res := (input_lt.lt_start + delay - lt_start, Printf.sprintf "%s_in_%s_right" invoked_ident inp_ident)::!res
          )
        ) invoked_comp.comp_input vals
      | InsnRet ret_ident ->
        if ret_ident = ValIdent ident then
          res := (0, Printf.sprintf "%s_out" comp.comp_ident)::!res
      | _ -> ()
    ) comp.comp_body;
    List.fast_sort (fun (delay1, _) (delay2, _) -> delay1 - delay2) !res
  in

  (* now just loops *)
  (* for each input and each invocation, a loop *)
  List.iter (fun (inp_ident, inp_lt) ->
    Printf.printf "  loop {\n";
    Printf.printf "    let r = recv %s_in_%s.data in\n" comp.comp_ident inp_ident;
    Printf.printf "    r =>\n";
    (* cycle 0 starts now *)
    (* figure out how the value is used *)
    let uses = gather_uses inp_lt.lt_start inp_ident in
    let cur_cycle = ref 0 in
    List.iter (fun (delay, endpoint) ->
      if !cur_cycle < delay then (
        Printf.printf "    cycle %d =>\n" (delay - !cur_cycle);
        cur_cycle := delay
      );
      Printf.printf "    send %s.data (r) =>\n" endpoint
    ) uses;
    Printf.printf "    cycle 1\n";
    Printf.printf "  }\n"
  ) comp.comp_input;
  (* for each invocation, a loop to receive the result *)
  List.iter (
    function
    | InsnBind (bind_ident, ExprInvoke (invoked_ident, delay, vals)) ->
      let invoked_comp = find_spawn comp invoked_ident |> find_comp prog in
      let delay_received = delay + invoked_comp.comp_output.lt_start in
      Printf.printf "  loop {\n";
      Printf.printf "    let r = recv %s_out_left.data in\n" invoked_ident;
      Printf.printf "    r =>\n";
      (* cycle 0 starts now *)
      let uses = gather_uses delay_received bind_ident in
      let cur_cycle = ref 0 in
      List.iter (fun (delay, endpoint) ->
        if !cur_cycle < delay then (
          Printf.printf "    cycle %d =>\n" (delay - !cur_cycle);
          cur_cycle := delay
        );
        Printf.printf "    send %s.data (r) =>\n" endpoint
      ) uses;
      Printf.printf "    cycle 1\n";
      Printf.printf "  }\n"
    | _ -> ()
  ) comp.comp_body


(* translates a Filament program into an Anvil program *)
let translate prog =
  Printf.printf "import \"Reg.anvil\"\n";
  (* translate each component *)
  List.iter generate_chan_classes prog.prog_comps;
  List.iter (translate_comp prog) prog.prog_comps

let () =
  translate {
    (* sample program *)
    prog_comps = [
      {
        comp_ident = "Alu";
        comp_interval = 1;
        comp_input = [
          ("l", {lt_start = 0; lt_end = 1});
          ("r", {lt_start = 0; lt_end = 1});
          ("op", {lt_start = 2; lt_end = 3});
        ];
        comp_output = {lt_start = 3; lt_end = 4};
        comp_body = [
          InsnNew ("reg0", "Reg");
          InsnNew ("reg1", "Reg");
          InsnNew ("reg2", "Reg");
          InsnBind ("l1", ExprInvoke ("reg0", 0, [ValIdent "l"]));
          InsnBind ("l2", ExprInvoke ("reg1", 1, [ValIdent "l1"]));
          InsnBind ("l3", ExprInvoke ("reg2", 2, [ValIdent "l2"]));
          InsnRet (ValIdent "l3");
        ];
      }
    ]
  }
