open Lang

module StringMap = Map.Make(String)
type 'a string_map = 'a StringMap.t

type port_def = {
  dir: message_direction;
  dtype: data_type;
  name: identifier;
}

type wire_id = int
type cycle_id = int

type condition = {
  w: wire_id;
  neg: bool; (* negate? *)
}

type reg_assign = {
  conds: condition list;
  ident: identifier;
  expr_str: string;
}

type expr_result = {
  v: (wire_id * data_type) list;
  assigns: reg_assign list; (* assignment to regs only *)
}

type wire_def = {
  id: wire_id;
  dtype: data_type;
}

type cycle_node = {
  id: cycle_id;
  assigns: reg_assign list; (* assignment to regs only *)
  next_switch: (wire_id * (cycle_id option)) list;
  next_default: cycle_id option;
}

type assign = {
  wire : wire_id;
  expr_str : string;
}


type codegen_context = {
  mutable wires: wire_def list;
  mutable wires_n: int;
  mutable cycles: cycle_node list;
  mutable cycles_n: int;
  mutable assigns: assign list;
  mutable first_cycle: int;
  msgs: (identifier * (message_def list)) list;
}

let codegen_context_proc_clear (ctx : codegen_context) =
  ctx.wires <- [];
  ctx.wires_n <- 0;
  ctx.cycles <- [];
  ctx.cycles_n <- 0;
  ctx.assigns <- []

let codegen_context_new_wire (ctx: codegen_context) (dtype : data_type) : wire_id =
  let id = ctx.wires_n in
  ctx.wires_n <- id + 1;
  ctx.wires <- {id = id; dtype = dtype}::ctx.wires;
  id

let codegen_context_new_cycle (ctx : codegen_context) : cycle_id =
  let id = ctx.cycles_n in
  ctx.cycles_n <- id + 1;
  id

let codegen_context_add_cycle (ctx : codegen_context) (cycle : cycle_node) =
  ctx.cycles <- cycle::ctx.cycles

let codegen_context_new_assign (ctx : codegen_context) (a : assign) =
  ctx.assigns <- a::ctx.assigns

let rec format_dtype_split dtype =
  match dtype with
  | Logic -> ("logic", "")
  | Type typename -> (typename, "")
  | Array (dtype', n) ->
      let (base, arrs) = format_dtype_split dtype' in
      let idx = Printf.sprintf "[%d:0]" (n - 1) in
      (base, idx ^ arrs)

let format_dtype dtype =
  let (base, arrs) = format_dtype_split dtype
  in base ^ arrs

let format_port port =
  let inout = match port.dir with
    | In -> "input"
    | Out -> "output"
  in inout ^ " " ^ format_dtype port.dtype ^ " " ^ port.name

let rec print_port_list port_list =
  match port_list with
  | [] -> ()
  | port :: [] ->
      let port_fmt = format_port port in print_endline ("  " ^ port_fmt);
  | port :: port_list' ->
      let port_fmt = format_port port in print_endline ("  " ^ port_fmt ^ ",");
      print_port_list port_list'

let gather_ports_from_msg (msg_def : message_def) =
  let folder = fun (n, port_list) (stype : sig_type) ->
    let new_port : port_def = {
      name = msg_def.name ^ "_" ^ (string_of_int n);
      dir = msg_def.dir;
      dtype = stype.dtype;
    } in (n + 1, new_port::port_list)
  in let (_, res) = List.fold_left folder (0, []) msg_def.sig_types in res

let gather_ports (msg_list : message_def list) =
  List.fold_left (fun port_list msg_def -> (gather_ports_from_msg msg_def) @
    port_list) [] msg_list

let codegen_ports (msg_list : message_def list) =
  let clk_port = {
    dir = In; dtype = Logic; name = "clk_i"
  } in
  let rst_port = {
    dir = In; dtype = Logic; name = "rst_ni"
  } in
  let port_list = gather_ports msg_list in
    print_port_list ([clk_port; rst_port] @ port_list)

let format_regname_current (regname : identifier) = regname ^ "_q"
let format_regname_next (regname : identifier) = regname ^ "_n"

let codegen_state_transition (regs : reg_def list) =
  match regs with
  | [] -> ()
  | _ ->
    print_endline "  always_ff @(posedge clk_i or negedge rst_ni) begin: state_transition";
    print_endline "    if (~rst_ni) begin";
    let codegen_reg_reset = fun (r: reg_def) ->
      let init_val_str = Option.value ~default:"'0" r.init in
      Printf.printf "      %s <= %s;\n" (format_regname_current r.name) init_val_str
    in List.iter codegen_reg_reset regs;
    print_endline "    end else begin";
    let codegen_reg_next = fun (r: reg_def) ->
      Printf.printf "      %s <= %s;\n"
        (format_regname_current r.name) (format_regname_next r.name)
    in List.iter codegen_reg_next regs;
    print_endline "    end";
    print_endline "  end"

let codegen_regs_declare (regs : reg_def list) =
  let codegen_reg = fun (r: reg_def) ->
    Printf.printf "  %s %s, %s;\n" (format_dtype r.dtype)
      (format_regname_current r.name) (format_regname_next r.name)
  in List.iter codegen_reg regs

let format_wirename (id : wire_id) : string = "wire" ^ (string_of_int id)
let format_statename (id : cycle_id) : string = "STATE" ^ (string_of_int id)

let format_condition (c : condition) : string =
  let prefix = if c.neg then "" else "!" in prefix ^ (format_wirename c.w)

let print_assign (a : assign) =
  Printf.printf "  assign %s = %s;\n" (format_wirename a.wire) a.expr_str

let string_of_literal (lit : literal) : string =
  let len = List.length lit in
  let bit_str = String.concat "" (List.map string_of_bit lit) in
  (string_of_int len) ^ "'b" ^ bit_str

let get_identifier_dtype (_ctx : codegen_context) (proc : proc_def) (ident : identifier) : data_type option =
  let m = fun (r: reg_def) -> if r.name = ident then Some r.dtype else None in
  List.find_map m proc.regs

let rec codegen_expr (ctx : codegen_context) (proc : proc_def)
                     (conds : condition list) (env : wire_def string_map) (e : expr) : expr_result =
  match e with
  | Literal lit ->
      let dtype = Array (Logic, List.length lit) in
      let id = codegen_context_new_wire ctx dtype in
      codegen_context_new_assign ctx {wire = id; expr_str = string_of_literal lit};
      {v = [(id, dtype)]; assigns = []}
  | Identifier ident ->
      (* only supports registers for now *)
      (
        match StringMap.find_opt ident env with
        | Some w -> {v = [(w.id, w.dtype)]; assigns = []}
        | None ->
            let dtype = Option.get (get_identifier_dtype ctx proc ident) in
            let id = codegen_context_new_wire ctx dtype in
            codegen_context_new_assign ctx {wire = id; expr_str = format_regname_current ident};
            {v = [(id, dtype)]; assigns = []}
      )
  | Function _  -> {v = []; assigns = []}
  | Send _ -> {v = []; assigns = []}
  | Recv _ -> {v = []; assigns = []}
  | Apply _ -> {v = []; assigns = []}
  | Binop (binop, e1, e2) ->
      let e1_res = codegen_expr ctx proc conds env e1 in
      let e2_res = codegen_expr ctx proc conds env e2 in
      begin
        match e1_res.v, e2_res.v with
        | [(id1, dtype1)], [(id2, dtype2)] ->
            if dtype1 = dtype2 then
              let id = codegen_context_new_wire ctx dtype1 in
              let expr_str = Printf.sprintf "%s %s %s"
                (format_wirename id1) (string_of_binop binop) (format_wirename id2) in
              codegen_context_new_assign ctx {wire = id; expr_str = expr_str};
              {v = [(id, dtype1)]; assigns = e1_res.assigns @ e2_res.assigns}
            else {v = []; assigns = []}
        | _ -> {v = []; assigns = []}
      end
  | Unop _ -> {v = []; assigns = []}
  | Tuple elist ->
      let expr_res = List.map (codegen_expr ctx proc conds env) elist in
      {
        v = List.concat (List.map (fun (x: expr_result) -> x.v) expr_res);
        assigns = List.concat (List.map (fun (x: expr_result) -> x.assigns) expr_res);
      }
  | LetIn (ident, v_expr, body_expr) ->
      let v_res = codegen_expr ctx proc conds env v_expr in
      let (v_w, v_dtype) = List.hd v_res.v in
      let new_env = StringMap.add ident {id = v_w; dtype = v_dtype} env in
      let expr_res = codegen_expr ctx proc conds new_env body_expr in
      {
        v = expr_res.v;
        assigns = v_res.assigns @ expr_res.assigns
      }
  | IfExpr (cond_expr, e1, e2) ->
      let cond_res = codegen_expr ctx proc conds env cond_expr in
      let (cond_w, _) = List.hd cond_res.v in
      let e1_res = codegen_expr ctx proc ({w = cond_w; neg = false}::conds) env e1 in
      let e2_res = codegen_expr ctx proc ({w = cond_w; neg = true}::conds) env e2 in
      (* get the results *)
      let gen_result = fun (w1, dtype1) (w2, _) ->
        let new_w = codegen_context_new_wire ctx dtype1 in
        let expr_str = Printf.sprintf "(%s) ? %s : %s"
          (format_wirename cond_w) (format_wirename w1) (format_wirename w2) in
        let _ = codegen_context_new_assign ctx {wire = new_w; expr_str = expr_str} in
        (new_w, dtype1)
      in
      let v = List.map2 gen_result e1_res.v e2_res.v in
      {v = v; assigns = cond_res.assigns @ e1_res.assigns @ e2_res.assigns}
  | Assign (ident, e_val) ->
      let expr_res = codegen_expr ctx proc conds env e_val in
      let codegen_assign = fun (w, _) -> {
        conds = conds;
        ident = ident;
        expr_str = format_wirename w
       } in
      {v = []; assigns = (List.map codegen_assign expr_res.v) @ expr_res.assigns}
      (* assign evaluates to unit val *)

let rec codegen_proc_body (ctx :codegen_context) (proc : proc_def)
                  (pb : proc_body) (next_cycle : cycle_id option) : cycle_id option =
  let expr_res = codegen_expr ctx proc [] StringMap.empty pb.cycle in
  let new_cycle_id = codegen_context_new_cycle ctx in
  let (next_switch, next_default) = match pb.transition with
  | Seq -> ([], next_cycle)
  | If (cond, body) ->
      let next_cycle' = codegen_proc_body_list ctx proc body next_cycle in
      let cond_res = codegen_expr ctx proc [] StringMap.empty cond in
      let sw = List.map (fun (c, _) -> (c, next_cycle')) cond_res.v in
      (sw, next_cycle)
  | IfElse (cond, body1, body2) ->
      let next_cycle1 = codegen_proc_body_list ctx proc body1 next_cycle in
      let next_cycle2 = codegen_proc_body_list ctx proc body2 next_cycle in
      let cond_res = codegen_expr ctx proc [] StringMap.empty cond in
      let sw = List.map (fun (c, _) -> (c, next_cycle1)) cond_res.v in
      (sw, next_cycle2)
  | While (cond, body) ->
      let next_cycle' = codegen_proc_body_list ctx proc body (Some new_cycle_id) in
      let cond_res = codegen_expr ctx proc [] StringMap.empty cond in
      let sw = List.map (fun (c, _) -> (c, next_cycle')) cond_res.v in
      (sw, next_cycle)
  in
  let new_cycle = {id = new_cycle_id; assigns = expr_res.assigns; next_switch = next_switch; next_default = next_default} in
  begin
    codegen_context_add_cycle ctx new_cycle;
    Some new_cycle_id
  end

and codegen_proc_body_list (ctx : codegen_context) (proc : proc_def)
                  (body : proc_body_list) (next_cycle : cycle_id option): cycle_id option =
  match body with
  | [] -> next_cycle
  | pb::body' ->
      let next_cycle' = codegen_proc_body_list ctx proc body' next_cycle in
      codegen_proc_body ctx proc pb next_cycle'

let codegen_post_declare (ctx : codegen_context) =
  (* wire declarations *)
  let codegen_wire = fun (w: wire_def) ->
    Printf.printf "  %s %s;\n" (format_dtype w.dtype) (format_wirename w.id)
  in List.iter codegen_wire ctx.wires;
  (* wire assignments *)
  List.iter print_assign ctx.assigns

let codegen_state_machine (ctx : codegen_context) (proc : proc_def) =
  (* state definition *)
  let state_cnt = List.length ctx.cycles in
  let state_width = Utils.int_log2 (state_cnt - 1) in
    begin
      if state_width > 0 then begin
        Printf.printf "  typedef enum logic[%d:0] {\n" (state_width - 1);
        let codegen_state_def = fun id wire ->
          if id = 0 then
            Printf.printf "    %s" (format_statename wire.id)
          else
            Printf.printf ",\n    %s" (format_statename wire.id)
        in List.iteri codegen_state_def ctx.cycles;
        print_endline "\n  } _state_t;"
      end else ();
      print_endline "  always_comb begin : state_machine";

      (* default next reg values *)
      let assign_reg_default = fun (r: reg_def) ->
        Printf.printf "    %s = %s;\n" (format_regname_next r.name) (format_regname_current r.name)
      in List.iter assign_reg_default proc.regs;

      if state_width > 0 then
        print_endline "    unique case (_st_q)"
      else ();
      let codegen_cycle = fun (cycle: cycle_node) ->
        if state_width > 0 then
          Printf.printf "      %s: begin\n" (format_statename cycle.id)
        else ();

        (* choose the state transition signals and output for this cycle *)
        let assign_reg = fun assign ->
          let cond_str = if assign.conds = [] then "" else
            let cond_str_list = List.map format_condition assign.conds in
            let cond_a_str = String.concat " && " cond_str_list in
            Printf.sprintf "if (%s) " cond_a_str
          in
          Printf.printf "        %s%s = %s;\n" cond_str (format_regname_next assign.ident) assign.expr_str
        in List.iter assign_reg cycle.assigns;

        if state_width > 0 then
          (* state transition *)
          (* default next state *)
          let def_next_state = Option.value ~default:ctx.first_cycle cycle.next_default in
          Printf.printf "        _st_n = %s;\n" (format_statename def_next_state);
          (* switch *)
          let cond_next_state = fun (w_cond, c) ->
            let next_c = Option.value ~default:ctx.first_cycle c in
            Printf.printf "        if (%s) begin\n          _st_n = %s;\n        end\n"
              (format_wirename w_cond) (format_statename next_c)
          in List.iter cond_next_state cycle.next_switch;
          print_endline "      end"
        else ()
      in List.iter codegen_cycle ctx.cycles;
      if state_width > 0 then
        print_endline "    endcase"
      else ();
      print_endline "  end"
    end

let codegen_proc ctx (proc : proc_def) =
  (* generate ports *)
  Printf.printf "module %s (\n" proc.name;
  codegen_ports proc.msgs;
  print_endline ");";

  (* implicit state *)
  let first_cycle_op = codegen_proc_body_list ctx proc proc.body None in
  begin
    Option.iter (fun c -> ctx.first_cycle <- c) first_cycle_op;
    let regs = if List.length ctx.cycles > 1 then
      ({name = "_st"; dtype = Type "_state_t"; init = Some (format_statename ctx.first_cycle)}::proc.regs)
    else proc.regs in
      (codegen_regs_declare regs;
      codegen_state_transition regs);
    codegen_state_machine ctx proc;
    codegen_post_declare ctx
  end;

  print_endline "endmodule"

let rec codegen_with_context (ctx : codegen_context) (cunit : compilation_unit) =
  match cunit with
  | [] -> ()
  | proc :: cunit' ->
      codegen_context_proc_clear ctx;
      codegen_proc ctx proc;
      codegen_with_context ctx cunit'

let codegen (cunit : compilation_unit) =
  let msgs = List.map (fun (p: proc_def) -> (p.name, p.msgs)) cunit in
  let ctx : codegen_context = {
    wires = [];
    wires_n = 0;
    cycles = [];
    cycles_n = 0;
    first_cycle = 0;
    assigns = [];
    msgs = msgs;
  } in
  codegen_with_context ctx cunit
