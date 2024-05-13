open Lang
open Utils

(* borrows are when we need to check the lifetime constraints *)
type borrow_source =
| FromRef of identifier
| FromReg of identifier
(* these two are cycle-local constraints *)
| FromMsgData of message_specifier * int
| FromMsgRet of message_specifier * int


module BorrowDestination : sig
  type t =
  | ToRef of identifier (* bind to reference*)
  | ToReg of identifier (* set register *)
  | ToOthers (* pass as data or ret in msg; no need to record which it is *)

  val compare : t -> t -> int
end = struct
  type t =
  | ToRef of identifier (* bind to reference*)
  | ToReg of identifier (* set register *)
  | ToOthers (* pass as data or ret in msg; no need to record which it is *)

  let compare (a : t) (b : t) : int =
    match a, b with
    | ToRef _, ToReg _ | ToRef _, ToOthers | ToReg _, ToOthers -> -1
    | ToRef a_ident, ToRef b_ident -> String.compare a_ident b_ident
    | ToReg a_ident, ToReg b_ident -> String.compare a_ident b_ident
    | ToOthers, ToOthers -> 0
    | _ -> 1
end

type borrow_destination = BorrowDestination.t
module BorrowDestinationMap = Map.Make(BorrowDestination)


type borrow_info = {
  (* where does the borrow comes from *)
  src: borrow_source list;
  (* how long is this borrow *)
  lifetime: sig_lifetime;
  (* where does this borrow go *)
  dst: borrow_destination;
}


type wire_def = {
  name: identifier;
  ty: sig_type;
  borrow_src: borrow_source list;
}

type port_def = {
  dir: message_direction;
  dtype: data_type;
  name: identifier;
}

type wire_id = int
type cycle_id = int

type condition = {
  w: string;
  neg: bool; (* negate? *)
}

type reg_assign = {
  conds: condition list;
  ident: identifier;
  expr_str: string;
}

type expr_result = {
  v: wire_def list;
  assigns: reg_assign list; (* assignment to regs only *)
}

type cycle_node = {
  id: cycle_id;
  delay: future;
  assigns: reg_assign list; (* assignment to regs only *)
  borrows: borrow_info list;
  next_switch: (string * (cycle_id option)) list;
  next_default: cycle_id option;
}

type assign = {
  wire : string;
  expr_str : string;
}


type codegen_context = {
  (* temp wires only *)
  mutable wires: wire_def list;
  mutable wires_n: int;
  mutable cycles: cycle_node list;
  mutable cycles_n: int;
  mutable assigns: assign list;
  mutable first_cycle: int;
  (* endpoints defined in a process *)
  mutable endpoints: endpoint_def list;
  (* messages that are sent by this process, through either ports or wires *)
  mutable local_messages: (endpoint_def * message_def * message_direction) list;
  cunit: compilation_unit;
  (* msgs: (identifier * (message_def list)) list; *)
}

let lookup_proc (ctx: codegen_context) (name: identifier) : proc_def option =
  List.find_opt (fun (p : proc_def) -> p.name = name) ctx.cunit.procs

let lookup_ref (proc: proc_def) (name: identifier) : ref_def option =
  List.find_opt (fun (r : ref_def) -> r.name = name) proc.refs

let format_wirename (id : wire_id) : string = "wire" ^ (string_of_int id)
let format_statename (id : cycle_id) : string = "STATE" ^ (string_of_int id)

let format_condition (c : condition) : string =
  let prefix = if c.neg then "!" else "" in prefix ^ c.w

let codegen_context_proc_clear (ctx : codegen_context) =
  ctx.wires <- [];
  ctx.wires_n <- 0;
  ctx.cycles <- [];
  ctx.cycles_n <- 0;
  ctx.assigns <- []

let codegen_context_new_wire (ctx: codegen_context) (ty : sig_type)
    (borrow_src : borrow_source list): wire_def =
  let id = ctx.wires_n in
  let w = {name = format_wirename id; ty = ty; borrow_src = borrow_src} in
  ctx.wires_n <- id + 1;
  ctx.wires <- w::ctx.wires;
  w

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

let format_refname name : string = "_ref_" ^ name

let wire_of_ref (r: ref_def) : wire_def = { name = r.name; ty = r.ty; borrow_src = [FromRef r.name] }

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

let endpoint_is_canonical (endpoint: endpoint_def) : bool =
  (Option.is_none endpoint.opp) || (endpoint.dir = Left)

let endpoint_canonical_name (endpoint: endpoint_def) : identifier =
  match endpoint.dir with
  | Left -> endpoint.name
  | Right -> Option.value ~default:endpoint.name endpoint.opp

let lookup_channel_class (ctx : codegen_context) (name : identifier) : channel_class_def option =
  List.find_opt (fun (cc : channel_class_def) -> cc.name = name) ctx.cunit.channel_classes

let format_msg_data_signal_name (endpoint_name : identifier) (message_name : identifier) (data_idx : int) : string =
  endpoint_name ^ "_" ^ message_name ^ "_" ^ (string_of_int data_idx)

let format_msg_ret_signal_name (endpoint_name : identifier) (message_name : identifier) (ret_idx : int) : string =
  endpoint_name ^ "_" ^ message_name ^ "_" ^ (string_of_int ret_idx) ^ "_ret"

let format_msg_valid_signal_name (endpoint_name : identifier) (message_name : identifier) : string =
  endpoint_name ^ "_" ^ message_name ^ "_valid"

let format_msg_ack_signal_name (endpoint_name : identifier) (message_name : identifier) : string =
  endpoint_name ^ "_" ^ message_name ^ "_ack"

let lookup_endpoint (ctx : codegen_context) (proc : proc_def) (endpoint_name : identifier) : endpoint_def option =
  let match_fun = fun (p : endpoint_def) -> p.name = endpoint_name in
  let local_endpoint_opt = List.find_opt match_fun ctx.endpoints in
  if Option.is_none local_endpoint_opt then
    List.find_opt match_fun proc.args
  else
    local_endpoint_opt

let canonicalise (ctx : codegen_context) (proc : proc_def)
    fmt (endpoint_name : identifier) =
  let endpoint_name' =
    let endpoint = Option.get (lookup_endpoint ctx proc endpoint_name) in
    endpoint.name
  in fmt endpoint_name'

let lookup_message_def_by_msg (ctx : codegen_context) (proc : proc_def)
            (msg_spec : message_specifier) : message_def option =
  let ( >>= ) = Option.bind in
  lookup_endpoint ctx proc msg_spec.endpoint >>=
    (fun endpoint -> lookup_channel_class ctx endpoint.channel_class) >>=
    (fun cc -> List.find_opt (fun (m : message_def) -> m.name = msg_spec.msg) cc.messages)

let gather_ret_wires_from_msg (ctx : codegen_context) (proc : proc_def) (msg_spec : message_specifier) : wire_def list =
  let endpoint = Option.get (lookup_endpoint ctx proc msg_spec.endpoint) in
  let cc = Option.get (lookup_channel_class ctx endpoint.channel_class) in
  let msg = List.find (fun (m : message_def) -> m.name = msg_spec.msg) cc.messages in
  let endpoint_name = endpoint_canonical_name endpoint in
  let mapper = fun (idx : int) (ty : sig_type) : wire_def ->
    {
      name = format_msg_ret_signal_name endpoint_name msg_spec.msg idx;
      ty = ty;
      borrow_src = [FromMsgRet (msg_spec, idx)];
    } in
  List.mapi mapper msg.ret_types

let gather_data_wires_from_msg (ctx : codegen_context) (proc : proc_def) (msg_spec : message_specifier) : wire_def list =
  let endpoint = Option.get (lookup_endpoint ctx proc msg_spec.endpoint) in
  let cc = Option.get (lookup_channel_class ctx endpoint.channel_class) in
  let msg = List.find (fun (m : message_def) -> m.name = msg_spec.msg) cc.messages in
  let endpoint_name = endpoint_canonical_name endpoint in
  let mapper = fun (idx : int) (ty : sig_type) : wire_def ->
    {
      name = format_msg_data_signal_name endpoint_name msg_spec.msg idx;
      ty = ty;
      borrow_src = [FromMsgData (msg_spec, idx)];
    } in
  List.mapi mapper msg.sig_types

let gather_all_wires_from_msg (ctx : codegen_context) (proc : proc_def) (msg_spec : message_specifier) : wire_def list =
  {
    name = canonicalise ctx proc format_msg_valid_signal_name msg_spec.endpoint msg_spec.msg;
    ty = {
      dtype = Logic;
      lifetime = sig_lifetime_const; (* should not matter as this is not directly referenceable *)
    };
    borrow_src = [];
  }::
  {
    name = canonicalise ctx proc format_msg_ack_signal_name msg_spec.endpoint msg_spec.msg;
    ty = {
      dtype = Logic;
      lifetime = sig_lifetime_const;
    };
    borrow_src = [];
  }::
  (gather_data_wires_from_msg ctx proc msg_spec)@(gather_ret_wires_from_msg ctx proc msg_spec)

let gather_ports_from_endpoint (ctx : codegen_context) (endpoint : endpoint_def) : port_def list =
  let cc = Option.get (lookup_channel_class ctx endpoint.channel_class) in
  let gen_endpoint_ports = fun (msg : message_def) ->
    let folder_inner = fun fmt msg_dir (n, port_list) (stype : sig_type) ->
      let new_port : port_def = {
        name = fmt endpoint.name msg.name n;
        dir = msg_dir;
        dtype = stype.dtype;
      } in (n + 1, new_port::port_list)
    in
    let msg_data_dir = get_message_direction msg.dir endpoint.dir in
    let (_, res0) = List.fold_left (folder_inner format_msg_data_signal_name msg_data_dir) (0, []) msg.sig_types in
    let (_, res) = List.fold_left (folder_inner format_msg_ret_signal_name (reverse msg_data_dir)) (0, res0) msg.ret_types in
    let valid_port = { name = format_msg_valid_signal_name endpoint.name msg.name; dir = msg_data_dir; dtype = Logic} in
    let ack_port = {name = format_msg_ack_signal_name endpoint.name msg.name; dir = reverse msg_data_dir; dtype = Logic} in
    ack_port::valid_port::res
  in List.concat_map gen_endpoint_ports cc.messages

let gather_ports (ctx : codegen_context) (endpoints : endpoint_def list) : port_def list =
  List.concat_map (gather_ports_from_endpoint ctx) endpoints

let codegen_ports (ctx : codegen_context) (endpoints : endpoint_def list) =
  let clk_port = {
    dir = In; dtype = Logic; name = "clk_i"
  } in
  let rst_port = {
    dir = In; dtype = Logic; name = "rst_ni"
  } in
  let port_list = gather_ports ctx endpoints in
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


let print_assign (a : assign) =
  Printf.printf "  assign %s = %s;\n" a.wire a.expr_str

let string_of_literal (lit : literal) =
  match lit with
  | Binary (len, b) -> Printf.sprintf "%d'b%s" len (List.map string_of_digit b |> String.concat "")
  | Decimal (len, d) -> Printf.sprintf "%d'd%s" len (List.map string_of_digit d |> String.concat "")
  | Hexadecimal (len, h) -> Printf.sprintf "%d'h%s" len (List.map string_of_digit h |> String.concat "")
  | NoLength n -> string_of_int n

let get_identifier_dtype (_ctx : codegen_context) (proc : proc_def) (ident : identifier) : data_type option =
  let m = fun (r: reg_def) -> if r.name = ident then Some r.dtype else None in
  List.find_map m proc.regs

let rec codegen_expr (ctx : codegen_context) (proc : proc_def)
                     (conds : condition list) (env : wire_def string_map)
                     (borrows : (borrow_info list) ref) (e : expr) : expr_result =
  match e with
  | Literal lit ->
      (* TODO: no-length literal not supported here *)
      let dtype = Array (Logic, literal_bit_len lit |> Option.get) in
      let ty = { dtype = dtype; lifetime = sig_lifetime_const } in
      let w = codegen_context_new_wire ctx ty [] in (* literal does not depend on anything *)
      codegen_context_new_assign ctx {wire = w.name; expr_str = string_of_literal lit};
      {v = [w]; assigns = []}
  | Identifier ident ->
      (* only supports registers for now *)
      (
        match StringMap.find_opt ident env with
        | Some w -> {v = [w]; assigns = []}
        | None ->
            let dtype = Option.get (get_identifier_dtype ctx proc ident) in
            let ty = { dtype = dtype; lifetime = sig_lifetime_const } in
            let w = codegen_context_new_wire ctx ty [FromReg ident]  in
            codegen_context_new_assign ctx {wire = w.name; expr_str = format_regname_current ident};
            {v = [w]; assigns = []}
      )
  | Function _  -> {v = []; assigns = []}
  | TrySend _ -> {v = []; assigns = []}
  | TryRecv _ -> {v = []; assigns = []}
  | Apply _ -> {v = []; assigns = []}
  | Binop (binop, e1, e2) ->
      let e1_res = codegen_expr ctx proc conds env borrows e1 in
      let e2_res = codegen_expr ctx proc conds env borrows e2 in
      begin
        match e1_res.v, e2_res.v with
        | [w1], [w2] ->
            (* if w1.ty.dtype = w2.ty.dtype then *)
            (* TODO: compute the new lifetime; type checking *)
            let w = codegen_context_new_wire ctx w1.ty (w1.borrow_src @ w2.borrow_src) in
            let expr_str = Printf.sprintf "%s %s %s"
              w1.name (string_of_binop binop) w2.name in
            codegen_context_new_assign ctx {wire = w.name; expr_str = expr_str};
            {v = [w]; assigns = e1_res.assigns @ e2_res.assigns}
            (* else {v = []; assigns = []} *)
        | _ -> {v = []; assigns = []}
      end
  | Unop (unop, e') ->
    let e_res = codegen_expr ctx proc conds env borrows e' in
    let w = List.hd e_res.v in
    let new_dtype =
      match unop with
      | Neg | Not -> w.ty.dtype
      | AndAll | OrAll -> Logic
    in
    let w' = codegen_context_new_wire ctx {w.ty with dtype = new_dtype} w.borrow_src in
    let expr_str = Printf.sprintf "%s%s" (string_of_unop unop) w.name in
    codegen_context_new_assign ctx {wire = w'.name; expr_str = expr_str};
    {v = [w']; assigns = e_res.assigns}
  | Tuple elist ->
      let expr_res = List.map (codegen_expr ctx proc conds env borrows) elist in
      {
        v = List.concat_map (fun (x: expr_result) -> x.v) expr_res;
        assigns = List.concat_map (fun (x: expr_result) -> x.assigns) expr_res
      }
  | LetIn (ident, v_expr, body_expr) ->
      let v_res = codegen_expr ctx proc conds env borrows v_expr in
      let new_env =
        if ident = "_" then env
        else
          let w = List.hd v_res.v in
          StringMap.add ident w env
      in
      let expr_res = codegen_expr ctx proc conds new_env borrows body_expr in
      {
        v = expr_res.v;
        assigns = v_res.assigns @ expr_res.assigns
      }
  | IfExpr (cond_expr, e1, e2) ->
      let cond_res = codegen_expr ctx proc conds env borrows cond_expr in
      let cond_w = List.hd cond_res.v in
      let e1_res = codegen_expr ctx proc ({w = cond_w.name; neg = false}::conds) env borrows e1 in
      let e2_res = codegen_expr ctx proc ({w = cond_w.name; neg = true}::conds) env borrows e2 in
      (* get the results *)
      let gen_result = fun (w1 : wire_def) (w2 : wire_def) ->
        (* compute new lifetime *)
        let new_w = codegen_context_new_wire ctx w1.ty (cond_w.borrow_src @ w1.borrow_src @ w2.borrow_src) in
        let expr_str = Printf.sprintf "(%s) ? %s : %s" cond_w.name w1.name w2.name in
        let _ = codegen_context_new_assign ctx {wire = new_w.name; expr_str = expr_str} in
        new_w
      in
      let v = List.map2 gen_result e1_res.v e2_res.v in
      {v = v; assigns = cond_res.assigns @ e1_res.assigns @ e2_res.assigns}
  | Assign (ident, e_val) ->
      let expr_res = codegen_expr ctx proc conds env borrows e_val in
      let codegen_assign = fun (w : wire_def) ->
        (* assign borrows for one cycle *)
        borrows := {src = w.borrow_src; lifetime = sig_lifetime_this_cycle; dst = ToReg ident}::!borrows;
        {
          conds = conds;
          ident = ident;
          expr_str = w.name
        } in
        {v = []; assigns = (List.map codegen_assign expr_res.v) @ expr_res.assigns}
      (* assign evaluates to unit val *)
  | Return (endpoint_ident, msg_ident, e_val) ->
      let msg_spec = {endpoint = endpoint_ident; msg = msg_ident} in
      let expr_res = codegen_expr ctx proc conds env borrows e_val in
      let assign_ret = fun (w : wire_def) (ret_wire : wire_def) ->
        (* we have to add to assign because the signals might be used for multiple cycles *)
        borrows := {src = w.borrow_src; lifetime = ret_wire.ty.lifetime; dst = ToOthers}::!borrows;
        codegen_context_new_assign ctx {wire = ret_wire.name; expr_str = w.name}
      in
      begin
        List.iter2 assign_ret expr_res.v (gather_ret_wires_from_msg ctx proc msg_spec);
        {v = []; assigns = expr_res.assigns}
      end
  | Ref (ident, e_val) ->
      let expr_res = codegen_expr ctx proc conds env borrows e_val in
      let w_res = List.hd expr_res.v in
      let r = lookup_ref proc ident |> Option.get in
      let _ = codegen_context_new_assign ctx {wire = format_refname ident; expr_str = w_res.name } in
      borrows := {src = w_res.borrow_src; lifetime = r.ty.lifetime; dst = ToRef ident}::!borrows;
      {v = []; assigns = expr_res.assigns}

let rec codegen_proc_body (ctx : codegen_context) (proc : proc_def)
                  (pb : proc_body) (next_cycle : cycle_id option) : cycle_id option =
  (* env for refs; TODO: refs always available for now but should be limited by lifetime; compute the lifetime *)
  let ref_env =
    let gen_ref_map = fun (r : ref_def) -> (r.name, wire_of_ref r) in
    List.map gen_ref_map proc.refs |> map_of_list
  in
  let borrows = ref [] in
  let (delay, init_cond, init_env) =
    (* we only look at the first delay for now *)
    match List.nth_opt pb.delays 0 with
    | Some (Send (idents, msg_specifier, expr)) ->
        (* bind return results *)
        let env = gather_ret_wires_from_msg ctx proc msg_specifier |>
          List.combine idents |> map_of_list in
        (* gather the data to send *)
        let expr_res = codegen_expr ctx proc [] ref_env borrows expr in
        gather_data_wires_from_msg ctx proc msg_specifier |>
          List.iter2 (fun (res_wire : wire_def) (data_wire : wire_def) ->
            borrows := {src = res_wire.borrow_src; lifetime = data_wire.ty.lifetime; dst = ToOthers}::!borrows;
            codegen_context_new_assign ctx {wire = data_wire.name; expr_str = res_wire.name}) expr_res.v;
        let cond : condition list = [{
          w = canonicalise ctx proc format_msg_ack_signal_name msg_specifier.endpoint msg_specifier.msg;
          neg = false;
        }] in
        (AtSend msg_specifier, cond, env)
    | Some (Recv (idents, msg_specifier)) ->
        let env = gather_data_wires_from_msg ctx proc msg_specifier |>
          List.combine idents |> map_of_list in
        let cond : condition list = [{
          w = canonicalise ctx proc format_msg_valid_signal_name msg_specifier.endpoint msg_specifier.msg;
          neg = false;
        }] in
        (AtRecv msg_specifier, cond, env)
    | _ -> (Cycles 0, [], StringMap.empty) in
  let expr_res =
    let merger = fun _ a b -> if Option.is_none a then b else a in
    codegen_expr ctx proc init_cond (StringMap.merge merger ref_env init_env) borrows pb.cycle in
  let new_cycle_id = codegen_context_new_cycle ctx in
  let (next_switch, next_default) = match pb.transition with
  | Seq -> ([], next_cycle)
  | If body ->
      let next_cycle' = codegen_proc_body_list ctx proc body next_cycle in
      let cond_w = List.hd expr_res.v in
      borrows := { src = cond_w.borrow_src; lifetime = sig_lifetime_this_cycle; dst = ToOthers }::!borrows;
      ([(cond_w.name, next_cycle')], next_cycle)
  | IfElse (body1, body2) ->
      let next_cycle1 = codegen_proc_body_list ctx proc body1 next_cycle in
      let next_cycle2 = codegen_proc_body_list ctx proc body2 next_cycle in
      let cond_w = List.hd expr_res.v in
      borrows := { src = cond_w.borrow_src; lifetime = sig_lifetime_this_cycle; dst = ToOthers }::!borrows;
      ([(cond_w.name, next_cycle1)], next_cycle2)
  | While body ->
      let next_cycle' = codegen_proc_body_list ctx proc body (Some new_cycle_id) in
      let cond_w = List.hd expr_res.v in
      borrows := { src = cond_w.borrow_src; lifetime = sig_lifetime_this_cycle; dst = ToOthers }::!borrows;
      ([(cond_w.name, next_cycle')], next_cycle)
  in
  let new_cycle = {
    id = new_cycle_id;
    assigns = expr_res.assigns;
    delay = delay;
    borrows = !borrows;
    next_switch = next_switch;
    next_default = next_default
  } in
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

let codegen_post_declare (ctx : codegen_context) (proc : proc_def)=
  (* wire declarations *)
  let ref_wires = List.map wire_of_ref proc.refs in
  let codegen_wire = fun (w: wire_def) ->
    Printf.printf "  %s %s;\n" (format_dtype w.ty.dtype) w.name
  in List.iter codegen_wire (ctx.wires @ ref_wires);
  (* wire assignments *)
  List.iter print_assign ctx.assigns

let gather_out_indicators (ctx : codegen_context) : identifier list =
  let msg_map = fun ((endpoint, msg, msg_dir) : endpoint_def * message_def * message_direction) ->
    let indicator_formatter = match msg_dir with
    | In -> format_msg_ack_signal_name
    | Out -> format_msg_valid_signal_name
    in indicator_formatter (endpoint_canonical_name endpoint) msg.name
  in List.map msg_map ctx.local_messages

exception CodegenError of string

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

      (* default output indicators, we need to know which messages are local-sent/received *)
      List.iter (Printf.printf "    %s = '0;\n") (gather_out_indicators ctx);

      if state_width > 0 then
        print_endline "    _st_n = _st_q;\n    unique case (_st_q)"
      else ();
      let codegen_cycle = fun (cycle: cycle_node) ->
        if state_width > 0 then
          Printf.printf "      %s: begin\n" (format_statename cycle.id)
        else ();

        (* if has blocking send, set the valid indicator *)
        let transition_cond = match cycle.delay with
        | Cycles _ -> None
        | AtSend msg_spec ->
            Printf.printf "        %s = 1'b1;\n"
              (canonicalise ctx proc format_msg_valid_signal_name msg_spec.endpoint msg_spec.msg);
            Some (canonicalise ctx proc format_msg_ack_signal_name msg_spec.endpoint msg_spec.msg)
        | AtRecv msg_spec ->
            Printf.printf "        if (%s) %s = 1'b1;\n"
              (canonicalise ctx proc format_msg_valid_signal_name msg_spec.endpoint msg_spec.msg)
              (canonicalise ctx proc format_msg_ack_signal_name msg_spec.endpoint msg_spec.msg);
            Some (canonicalise ctx proc format_msg_valid_signal_name msg_spec.endpoint msg_spec.msg)
        | Eternal -> raise (CodegenError "Bad delay for a cycle.")
        in
        let (transition_begin, transition_end) =
          match transition_cond with
          | Some cond -> (Printf.sprintf "        if (%s) begin\n" cond, "        end\n")
          | None -> ("", "")
        in

        (* choose the state transition signals and output for this cycle *)
        let assign_reg = fun assign ->
          let cond_str = if assign.conds = [] then "" else
            let cond_str_list = List.map format_condition assign.conds in
            let cond_a_str = String.concat " && " cond_str_list in
            Printf.sprintf "if (%s) " cond_a_str
          in
          Printf.printf "        %s%s = %s;\n" cond_str (format_regname_next assign.ident) assign.expr_str
        in
        begin
          List.iter assign_reg cycle.assigns;

          if state_width > 0 then
          begin
            (* state transition *)
            print_string transition_begin;
            (* default next state *)
            let def_next_state = Option.value ~default:ctx.first_cycle cycle.next_default in
            Printf.printf "          _st_n = %s;\n" (format_statename def_next_state);
            let cond_next_state = fun (w_cond, c) ->
              let next_c = Option.value ~default:ctx.first_cycle c in
              Printf.printf "          if (%s) _st_n = %s;\n"
                w_cond (format_statename next_c)
            in List.iter cond_next_state cycle.next_switch;
            print_string transition_end;
            print_endline "      end"
          end else ()
        end
      in List.iter codegen_cycle ctx.cycles;
      if state_width > 0 then
        print_endline "    endcase"
      else ();
      print_endline "  end"
    end

(* get endpoints for extra channels *)
let codegen_channels (ctx: codegen_context) (channels : channel_def list) =
  let codegen_chan = fun (chan : channel_def) ->
    let (left_foreign, right_foreign) =
      match chan.visibility with
      | BothForeign -> (true, true)
      | LeftForeign -> (true, false)
      | RightForeign -> (false, true)
    in
    let left_endpoint = { name = chan.endpoint_left; channel_class = chan.channel_class;
                          dir = Left; foreign = left_foreign; opp = Some chan.endpoint_right } in
    let right_endpoint = { name = chan.endpoint_right; channel_class = chan.channel_class;
                          dir = Right; foreign = right_foreign; opp = Some chan.endpoint_left } in
    [left_endpoint; right_endpoint]
  in
  ctx.endpoints <- List.concat_map codegen_chan channels

let codegen_endpoints (ctx: codegen_context) =
  let print_port_signal_decl = fun (port : port_def) ->
    Printf.printf "  %s %s;\n" (format_dtype port.dtype) (port.name)
  in
  List.filter (fun (p : endpoint_def) -> p.dir = Left) ctx.endpoints |>
  gather_ports ctx |>
  List.iter print_port_signal_decl

let gather_local_messages (ctx: codegen_context) (proc: proc_def): (endpoint_def * message_def * message_direction) list =
  let gather_from_endpoint = fun (endpoint: endpoint_def) ->
    let cc = Option.get (lookup_channel_class ctx endpoint.channel_class) in
    let msg_map = fun (msg: message_def) ->
      let msg_dir = get_message_direction msg.dir endpoint.dir in
      (endpoint, msg, msg_dir)
    in List.map msg_map cc.messages
  in
  List.filter (fun p -> not p.foreign) (proc.args @ ctx.endpoints) |>
  List.concat_map gather_from_endpoint

let codegen_spawns (ctx: codegen_context) (proc: proc_def) =
  let gen_connect = fun (dst : string) (src : string) ->
    Printf.printf ",\n    .%s (%s)" dst src
  in
  let gen_spawn = fun (idx : int) (spawn : spawn_def) ->
    Printf.printf "  %s _spawn_%d (\n    .clk_i,\n    .rst_ni" spawn.proc idx;
    (* connect the wires *)
    let proc_other = lookup_proc ctx spawn.proc |> Option.get in
    let connect_endpoints = fun (arg_endpoint : endpoint_def) (param_ident : identifier) ->
      let endpoint_local = lookup_endpoint ctx proc param_ident |> Option.get in
      let endpoint_name_local = endpoint_canonical_name endpoint_local in
      let cc = lookup_channel_class ctx endpoint_local.channel_class |> Option.get in
      let print_msg_con = fun (msg : message_def) ->
        gen_connect (format_msg_valid_signal_name arg_endpoint.name msg.name)
          (format_msg_valid_signal_name endpoint_name_local msg.name);
        gen_connect (format_msg_ack_signal_name arg_endpoint.name msg.name)
          (format_msg_ack_signal_name endpoint_name_local msg.name);
        let print_data_con = fun fmt idx _ ->
          gen_connect (fmt arg_endpoint.name msg.name idx)
            (fmt endpoint_name_local msg.name idx)
        in begin
          List.iteri (print_data_con format_msg_data_signal_name) msg.sig_types;
          List.iteri (print_data_con format_msg_ret_signal_name) msg.ret_types
        end
      in List.iter print_msg_con cc.messages
    in List.iter2 connect_endpoints proc_other.args spawn.params;
    Printf.printf "\n  );\n"
  in List.iteri gen_spawn proc.spawns

(** A variant of borrow_info for borrow checking *)
type borrow_check_info = {
  src: borrow_source list;
  tight: sig_lifetime;
  relaxed: sig_lifetime;
  pinned: bool;
}

type borrow_map = borrow_check_info BorrowDestinationMap.t

type borrow_check_cycle_state = {
  borrows: borrow_map
}

type borrow_check_state = {
  cycle_nodes: cycle_node array;
  cycle_states: borrow_check_cycle_state array;
}

exception BorrowCheckError of string

let borrow_check_get_lifetime_relaxed (cycle_state : borrow_check_cycle_state) (d : borrow_destination) : sig_lifetime =
  BorrowDestinationMap.find_opt d cycle_state.borrows |>
  Option.map (fun (bci : borrow_check_info) -> bci.relaxed) |>
  Option.value ~default:sig_lifetime_null

let borrow_check_get_lifetime_tight (cycle_state : borrow_check_cycle_state) (d : borrow_destination) : sig_lifetime =
  BorrowDestinationMap.find_opt d cycle_state.borrows |>
  Option.map (fun (bci : borrow_check_info) -> bci.tight) |>
  Option.value ~default:sig_lifetime_null

(** Performs checks on each cycle that borrowing is always valid *)
let borrow_check_inspect (state : borrow_check_state)
                         (ctx : codegen_context) (proc : proc_def) =
  let chk_cycle = fun (cycle_node : cycle_node) (cycle_state : borrow_check_cycle_state) ->
    (* check each borrowing *)
    let chk_borrow = fun (bi : borrow_info) ->
      (* check that the references borrowed have sufficient lifetime *)
      let chk_source = fun (src : borrow_source) ->
        match src with
        | FromMsgData (msg_spec, n) ->
          let msg_def = lookup_message_def_by_msg ctx proc msg_spec |> Option.get in
          let msg_ty = List.nth msg_def.sig_types n in
          if not (Lifetime.lifetime_covered_by bi.lifetime msg_ty.lifetime) then
            let err_msg = Printf.sprintf "Insufficient lifetime of message data: %s::%s, %d (requiring %s but got %s)"
              msg_spec.endpoint msg_spec.msg n (string_of_lifetime bi.lifetime) (string_of_lifetime msg_ty.lifetime) in
            BorrowCheckError err_msg |>
            raise
          else ()
        | FromMsgRet (msg_spec, n) ->
          let msg_def = lookup_message_def_by_msg ctx proc msg_spec |> Option.get in
          let msg_ty = List.nth msg_def.ret_types n in
          if not (Lifetime.lifetime_covered_by bi.lifetime msg_ty.lifetime) then
            let err_msg = Printf.sprintf "Insufficient lifetime of message return value: %s::%s, %d (requiring %s but got %s)"
              msg_spec.endpoint msg_spec.msg n (string_of_lifetime bi.lifetime) (string_of_lifetime msg_ty.lifetime) in
            BorrowCheckError err_msg |>
            raise
          else ()
        | FromRef ref_ident ->
          let lt = borrow_check_get_lifetime_tight cycle_state (ToRef ref_ident) in
          if not (Lifetime.lifetime_covered_by bi.lifetime lt) then
            raise (BorrowCheckError "Insufficient lifetime of reference!")
          else ()
        | FromReg _ -> () (* regs are always good *)
      in
      List.iter chk_source bi.src;

      let chk_mutate = fun (reg_assign : reg_assign) ->
        (* now check all borrows of the register in this cycle *)
        let chk_reg_borrows = fun _ (bci : borrow_check_info) ->
          if List.find_opt (fun (s : borrow_source) -> s = FromReg reg_assign.ident) bci.src |> Option.is_some then
            if Lifetime.lifetime_overlaps_current_mutation bci.relaxed then
              let err_msg = Printf.sprintf "Mutation of borrowed registers detected: %s (borrowed for %s)"
                reg_assign.ident (string_of_lifetime bci.relaxed) in
              raise (BorrowCheckError err_msg)
            else ()
          else ()
        in
        BorrowDestinationMap.iter chk_reg_borrows cycle_state.borrows
      in
      List.iter chk_mutate cycle_node.assigns
    in
    List.iter chk_borrow cycle_node.borrows;
  in
  Array.iter2 chk_cycle state.cycle_nodes state.cycle_states
          (* check the current *)
      (* check that the mutated stuff *)


let borrow_check_merge (state : borrow_check_state)
                       (cycle_idx : int)
                       (m : borrow_map)
                       (e : future) : bool =
  let changed = ref false in
  let map_merge = fun (_dst : borrow_destination) (old_opt : borrow_check_info option) (new_opt : borrow_check_info option) ->
    let new_opt' = Option.map (fun nw ->
      let new_tight = Lifetime.consume_lifetime_must_live nw.tight e
      and new_relaxed = Lifetime.consume_lifetime_might_live nw.relaxed e in
      {nw with tight = new_tight; relaxed = new_relaxed;})
      new_opt in
    let r = match old_opt, new_opt' with
    | None, _ -> new_opt'
    | Some _, None -> old_opt
    | Some od, _ when od.pinned -> old_opt
    | Some od, Some nw ->
        Some ({
          src = od.src;
          tight = Lifetime.lifetime_merge_tight od.tight nw.tight;
          relaxed = Lifetime.lifetime_merge_relaxed od.relaxed nw.relaxed;
          pinned = false;
        }) in
    changed := !changed || old_opt <> r;
    r in
  let update = fun (old_map : borrow_check_cycle_state) : borrow_check_cycle_state ->
    {borrows = BorrowDestinationMap.merge map_merge old_map.borrows m} in
  let old = Array.get state.cycle_states cycle_idx in
  Array.set state.cycle_states cycle_idx (update old);
  !changed

let rec borrow_check_internal (state : borrow_check_state)
                          (ctx : codegen_context)
                          (proc : proc_def) =
  let to_continue = ref false in
  let transit_state = fun cycle_node cycle_state ->
    let visit_next_cycle = fun (nxt : cycle_id) ->
      let cycle_next : cycle_node = Array.get state.cycle_nodes nxt in
      to_continue := (borrow_check_merge state nxt cycle_state.borrows cycle_next.delay) || !to_continue;
    in
    let next_default = Option.value ~default:ctx.first_cycle cycle_node.next_default in
    visit_next_cycle next_default;
    List.map (fun ns -> snd ns |> Option.value ~default:ctx.first_cycle) cycle_node.next_switch |>
    List.iter visit_next_cycle;
  in
  Array.iter2 transit_state state.cycle_nodes state.cycle_states;
  if !to_continue then borrow_check_internal state ctx proc
  else borrow_check_inspect state ctx proc

(** Perform checks on the borrowing lifetimes and decide whether the operations should be allowed *)
let borrow_check (ctx : codegen_context) (proc : proc_def) =
  let make_cycle_state = fun (c : cycle_node) : borrow_check_cycle_state ->
    { borrows = List.map (fun (bi : borrow_info) : (borrow_destination * borrow_check_info) ->
      (bi.dst, {src = bi.src; relaxed = bi.lifetime; tight = bi.lifetime; pinned = true})) c.borrows |>
    List.to_seq |> BorrowDestinationMap.of_seq } in
  let cycle_states = List.map make_cycle_state ctx.cycles |> Array.of_list in
  let state : borrow_check_state = {
    cycle_states = cycle_states;
    cycle_nodes = Array.of_list ctx.cycles;
  } in
  borrow_check_internal state ctx proc

let codegen_proc ctx (proc : proc_def) =
  (* generate ports *)
  Printf.printf "module %s (\n" proc.name;
  codegen_ports ctx proc.args;
  print_endline ");";

  (* implicit state *)
  let first_cycle_op = codegen_proc_body_list ctx proc proc.body None in
  Option.iter (fun c -> ctx.first_cycle <- c) first_cycle_op;
  let regs = if List.length ctx.cycles > 1 then
    ({name = "_st"; dtype = Type "_state_t"; init = Some (format_statename ctx.first_cycle)}::proc.regs)
  else proc.regs in
  begin
    codegen_regs_declare regs;
    codegen_state_transition regs
  end;
  codegen_channels ctx proc.channels;
  codegen_endpoints ctx;
  ctx.local_messages <- gather_local_messages ctx proc;
  codegen_spawns ctx proc;
  codegen_state_machine ctx proc;
  codegen_post_declare ctx proc;

  print_endline "endmodule";

  try borrow_check ctx proc with BorrowCheckError err_msg
    -> Printf.eprintf "Borrow check failed: %s\n" err_msg

let rec codegen_with_context (ctx : codegen_context) (procs: proc_def list) =
  match procs with
  | [] -> ()
  | proc :: procs' ->
      codegen_context_proc_clear ctx;
      codegen_proc ctx proc;
      codegen_with_context ctx procs'

let codegen (cunit : compilation_unit) =
  (* let msgs = List.map (fun (p: proc_def) -> (p.name, p.msgs)) cunit in *)
  let ctx : codegen_context = {
    wires = [];
    wires_n = 0;
    cycles = [];
    cycles_n = 0;
    first_cycle = 0;
    assigns = [];
    cunit = cunit;
    endpoints = [];
    local_messages = [];
    (* msgs = msgs; *)
  } in
  codegen_with_context ctx cunit.procs
