open Lang
open Utils

exception UnimplementedError of string

let message_has_valid_port (msg : message_def) : bool = msg.send_sync = Dynamic
let message_has_ack_port (msg : message_def) : bool = msg.recv_sync = Dynamic

(* borrows are when we need to check the lifetime constraints *)
type borrow_source =
| FromRef of identifier
| FromReg of identifier
(* these two are cycle-local constraints *)
| FromMsgData of message_specifier * int


module BorrowDestination : sig
  type t =
  | ToRef of identifier (* bind to reference*)
  | ToReg of identifier (* set register *)
  | ToOthers (* pass as data in msg; no need to record which it is *)

  val compare : t -> t -> int
end = struct
  type t =
  | ToRef of identifier (* bind to reference*)
  | ToReg of identifier (* set register *)
  | ToOthers (* pass as data in msg; no need to record which it is *)

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
  (* is this in the delay area of the block? *)
  in_delay: bool;
  (* how long is this borrow *)
  lifetime: sig_lifetime;
  (* where does this borrow go *)
  dst: borrow_destination;
}


type wire_def = {
  name: identifier;
  dtype: data_type;
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

type index_range = {
  le: int;
  ri: int;
}

let index_range_transform (base_opt : index_range option) (offset : index_range) : index_range =
  match base_opt with
  | None -> offset
  | Some base -> {le = base.le + offset.le; ri = base.le + offset.ri}

let variant_lookup_range
          (v : [< `Variant of (identifier * data_type option) list]) (cstr: identifier) (type_defs : type_def_map) : index_range option =
  let tag_size = variant_tag_size v in
  variant_lookup_dtype v cstr |> Option.map (fun x -> let data_size = data_type_size type_defs x in {le = tag_size; ri = tag_size + data_size - 1})

type lvalue_evaluated = {
  ident: identifier;
  range: index_range option;
  dtype: data_type;
}

type reg_assign = {
  conds: condition list;
  lval: lvalue_evaluated;
  expr_str: string;
}

type expr_result = {
  v: wire_def list;
  assigns: reg_assign list; (* assignment to regs only *)
}

type cycle_node = {
  id: cycle_id;
  delay: delay_def;
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

let codegen_context_new_wire (ctx: codegen_context) (dtype : data_type)
    (borrow_src : borrow_source list): wire_def =
  let id = ctx.wires_n in
  let w = {name = format_wirename id; dtype; borrow_src = borrow_src} in
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

let format_dtype (ctx : codegen_context) (dtype : data_type) =
  match dtype with
  | `Logic -> "logic"
  | `Opaque typename -> typename
  | _ -> (data_type_size ctx.cunit.type_defs dtype) - 1 |> Printf.sprintf "logic[%d:0]"

let format_refname name : string = "_ref_" ^ name

let wire_of_ref (r: ref_def) : wire_def = { name = r.name; dtype = r.ty.dtype; borrow_src = [FromRef r.name] }

let format_port (ctx : codegen_context) port =
  let inout = match port.dir with
    | In -> "input"
    | Out -> "output"
  in inout ^ " " ^ format_dtype ctx port.dtype ^ " " ^ port.name

let rec print_port_list (ctx : codegen_context) port_list =
  match port_list with
  | [] -> ()
  | port :: [] ->
      let port_fmt = format_port ctx port in print_endline ("  " ^ port_fmt);
  | port :: port_list' ->
      let port_fmt = format_port ctx port in print_endline ("  " ^ port_fmt ^ ",");
      print_port_list ctx port_list'

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

let gather_data_wires_from_msg (ctx : codegen_context) (proc : proc_def) (msg_spec : message_specifier) : (wire_def * sig_lifetime) list =
  let endpoint = Option.get (lookup_endpoint ctx proc msg_spec.endpoint) in
  let cc = Option.get (lookup_channel_class ctx endpoint.channel_class) in
  let msg = List.find (fun (m : message_def) -> m.name = msg_spec.msg) cc.messages in
  let endpoint_name = endpoint_canonical_name endpoint in
  let mapper = fun (idx : int) (ty : sig_type_chan_local) : (wire_def * sig_lifetime) ->
    ({
      name = format_msg_data_signal_name endpoint_name msg_spec.msg idx;
      dtype = ty.dtype;
      borrow_src = [FromMsgData (msg_spec, idx)];
    }, lifetime_globalise endpoint_name ty.lifetime) in
  List.mapi mapper msg.sig_types

let gather_ports_from_endpoint (ctx : codegen_context) (endpoint : endpoint_def) : port_def list =
  let cc = Option.get (lookup_channel_class ctx endpoint.channel_class) in
  let gen_endpoint_ports = fun (msg : message_def) ->
    let folder_inner = fun fmt msg_dir (n, port_list) (stype : sig_type_chan_local) ->
      let new_port : port_def = {
        name = fmt endpoint.name msg.name n;
        dir = msg_dir;
        dtype = stype.dtype;
      } in (n + 1, new_port::port_list)
    in
    let msg_data_dir = get_message_direction msg.dir endpoint.dir in
    let (_, res) = List.fold_left (folder_inner format_msg_data_signal_name msg_data_dir) (0, []) msg.sig_types in
    let res =
      if message_has_valid_port msg then
        let valid_port = { name = format_msg_valid_signal_name endpoint.name msg.name; dir = msg_data_dir; dtype = `Logic} in
        valid_port::res
      else res
    in
    if message_has_ack_port msg then
      let ack_port = {name = format_msg_ack_signal_name endpoint.name msg.name; dir = reverse msg_data_dir; dtype = `Logic} in
      ack_port::res
    else res
  in List.concat_map gen_endpoint_ports cc.messages

let gather_ports (ctx : codegen_context) (endpoints : endpoint_def list) : port_def list =
  List.concat_map (gather_ports_from_endpoint ctx) endpoints

let codegen_ports (ctx : codegen_context) (endpoints : endpoint_def list) =
  let clk_port = {
    dir = In; dtype = `Logic; name = "clk_i"
  } in
  let rst_port = {
    dir = In; dtype = `Logic; name = "rst_ni"
  } in
  let port_list = gather_ports ctx endpoints in
    print_port_list ctx ([clk_port; rst_port] @ port_list)

let lookup_reg (proc : proc_def) (name : identifier) : reg_def option =
  List.find_opt (fun (x : reg_def) -> x.name = name) proc.regs

let format_regname_current (regname : identifier) = regname ^ "_q"
let format_regname_next (regname : identifier) = regname ^ "_n"

let format_lval_next (lval : lvalue_evaluated) : string =
  let ind_str =
    match lval.range with
    | None -> ""
    | Some {le = offset_le; ri = offset_ri} -> Printf.sprintf "[%d:%d]" offset_ri offset_le
  in (format_regname_next lval.ident) ^ ind_str

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

let codegen_regs_declare (ctx : codegen_context) (regs : reg_def list) =
  let codegen_reg = fun (r: reg_def) ->
    Printf.printf "  %s %s, %s;\n" (format_dtype ctx r.dtype)
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

(* TODO: here we should use mux instead direct assigns *)
let assign_message_data (ctx : codegen_context) (proc : proc_def) (borrows : borrow_info list ref)
                        (in_delay : bool) (msg_spec : message_specifier) (data_ws : wire_def list) =
  gather_data_wires_from_msg ctx proc msg_spec |>
    List.iter2 (fun (res_wire : wire_def) ((data_wire, lt) : (wire_def * sig_lifetime)) ->
        borrows := {src = res_wire.borrow_src; in_delay = in_delay; lifetime = lt; dst = ToOthers}::!borrows;
        codegen_context_new_assign ctx {wire = data_wire.name; expr_str = res_wire.name}) data_ws

let message_ack_wirename (ctx : codegen_context) (proc : proc_def) (msg_spec : message_specifier) : string =
  canonicalise ctx proc format_msg_ack_signal_name msg_spec.endpoint msg_spec.msg

let message_valid_wirename (ctx : codegen_context) (proc : proc_def) (msg_spec : message_specifier) : string =
  canonicalise ctx proc format_msg_valid_signal_name msg_spec.endpoint msg_spec.msg

let format_cstrname (cstr : identifier) : string = Printf.sprintf "_CSTR_%s" cstr

let rec evaluate_lvalue (ctx : codegen_context) (proc : proc_def) (lval : lvalue) : lvalue_evaluated option =
  let ( let* ) = Option.bind in
  match lval with
  | Reg ident ->
      let* reg = lookup_reg proc ident in
      Some {ident = ident; range = None; dtype = reg.dtype}
  | Indexed (lval', ind) ->
      let* lval_eval' = evaluate_lvalue ctx proc lval' in
      let* (offset_le, offset_ri, new_dtype) = data_type_index ctx.cunit.type_defs lval_eval'.dtype ind in
      let new_range = index_range_transform lval_eval'.range {le = offset_le; ri = offset_ri - 1} in
      Some {lval_eval' with range = Some new_range; dtype = new_dtype}
  | Indirected (lval', fieldname) ->
      let* lval_eval' = evaluate_lvalue ctx proc lval' in
      let* (offset_le, offset_ri, new_dtype) = data_type_indirect ctx.cunit.type_defs lval_eval'.dtype fieldname in
      let new_range = index_range_transform lval_eval'.range {le = offset_le; ri = offset_ri - 1} in
      Some {lval_eval' with range = Some new_range; dtype = new_dtype}

let rec codegen_expr (ctx : codegen_context) (proc : proc_def)
                     (conds : condition list) (env : wire_def string_map)
                     (borrows : (borrow_info list) ref) (in_delay : bool) (e : expr) : expr_result =
  match e with
  | Literal lit ->
      (* TODO: no-length literal not supported here *)
      let dtype = `Array (`Logic, literal_bit_len lit |> Option.get) in
      let w = codegen_context_new_wire ctx dtype [] in (* literal does not depend on anything *)
      codegen_context_new_assign ctx {wire = w.name; expr_str = string_of_literal lit};
      {v = [w]; assigns = []}
  | Identifier ident ->
      (* only supports registers for now *)
      (
        match StringMap.find_opt ident env with
        | Some w -> {v = [w]; assigns = []}
        | None ->
            let dtype = Option.get (get_identifier_dtype ctx proc ident) in
            let w = codegen_context_new_wire ctx dtype [FromReg ident]  in
            codegen_context_new_assign ctx {wire = w.name; expr_str = format_regname_current ident};
            {v = [w]; assigns = []}
      )
  | Function _  -> {v = []; assigns = []}
  | TrySend (send_pack, e_succ, e_fail) ->
      let data_res = codegen_expr ctx proc conds env borrows in_delay send_pack.send_data in
      assign_message_data ctx proc borrows in_delay send_pack.send_msg_spec data_res.v;
      let has_ack = lookup_message_def_by_msg ctx proc send_pack.send_msg_spec |> Option.get |> message_has_ack_port in
      if has_ack then
        let ack_w = message_ack_wirename ctx proc send_pack.send_msg_spec in
        let succ_res = codegen_expr ctx proc ({w = ack_w; neg = false}::conds) env borrows in_delay e_succ
        and fail_res = codegen_expr ctx proc ({w = ack_w; neg = true}::conds) env borrows in_delay e_fail in
        let succ_w = List.hd succ_res.v and fail_w = List.hd fail_res.v in
        let w = codegen_context_new_wire ctx succ_w.dtype (succ_w.borrow_src @ fail_w.borrow_src) in
        let expr_str = Printf.sprintf "(%s) ? %s : %s" ack_w succ_w.name fail_w.name in
        codegen_context_new_assign ctx {wire = w.name; expr_str = expr_str};
        {v = [w]; assigns = data_res.assigns @ succ_res.assigns @ fail_res.assigns}
      else
        (* TODO: static checks *)
        codegen_expr ctx proc conds env borrows in_delay e_succ
  | TryRecv (recv_pack, e_succ, e_fail) ->
      let has_valid = lookup_message_def_by_msg ctx proc recv_pack.recv_msg_spec |> Option.get |> message_has_valid_port in
      let succ_env = gather_data_wires_from_msg ctx proc recv_pack.recv_msg_spec |>
        List.map fst |>
        List.combine recv_pack.recv_binds |> map_of_list |>
        StringMap.merge (fun _ op1 op2 -> if Option.is_some op2 then op2 else op1) env in
      if has_valid then
        let valid_w = message_valid_wirename ctx proc recv_pack.recv_msg_spec in
        let succ_res = codegen_expr ctx proc ({w = valid_w; neg = false}::conds) succ_env borrows in_delay e_succ
        and fail_res = codegen_expr ctx proc ({w = valid_w; neg = true}::conds) succ_env borrows in_delay e_fail in
        let succ_w = List.hd succ_res.v and fail_w = List.hd fail_res.v in
        let w = codegen_context_new_wire ctx succ_w.dtype (succ_w.borrow_src @ fail_w.borrow_src) in
        let expr_str = Printf.sprintf "(%s) ? %s : %s" valid_w succ_w.name fail_w.name in
        codegen_context_new_assign ctx {wire = w.name; expr_str = expr_str};
        {v = [w]; assigns = succ_res.assigns @ fail_res.assigns}
      else
        (* TODO: static checks *)
        codegen_expr ctx proc conds succ_env borrows in_delay e_succ
  | Apply _ -> {v = []; assigns = []}
  | Binop (binop, e1, e2) ->
      let e1_res = codegen_expr ctx proc conds env borrows in_delay e1 in
      let e2_res = codegen_expr ctx proc conds env borrows in_delay e2 in
      begin
        match e1_res.v, e2_res.v with
        | [w1], [w2] ->
            (* if w1.ty.dtype = w2.ty.dtype then *)
            (* TODO: compute the new lifetime; type checking *)
            let w = codegen_context_new_wire ctx w1.dtype (w1.borrow_src @ w2.borrow_src) in
            let expr_str = Printf.sprintf "%s %s %s"
              w1.name (string_of_binop binop) w2.name in
            codegen_context_new_assign ctx {wire = w.name; expr_str = expr_str};
            {v = [w]; assigns = e1_res.assigns @ e2_res.assigns}
            (* else {v = []; assigns = []} *)
        | _ -> {v = []; assigns = []}
      end
  | Unop (unop, e') ->
    let e_res = codegen_expr ctx proc conds env borrows in_delay e' in
    let w = List.hd e_res.v in
    let new_dtype =
      match unop with
      | Neg | Not -> w.dtype
      | AndAll | OrAll -> `Logic
    in
    let w' = codegen_context_new_wire ctx new_dtype w.borrow_src in
    let expr_str = Printf.sprintf "%s%s" (string_of_unop unop) w.name in
    codegen_context_new_assign ctx {wire = w'.name; expr_str = expr_str};
    {v = [w']; assigns = e_res.assigns}
  | Tuple elist ->
      let expr_res = List.map (codegen_expr ctx proc conds env borrows in_delay) elist in
      {
        v = List.concat_map (fun (x: expr_result) -> x.v) expr_res;
        assigns = List.concat_map (fun (x: expr_result) -> x.assigns) expr_res
      }
  | LetIn (ident, v_expr, body_expr) ->
      let v_res = codegen_expr ctx proc conds env borrows in_delay v_expr in
      let new_env =
        if ident = "_" then env
        else
          let w = List.hd v_res.v in
          StringMap.add ident w env
      in
      let expr_res = codegen_expr ctx proc conds new_env borrows in_delay body_expr in
      {
        v = expr_res.v;
        assigns = v_res.assigns @ expr_res.assigns
      }
  | IfExpr (cond_expr, e1, e2) ->
      let cond_res = codegen_expr ctx proc conds env borrows in_delay cond_expr in
      let cond_w = List.hd cond_res.v in
      let e1_res = codegen_expr ctx proc ({w = cond_w.name; neg = false}::conds) env borrows in_delay e1 in
      let e2_res = codegen_expr ctx proc ({w = cond_w.name; neg = true}::conds) env borrows in_delay e2 in
      (* get the results *)
      let gen_result = fun (w1 : wire_def) (w2 : wire_def) ->
        (* compute new lifetime *)
        let new_w = codegen_context_new_wire ctx w1.dtype (cond_w.borrow_src @ w1.borrow_src @ w2.borrow_src) in
        let expr_str = Printf.sprintf "(%s) ? %s : %s" cond_w.name w1.name w2.name in
        let _ = codegen_context_new_assign ctx {wire = new_w.name; expr_str = expr_str} in
        new_w
      in
      let v = List.map2 gen_result e1_res.v e2_res.v in
      {v = v; assigns = cond_res.assigns @ e1_res.assigns @ e2_res.assigns}
  | Assign (lval, e_val) ->
      let lval_eval = evaluate_lvalue ctx proc lval |> Option.get in
      let expr_res = codegen_expr ctx proc conds env borrows in_delay e_val in
      let codegen_assign = fun (w : wire_def) ->
        (* assign borrows for one cycle *)
        borrows := {src = w.borrow_src; in_delay; lifetime = sig_lifetime_this_cycle; dst = ToReg lval_eval.ident}::!borrows;
        {
          conds = conds;
          lval = lval_eval;
          expr_str = w.name
        } in
      {v = []; assigns = (List.map codegen_assign expr_res.v) @ expr_res.assigns}
      (* assign evaluates to unit val *)
  | Ref (ident, e_val) ->
      let expr_res = codegen_expr ctx proc conds env borrows in_delay e_val in
      let w_res = List.hd expr_res.v in
      let r = lookup_ref proc ident |> Option.get in
      let _ = codegen_context_new_assign ctx {wire = format_refname ident; expr_str = w_res.name } in
      borrows := {src = w_res.borrow_src; in_delay; lifetime = r.ty.lifetime; dst = ToRef ident}::!borrows;
      {v = []; assigns = expr_res.assigns}
  | Construct (_cstr_ident, _cstr_args) ->
      raise (UnimplementedError "Construct expression unimplemented!")
  | Index (e', ind) ->
      let expr_res = codegen_expr ctx proc conds env borrows in_delay e' in
      let w_res = List.hd expr_res.v in
      let (offset_le, offset_ri, new_dtype) = data_type_index ctx.cunit.type_defs w_res.dtype ind |> Option.get in
      let new_w = codegen_context_new_wire ctx new_dtype w_res.borrow_src in
      let expr_str = Printf.sprintf "%s[%d:%d]" w_res.name offset_le (offset_ri - 1) in
      codegen_context_new_assign ctx {wire = new_w.name; expr_str = expr_str};
      {v = [new_w]; assigns = expr_res.assigns}
  | Indirect (e', fieldname) ->
      let expr_res = codegen_expr ctx proc conds env borrows in_delay e' in
      let w_res = List.hd expr_res.v in
      let (offset_le, offset_ri, new_dtype) = data_type_indirect ctx.cunit.type_defs w_res.dtype fieldname |> Option.get in
      let new_w = codegen_context_new_wire ctx new_dtype w_res.borrow_src in
      let expr_str = Printf.sprintf "%s[%d:%d]" w_res.name offset_le (offset_ri - 1) in
      codegen_context_new_assign ctx {wire = new_w.name; expr_str = expr_str};
      {v = [new_w]; assigns = expr_res.assigns}
  | Concat components ->
      let comp_res = List.map (codegen_expr ctx proc conds env borrows in_delay) components in
      let wires = List.map (fun x -> List.hd x.v) comp_res in
      let borrow_src = List.concat_map (fun x -> x.borrow_src) wires in
      let size = List.fold_left (fun s (x : wire_def) -> s + (data_type_size ctx.cunit.type_defs x.dtype)) 0 wires in
      let new_w = codegen_context_new_wire ctx (`Array (`Logic, size)) borrow_src in
      let expr_str = List.map (fun (x: wire_def) -> x.name) wires |> String.concat ", " |>  Printf.sprintf "{%s}" in
      codegen_context_new_assign ctx {wire = new_w.name; expr_str = expr_str};
      {v = [new_w]; assigns = List.concat_map (fun (x : expr_result) -> x.assigns) comp_res}
  | Match (e', match_arm_list) ->
      let expr_res = codegen_expr ctx proc conds env borrows in_delay e' in
      let w = List.hd expr_res.v in
      let dtype_resolved = data_type_name_resolve ctx.cunit.type_defs w.dtype in
      match dtype_resolved with
      | Some dtype  ->
        begin
          match dtype with
          | `Variant _ as var ->
            let ( let* ) = Option.bind in
            let tag_size = variant_tag_size var
            and arm_conds : string list ref = ref [] in
            let process_arm = fun ((pattern, e_opt) : (match_pattern * expr option)) ->
              let range_opt = variant_lookup_range var pattern.cstr ctx.cunit.type_defs in
              let cstr_idx = variant_lookup_index var pattern.cstr |> Option.get in
              let new_cond = Printf.sprintf "%s[%d:0] == %d" w.name (tag_size - 1) cstr_idx in
              arm_conds := new_cond::!arm_conds;
              let* e_arm = e_opt in
              let cond_str = String.concat " || " !arm_conds |> Printf.sprintf "(%s)" in
              let new_env =
                match range_opt, pattern.bind_name with
                | None, None -> env
                | Some range, Some bind_name ->
                    let arm_vw = codegen_context_new_wire ctx dtype w.borrow_src in
                    let arm_v_expr_str = Printf.sprintf "%s[%d:%d]" w.name range.ri range.le in
                    codegen_context_new_assign ctx {wire = arm_vw.name; expr_str = arm_v_expr_str};
                    StringMap.add bind_name arm_vw env
                | _ -> raise (TypeError "Pattern matching incompatible with type definition!")
              in
              let arm_res = codegen_expr ctx proc ({w = cond_str; neg = false}::conds) new_env borrows in_delay e_arm in
              arm_conds := [];
              Some (new_cond, arm_res)
            in
            let arm_res_list = List.filter_map process_arm match_arm_list in
            let arm_expr_str = List.map (fun ((c, r) : (identifier * expr_result)) ->
              Printf.sprintf "(%s) ? %s" c (List.hd r.v).name) arm_res_list |> String.concat " : " |> Printf.sprintf "%s : '0" in
            let new_dtype = (List.hd arm_res_list |> snd |>  (fun (x : expr_result) -> x.v) |> List.hd).dtype in
            let new_w = codegen_context_new_wire ctx new_dtype (List.concat_map
              (fun ((_, x) : (identifier * expr_result)) -> (List.hd x.v).borrow_src) arm_res_list) in
            codegen_context_new_assign ctx {wire = new_w.name; expr_str = arm_expr_str};
            {v = [new_w]; assigns = List.concat_map (fun ((_, x) : (identifier * expr_result)) -> x.assigns) arm_res_list}
          | _ ->  raise (TypeError "Illegal match: value is not of a variant type!")
        end
      | None -> raise (TypeError "Illegal match: value is not of a variant type!")

let rec codegen_proc_body (ctx : codegen_context) (proc : proc_def)
                  (pb : proc_body) (next_cycle : cycle_id option) : cycle_id option =
  (* env for refs; TODO: refs always available for now but should be limited by lifetime; compute the lifetime *)
  let ref_env =
    let gen_ref_map = fun (r : ref_def) -> (r.name, wire_of_ref r) in
    List.map gen_ref_map proc.refs |> map_of_list
  in
  let borrows = ref [] in
  let delay = List.nth_opt pb.delays 0 |> Option.value ~default:delay_immediate in
  let (init_cond, init_env) =
    (* we only look at the first delay for now *)
    match delay with
    | `Send {send_msg_spec = msg_specifier; send_data = expr} ->
        (* gather the data to send *)
        let expr_res = codegen_expr ctx proc [] ref_env borrows true expr in
        assign_message_data ctx proc borrows true msg_specifier expr_res.v;
        let cond : condition list =
          if lookup_message_def_by_msg ctx proc msg_specifier |> Option.get |> message_has_ack_port then
            [{
              w = message_ack_wirename ctx proc msg_specifier;
              neg = false;
            }]
          else []
        in
        (cond, StringMap.empty)
    | `Recv {recv_binds = idents; recv_msg_spec = msg_specifier} ->
        let env = gather_data_wires_from_msg ctx proc msg_specifier |>
          List.map fst |>
          List.combine idents |> map_of_list in
        let cond : condition list =
          if lookup_message_def_by_msg ctx proc msg_specifier |> Option.get |> message_has_valid_port then
            [{
              w = canonicalise ctx proc format_msg_valid_signal_name msg_specifier.endpoint msg_specifier.msg;
              neg = false;
            }]
          else []
        in
        (cond, env)
    | _ -> ([], StringMap.empty) (* TODO: support cycle delays *)
    in
  let expr_res =
    let merger = fun _ a b -> if Option.is_none a then b else a in
    codegen_expr ctx proc init_cond (StringMap.merge merger ref_env init_env) borrows false pb.cycle in
  let new_cycle_id = codegen_context_new_cycle ctx in
  let (next_switch, next_default) = match pb.transition with
  | Seq -> ([], next_cycle)
  | If body ->
      let next_cycle' = codegen_proc_body_list ctx proc body next_cycle in
      let cond_w = List.hd expr_res.v in
      borrows := { src = cond_w.borrow_src; in_delay = false; lifetime = sig_lifetime_this_cycle; dst = ToOthers }::!borrows;
      ([(cond_w.name, next_cycle')], next_cycle)
  | IfElse (body1, body2) ->
      let next_cycle1 = codegen_proc_body_list ctx proc body1 next_cycle in
      let next_cycle2 = codegen_proc_body_list ctx proc body2 next_cycle in
      let cond_w = List.hd expr_res.v in
      borrows := { src = cond_w.borrow_src; in_delay = false; lifetime = sig_lifetime_this_cycle; dst = ToOthers }::!borrows;
      ([(cond_w.name, next_cycle1)], next_cycle2)
  | While body ->
      let next_cycle' = codegen_proc_body_list ctx proc body (Some new_cycle_id) in
      let cond_w = List.hd expr_res.v in
      borrows := { src = cond_w.borrow_src; in_delay = false; lifetime = sig_lifetime_this_cycle; dst = ToOthers }::!borrows;
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
    Printf.printf "  %s %s;\n" (format_dtype ctx w.dtype) w.name
  in List.iter codegen_wire (ctx.wires @ ref_wires);
  (* wire assignments *)
  List.iter print_assign ctx.assigns

let gather_out_indicators (ctx : codegen_context) : identifier list =
  let msg_map = fun ((endpoint, msg, msg_dir) : endpoint_def * message_def * message_direction) ->
    let (indicator_formatter, checker) = match msg_dir with
    | In -> (format_msg_ack_signal_name, message_has_ack_port)
    | Out -> (format_msg_valid_signal_name, message_has_valid_port)
    in
    if checker msg then
      Some (indicator_formatter (endpoint_canonical_name endpoint) msg.name)
    else None
  in List.filter_map msg_map ctx.local_messages

exception CodegenError of string

let codegen_state_machine (ctx : codegen_context) (proc : proc_def) =
  (* state definition *)
  let state_cnt = List.length ctx.cycles in
  (* need to make it at least 1 bit wide *)
  let state_width = Utils.int_log2 (state_cnt - 1) |> max 1 in
    begin
      if state_cnt > 1 then begin
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

      if state_cnt > 1 then
        print_endline "    _st_n = _st_q;\n    unique case (_st_q)"
      else ();
      let codegen_cycle = fun (cycle: cycle_node) ->
        if state_cnt > 1 then
          Printf.printf "      %s: begin\n" (format_statename cycle.id)
        else ();


        (* if has blocking send, set the valid indicator *)
        let transition_cond = match cycle.delay with
        | `Cycles _ -> None
        | `Send {send_msg_spec = msg_spec; _} ->
            if lookup_message_def_by_msg ctx proc msg_spec |> Option.get |> message_has_valid_port then begin
              Printf.printf "        %s = 1'b1;\n"
                (canonicalise ctx proc format_msg_valid_signal_name msg_spec.endpoint msg_spec.msg);
              Some (canonicalise ctx proc format_msg_ack_signal_name msg_spec.endpoint msg_spec.msg)
            end else None
        | `Recv {recv_msg_spec = msg_spec; _} ->
            let msg_def = lookup_message_def_by_msg ctx proc msg_spec |> Option.get in
            let has_valid = message_has_valid_port msg_def
            and has_ack = message_has_ack_port msg_def in
            if has_ack then
              let pred =
                if has_valid then Printf.sprintf "if (%s) " @@ canonicalise ctx proc format_msg_valid_signal_name msg_spec.endpoint msg_spec.msg
                else ""
              in
              Printf.printf "        %s%s = 1'b1;\n" pred  @@ canonicalise ctx proc format_msg_ack_signal_name msg_spec.endpoint msg_spec.msg;
            else ();
            if has_valid then
              Some (canonicalise ctx proc format_msg_valid_signal_name msg_spec.endpoint msg_spec.msg)
            else None
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
          Printf.printf "        %s%s = %s;\n" cond_str (format_lval_next assign.lval) assign.expr_str
        in
        begin
          List.iter assign_reg cycle.assigns;

          if state_cnt > 1 then
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
      if state_cnt > 1 then
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
    Printf.printf "  %s %s;\n" (format_dtype ctx port.dtype) (port.name)
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
        if message_has_valid_port msg then
          gen_connect (format_msg_valid_signal_name arg_endpoint.name msg.name)
            (format_msg_valid_signal_name endpoint_name_local msg.name)
        else ();
        if message_has_ack_port msg then
          gen_connect (format_msg_ack_signal_name arg_endpoint.name msg.name)
            (format_msg_ack_signal_name endpoint_name_local msg.name)
        else ();
        let print_data_con = fun fmt idx _ ->
          gen_connect (fmt arg_endpoint.name msg.name idx)
            (fmt endpoint_name_local msg.name idx)
        in begin
          List.iteri (print_data_con format_msg_data_signal_name) msg.sig_types;
        end
      in List.iter print_msg_con cc.messages
    in List.iter2 connect_endpoints proc_other.args spawn.params;
    Printf.printf "\n  );\n"
  in List.iteri gen_spawn proc.spawns

(** A variant of borrow_info for borrow checking *)
type borrow_check_info = {
  bi: borrow_info;
  tight: sig_lifetime;
  relaxed: sig_lifetime;
  pinned: bool;
}

type borrow_map = borrow_check_info BorrowDestinationMap.t

type borrow_check_cycle_state = {
  borrows_in_delay: borrow_map;
  borrows_in_cycle: borrow_map;
}

type borrow_check_state = {
  cycle_nodes: cycle_node array;
  cycle_states: borrow_check_cycle_state array;
}

exception BorrowCheckError of string

let borrow_check_get_lifetime_relaxed (cycle_state : borrow_check_cycle_state) (in_delay : bool) (d : borrow_destination) : sig_lifetime =
  (if in_delay then cycle_state.borrows_in_delay else cycle_state.borrows_in_cycle) |>
  BorrowDestinationMap.find_opt d |>
  Option.map (fun (bci : borrow_check_info) -> bci.relaxed) |>
  Option.value ~default:sig_lifetime_null

let borrow_check_get_lifetime_tight (cycle_state : borrow_check_cycle_state) (in_delay : bool) (d : borrow_destination) : sig_lifetime =
  (if in_delay then cycle_state.borrows_in_delay else cycle_state.borrows_in_cycle) |>
  BorrowDestinationMap.find_opt d |>
  Option.map (fun (bci : borrow_check_info) -> bci.tight) |>
  Option.value ~default:sig_lifetime_null

(** Performs checks on each cycle that borrowing is always valid *)
let borrow_check_inspect (state : borrow_check_state)
                         (ctx : codegen_context) (proc : proc_def) =
  let chk_cycle = fun (cycle_node : cycle_node) (cycle_state : borrow_check_cycle_state) ->
    (* check each borrowing *)
    let chk_borrow = fun (bi : borrow_info) ->
      (* check that the references borrowed have sufficient lifetime *)
      (* TODO: take into consideration whether this is an in-delay borrow *)
      let chk_source = fun (src : borrow_source) ->
        match src with
        | FromMsgData (msg_spec, n) ->
          let msg_def = lookup_message_def_by_msg ctx proc msg_spec |> Option.get in
          let msg_ty = List.nth msg_def.sig_types n |> sig_type_globalise msg_spec.endpoint in
          if not (Lifetime.lifetime_covered_by bi.lifetime msg_ty.lifetime) then
            let err_msg = Printf.sprintf "Insufficient lifetime of message data: %s::%s, %d (requiring %s but got %s)"
              msg_spec.endpoint msg_spec.msg n (string_of_lifetime bi.lifetime) (string_of_lifetime msg_ty.lifetime) in
            BorrowCheckError err_msg |>
            raise
          else ()
        | FromRef ref_ident ->
          let lt = borrow_check_get_lifetime_tight cycle_state bi.in_delay (ToRef ref_ident) in
          if not (Lifetime.lifetime_covered_by bi.lifetime lt) then
            raise (BorrowCheckError "Insufficient lifetime of reference!")
          else ()
        | FromReg _ -> () (* regs are always good *)
      in
      List.iter chk_source bi.src;

      let chk_mutate = fun (reg_assign : reg_assign) ->
        (* now check all borrows of the register in this cycle *)
        let chk_reg_borrows = fun _ (bci : borrow_check_info) ->
          if List.find_opt (fun (s : borrow_source) -> s = FromReg reg_assign.lval.ident) bci.bi.src |> Option.is_some then
            if Lifetime.lifetime_overlaps_current_mutation bci.relaxed then
              let err_msg = Printf.sprintf "Mutation of borrowed registers detected: %s (borrowed for %s)"
                reg_assign.lval.ident (string_of_lifetime bci.relaxed) in
              raise (BorrowCheckError err_msg)
            else ()
          else ()
        in
        (* TODO: check if it's in delay *)
        BorrowDestinationMap.iter chk_reg_borrows cycle_state.borrows_in_cycle
      in
      List.iter chk_mutate cycle_node.assigns
    in
    List.iter chk_borrow cycle_node.borrows;
  in
  Array.iter2 chk_cycle state.cycle_nodes state.cycle_states
          (* check the current *)
      (* check that the mutated stuff *)

let borrow_check_map_merge (lt_transform_tight : sig_lifetime -> sig_lifetime)
                           (lt_transform_relaxed : sig_lifetime -> sig_lifetime)
                           (old_opt : borrow_check_info option) (new_opt : borrow_check_info option)
                            : borrow_check_info option =
  let open Lifetime in
  let new_opt' = Option.map (fun nw ->
    let new_tight = lt_transform_tight nw.tight
    and new_relaxed = lt_transform_relaxed nw.relaxed in
    {nw with tight = new_tight; relaxed = new_relaxed;})
    new_opt in
  match old_opt, new_opt' with
  | None, _ -> new_opt'
  | Some _, None -> old_opt
  | Some od, _ when od.pinned -> old_opt
  | Some od, Some nw ->
      Some ({
        bi = od.bi;
        tight = lifetime_merge_tight od.tight nw.tight;
        relaxed = lifetime_merge_relaxed od.relaxed nw.relaxed;
        pinned = false;
      })


let borrow_check_merge (state : borrow_check_state)
                       (cycle_idx : int)
                       (m : borrow_map)
                       (d : delay_def) : bool =
  let open Lifetime in
  (* get the event between prev.in_cycle and in_delay *)
  let ev : event =
    match d with
    | `Send _ | `Recv _ -> `IndefCycles
    | _ -> delay_immediate
  in
  let changed = ref false in
  let lt_transform_tight = fun x ->
      Lifetime.consume_lifetime_must_live delay_single_cycle x |>
      Lifetime.consume_lifetime_must_live ev
  and lt_transform_relaxed = fun x ->
      Lifetime.consume_lifetime_might_live delay_single_cycle x  |>
      Lifetime.consume_lifetime_might_live ev
  in
  let map_merge = fun _ (old_opt : borrow_check_info option) (new_opt : borrow_check_info option) ->
    let r = borrow_check_map_merge lt_transform_tight lt_transform_relaxed old_opt new_opt in
    changed := !changed || old_opt <> r;
    r
  in
  let update = fun (old_map : borrow_check_cycle_state) : borrow_check_cycle_state ->
    {old_map with borrows_in_delay = BorrowDestinationMap.merge map_merge old_map.borrows_in_delay m} in
  let old = Array.get state.cycle_states cycle_idx in
  Array.set state.cycle_states cycle_idx (update old);
  !changed

let borrow_check_merge_delay_to_cycle (state : borrow_check_state) (cycle_node : cycle_node) : bool =
  let changed = ref false in
  let lt_transform_tight = Lifetime.consume_lifetime_must_live cycle_node.delay
  and lt_transform_relaxed = Lifetime.consume_lifetime_might_live cycle_node.delay in
  let map_merge = fun _ (old_opt : borrow_check_info option) (new_opt : borrow_check_info option) ->
    let r = borrow_check_map_merge lt_transform_tight lt_transform_relaxed old_opt new_opt in
    changed := !changed || old_opt <> r;
    r
  in
  let cycle_state = state.cycle_states.(cycle_node.id) in
  let new_cycle_state =
    {cycle_state with borrows_in_cycle = BorrowDestinationMap.merge map_merge cycle_state.borrows_in_cycle cycle_state.borrows_in_delay} in
  Array.set state.cycle_states cycle_node.id new_cycle_state;
  !changed

let rec borrow_check_internal (state : borrow_check_state)
                          (ctx : codegen_context)
                          (proc : proc_def) =
  let to_continue = ref false in
  let transit_state = fun cycle_node ->
    let visit_next_cycle = fun (nxt : cycle_id) ->
      let cycle_next : cycle_node = state.cycle_nodes.(nxt) in
      let cycle_state : borrow_check_cycle_state = state.cycle_states.(nxt) in
      to_continue := (borrow_check_merge state nxt cycle_state.borrows_in_cycle cycle_next.delay) || !to_continue;
    in
    let next_default = Option.value ~default:ctx.first_cycle cycle_node.next_default in
    visit_next_cycle next_default;
    List.map (fun ns -> snd ns |> Option.value ~default:ctx.first_cycle) cycle_node.next_switch |>
    List.iter visit_next_cycle;
    to_continue := (borrow_check_merge_delay_to_cycle state cycle_node) || !to_continue
  in
  Array.iter transit_state state.cycle_nodes;
  if !to_continue then borrow_check_internal state ctx proc
  else borrow_check_inspect state ctx proc

(** Perform checks on the borrowing lifetimes and decide whether the operations should be allowed *)
let borrow_check (ctx : codegen_context) (proc : proc_def) =
  ctx.cycles <- List.sort (fun c1 c2 -> c1.id - c2.id) ctx.cycles;
  let make_cycle_state = fun (c : cycle_node) : borrow_check_cycle_state ->
    { borrows_in_delay = List.filter_map (fun (bi : borrow_info) : (borrow_destination * borrow_check_info) option ->
       if bi.in_delay then Some (bi.dst, {bi = bi; relaxed = bi.lifetime; tight = bi.lifetime; pinned = true}) else None) c.borrows |>
      List.to_seq |> BorrowDestinationMap.of_seq;
      borrows_in_cycle = List.filter_map (fun (bi : borrow_info) : (borrow_destination * borrow_check_info) option ->
       if not bi.in_delay then Some (bi.dst, {bi = bi; relaxed = bi.lifetime; tight = bi.lifetime; pinned = true}) else None) c.borrows |>
      List.to_seq |> BorrowDestinationMap.of_seq} in
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
    ({name = "_st"; dtype = `Opaque "_state_t"; init = Some (format_statename ctx.first_cycle)}::proc.regs)
  else proc.regs in
  begin
    codegen_regs_declare ctx regs;
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

let codegen_preamble (_cunit : compilation_unit) = ()

let codegen (cunit : compilation_unit) =
  (* let msgs = List.map (fun (p: proc_def) -> (p.name, p.msgs)) cunit in *)
  codegen_preamble cunit;
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
