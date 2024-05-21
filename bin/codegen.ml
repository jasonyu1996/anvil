open Lang
open Utils

exception UnimplementedError of string
exception BorrowCheckError of string

let message_has_valid_port (msg : message_def) : bool = msg.send_sync = Dynamic
let message_has_ack_port (msg : message_def) : bool = msg.recv_sync = Dynamic

(* borrows are when we need to check the lifetime constraints *)
type borrow_source = string (* only registers *)

module Wire = struct
  type t = {
    name: identifier;
    mutable ty: sig_type;
    mutable borrow_src: borrow_source list; (** Not actually borrowed. Just the dependency. *)
  }

  let merge (u : t) (v : t) : t =
    {u with
      ty = {u.ty with lifetime = Lifetime.lifetime_merge_tight u.ty.lifetime v.ty.lifetime};
      borrow_src = u.borrow_src @ v.borrow_src
    }

  let clone (w : t) : t = {w with ty = w.ty; borrow_src = w.borrow_src}

  let live_now (w : t) : bool = Lifetime.lifetime_covered_by sig_lifetime_this_cycle w.ty.lifetime
  let live_for (w : t) (lt: sig_lifetime) : bool = Lifetime.lifetime_covered_by lt w.ty.lifetime

  (** Is this alive for an indefinite number of cycles? *)
  let live_indefinite (w : t) : bool = Lifetime.lifetime_live_indefinite w.ty.lifetime
end

type wire_def = Wire.t

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

type cycle_node = {
  id: cycle_id;
  delay: delay_def;
  mutable next_switch: (string * cycle_id) list;
}
and superposition = (condition list * cycle_node) list
and reg_assign = {
  superpos: superposition;
  lval: lvalue_evaluated;
  expr_str: string;
}

type expr_result = {
  v: wire_def list;
  superpos: superposition;
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
  mutable assigns: assign list; (* assignments to wires *)
  mutable reg_assigns: reg_assign list;
  mutable first_cycle: int;
  mutable first_cycle_nonempty: bool;
  (* endpoints defined in a process *)
  mutable endpoints: endpoint_def list;
  (* messages that are sent by this process, through either ports or wires *)
  mutable local_messages: (endpoint_def * message_def * message_direction) list;
  cunit: compilation_unit;
  config: Config.compile_config;
}

let lookup_proc (ctx: codegen_context) (name: identifier) : proc_def option =
  List.find_opt (fun (p : proc_def) -> p.name = name) ctx.cunit.procs

let format_wirename (id : wire_id) : string = Printf.sprintf "_wire$%d" id
let format_statename (id : cycle_id) : string = Printf.sprintf "_STATE_%d" id

let format_condition (c : condition) : string =
  let prefix = if c.neg then "!" else "" in prefix ^ c.w

let format_condition_list (conds : condition list) : string =
  List.map format_condition conds |> String.concat " && "

let codegen_context_proc_clear (ctx : codegen_context) =
  ctx.wires <- [];
  ctx.wires_n <- 0;
  ctx.cycles <- [];
  ctx.cycles_n <- 0;
  ctx.assigns <- [];
  ctx.reg_assigns <- [];
  ctx.first_cycle_nonempty <- false

(* TODO: No need to maintain the lifetime here. *)
let codegen_context_new_wire (ctx: codegen_context) (ty : sig_type)
    (borrow_src : borrow_source list): wire_def =
  let open Wire in
  let id = ctx.wires_n in
  let w = {name = format_wirename id; ty = ty; borrow_src = borrow_src} in
  ctx.wires_n <- id + 1;
  ctx.wires <- w::ctx.wires;
  w

let codegen_context_new_cycle (ctx : codegen_context) (delay : delay_def) : cycle_node =
  let new_cycle = {
    id = ctx.cycles_n;
    delay = delay;
    next_switch = [];
  } in
  ctx.cycles_n <- ctx.cycles_n + 1;
  ctx.cycles <- new_cycle::ctx.cycles;
  new_cycle

let codegen_context_new_assign (ctx : codegen_context) (a : assign) =
  ctx.assigns <- a::ctx.assigns

let format_dtype (ctx : codegen_context) (dtype : data_type) =
  match dtype with
  | `Logic -> "logic"
  | `Opaque typename -> typename
  | _ -> (data_type_size ctx.cunit.type_defs dtype) - 1 |> Printf.sprintf "logic[%d:0]"

let format_port (ctx : codegen_context) port =
  let inout = match port.dir with
    | In -> "input"
    | Out -> "output"
  in Printf.sprintf "%s %s %s" inout (format_dtype ctx port.dtype) port.name

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
  Printf.sprintf "_%s_%s_%d" endpoint_name message_name data_idx

let format_msg_valid_signal_name (endpoint_name : identifier) (message_name : identifier) : string =
  Printf.sprintf "_%s_%s_valid" endpoint_name message_name

let format_msg_ack_signal_name (endpoint_name : identifier) (message_name : identifier) : string =
  Printf.sprintf "_%s_%s_ack" endpoint_name message_name

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
      ty = sig_type_globalise endpoint_name ty;
      borrow_src = [];
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
  List.find_opt (fun (x : reg_def) -> x.name = name) proc.body.regs

let format_regname_current (regname : identifier) =
  Printf.sprintf "%s_q" regname
let format_regname_next (regname : identifier) =
  Printf.sprintf "%s_n" regname

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
  List.find_map m proc.body.regs

module BorrowEnv = struct
  exception BorrowEnvException of string

  type borrow_info = {
    reg: identifier;
    duration: sig_lifetime;
  }

  type t = {
    bindings: wire_def string_map;
    borrows: borrow_info list ref;
  }

  let merge (a : t) (b : t) : t =
    let bindings_merge _idx op1 op2 =
      match op1, op2 with
      | Some u, Some v ->
          Some (Wire.merge u v)
      | _ -> raise (BorrowEnvException "Invalid arguments for merging borrowing environment!")
      in
    let new_bindings = StringMap.merge bindings_merge (a.bindings) (b.bindings) in
    let new_borrows = !(a.borrows) @ !(b.borrows) in
    {bindings = new_bindings; borrows = ref new_borrows}

  let assign (env : t) (other : t) : unit =
    let update_binding s (w : wire_def) =
      let o = StringMap.find s other.bindings in
      w.ty <- o.ty
    in
    StringMap.iter update_binding env.bindings;
    env.borrows := !(other.borrows)

  let empty () : t = {bindings = StringMap.empty; borrows = ref []}

  (** Checks if it is okay to mutate the register now.
    If so, adjust the binding lifetimes accordingly.
   *)
  let check_reg_mutate (env : t) (reg_name : identifier) : bool =
    let check_borrow (b : borrow_info) : bool =
      b.reg = reg_name && Lifetime.lifetime_overlaps_current_mutation b.duration
    in
    let borrow_ok = Option.is_none @@ List.find_opt check_borrow !(env.borrows) in
    if borrow_ok then
      let adjust_binding_lifetime _ (w : wire_def) =
        w.ty <- {w.ty with lifetime = sig_lifetime_null}
      in StringMap.iter adjust_binding_lifetime env.bindings
    else ();
    borrow_ok

  (** Transit in-place. *)
  let transit (env : t) (d: Lifetime.event) : unit =
    let adjust_binding_lifetime _ (w : wire_def) =
      w.ty <- {w.ty with lifetime = Lifetime.consume_lifetime_must_live d w.ty.lifetime}
    in
    StringMap.iter adjust_binding_lifetime env.bindings;
    let adjust_borrow_lifetime (b : borrow_info) =
      {b with duration = Lifetime.consume_lifetime_might_live d b.duration}
    in
    env.borrows := List.map adjust_borrow_lifetime !(env.borrows)

  (** Create a clone of the environment. The borrows and bindings are all cloned. *)
  let clone (env : t) : t =
    let new_bindings = StringMap.map (fun x -> Wire.clone x) env.bindings in
    {bindings = new_bindings; borrows = ref !(env.borrows)}

  let add_bindings (env : t) (bindings : wire_def string_map) : t =
    let add _ a b =
      match a, b with
      | Some _, Some _ -> raise (BorrowEnvException "Duplicate bindings are not allowed!")
      | Some _, _ -> a
      | _ -> b
    in {env with bindings = StringMap.merge add env.bindings bindings}

  let add_borrows (env : t) (borrows : borrow_info list) : unit =
    env.borrows := borrows @ !(env.borrows)

  let name_resolve (env : t) (name : identifier) : wire_def option = StringMap.find_opt name env.bindings

  let debug_dump (env : t) (config : Config.compile_config) : unit =
    if config.verbose then begin
      Config.debug_println config "Borrow environment dump: ";
      List.map (fun x -> Printf.sprintf "%s@%s" x.reg (string_of_lifetime x.duration)) !(env.borrows) |> String.concat ", "
        |> Printf.sprintf "- Borrows = %s" |> Config.debug_println config;
      StringMap.to_seq env.bindings |> Seq.map
        (fun ((s, w) : (borrow_source * wire_def)) -> Printf.sprintf "%s@%s" s (string_of_lifetime w.ty.lifetime))
        |> List.of_seq |> String.concat ", " |> Printf.sprintf "- Bindings = %s" |> Config.debug_println config
    end else ()
end

type borrow_env = BorrowEnv.t

(* TODO: here we should use mux instead direct assigns *)
let assign_message_data (ctx : codegen_context) (proc : proc_def) (env : borrow_env)
                        (_in_delay : bool) (msg_spec : message_specifier) (data_ws : wire_def list) =
  gather_data_wires_from_msg ctx proc msg_spec |>
    (* data_wire is not a real wire with lifetimes and borrows, res_wire is the actual data *)
    List.iter2 (fun (res_wire : wire_def) ((data_wire, lt) : (wire_def * sig_lifetime)) ->
        let open BorrowEnv in
        if not @@ Wire.live_now res_wire then
          raise (BorrowCheckError "Attempting to send a dead signal!")
        else ();
        if not @@ Wire.live_for res_wire lt then
          raise (BorrowCheckError "Attempting to send a signal that does not live long enough!")
        else ();
        let borrows = List.map (fun x -> {reg = x; duration = lt}) res_wire.borrow_src in
        add_borrows env borrows;
        codegen_context_new_assign ctx {wire = data_wire.name; expr_str = res_wire.name}) data_ws

let message_ack_wirename (ctx : codegen_context) (proc : proc_def) (msg_spec : message_specifier) : string =
  canonicalise ctx proc format_msg_ack_signal_name msg_spec.endpoint msg_spec.msg

let message_valid_wirename (ctx : codegen_context) (proc : proc_def) (msg_spec : message_specifier) : string =
  canonicalise ctx proc format_msg_valid_signal_name msg_spec.endpoint msg_spec.msg

let format_cstrname (cstr : identifier) : string = Printf.sprintf "_CSTR_%s" cstr

let connect_cycles (superpos : superposition) (cycle_id : cycle_id) : unit =
  let connect_cycle = fun ((conds, prev_cycle) : (condition list * cycle_node)) ->
    let new_switch_entry = (format_condition_list conds, cycle_id) in
    prev_cycle.next_switch <- new_switch_entry::prev_cycle.next_switch in
  List.iter connect_cycle superpos

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

let leaf_expression_result (cur_cycles : superposition) (ws : wire_def list) : expr_result =
  {v = ws; superpos = cur_cycles}

(* This also create connections between cycles *)
(* let generate_expression_result
  (cur_cycles : cycle_node list)
  (expr_results : (condition list * expr_result) list) : expr_result = *)

let superposition_extra_conds (cur_cycles : superposition) (conds : condition list) : superposition =
  List.map (fun (cond, cyc) -> (cond@conds, cyc)) cur_cycles

(* let superposition_merge (superposes : (condition list * superposition) list) : superposition =
  List.concat_map (fun (conds, superpos) -> superposition_extra_conds superpos conds) superposes *)

let superposition_add_reg_assigns (ctx : codegen_context) (assign: reg_assign) =
  ctx.reg_assigns <- assign::ctx.reg_assigns

let expr_debug_dump (e : expr) (config : Config.compile_config) : unit =
  if config.verbose then begin
    (
      match e with
      | Literal _ -> "literal"
      | Identifier ident -> Printf.sprintf "ident(%s)" ident
      | Read reg_ident -> Printf.sprintf "read(%s)" reg_ident
      | Function _ -> "func"
      | TrySend _ -> "trysend"
      | TryRecv _ -> "tryrecv"
      | Apply _ -> "apply"
      | Binop _ -> "binop"
      | Unop _ -> "unop"
      | LetIn (ident, _, _) -> Printf.sprintf "let(%s)" ident
      | Wait (delay, _) -> Printf.sprintf "delay(%s)" (string_of_delay delay)
      | IfExpr _ -> "if"
      | Construct _ -> "cons"
      | Index _ -> "index"
      | Indirect (_, ident) -> Printf.sprintf "indirect(%s)" ident
      | Concat _ -> "concat"
      | Match _ -> "match"
      | Assign _ -> "assign"
      | Tuple _ -> "tuple"
    ) |> Printf.sprintf "Expr: %s" |> Config.debug_println config;
  end

let rec codegen_expr (ctx : codegen_context) (proc : proc_def)
                     (cur_cycles : superposition) (* possible cycles the start of the expression is in *)
                     (env : borrow_env)
                     (in_delay : bool) (e : expr) : expr_result =
  BorrowEnv.debug_dump env ctx.config;
  expr_debug_dump e ctx.config;
  let first_cycle_nonempty = ctx.first_cycle_nonempty in
  ctx.first_cycle_nonempty <- true;
  match e with
  | Literal lit ->
      (* TODO: no-length literal not supported here *)
      let dtype = `Array (`Logic, literal_bit_len lit |> Option.get) in
      let w = codegen_context_new_wire ctx {dtype; lifetime = sig_lifetime_const} [] in (* literal does not depend on anything *)
      codegen_context_new_assign ctx {wire = w.name; expr_str = string_of_literal lit};
      leaf_expression_result cur_cycles [w]
  | Identifier ident ->
      (* only supports registers for now *)
      let w = BorrowEnv.name_resolve env ident |> Option.get in leaf_expression_result cur_cycles [w]
  | Read reg_ident ->
      let dtype = Option.get (get_identifier_dtype ctx proc reg_ident) in
      (* this lifetime is bounded by when the register gets mutated next *)
      let w = codegen_context_new_wire ctx {dtype; lifetime = sig_lifetime_const} [reg_ident]  in
      codegen_context_new_assign ctx {wire = w.name; expr_str = format_regname_current reg_ident};
        leaf_expression_result cur_cycles [w]
  | Function _  -> raise (UnimplementedError "Function expression unimplemented!")
  | TrySend (send_pack, e_succ, e_fail) ->
      let data_res = codegen_expr ctx proc cur_cycles env in_delay send_pack.send_data in
      assign_message_data ctx proc env in_delay send_pack.send_msg_spec data_res.v;
      let has_ack = lookup_message_def_by_msg ctx proc send_pack.send_msg_spec |> Option.get |> message_has_ack_port in
      if has_ack then
        let ack_w = message_ack_wirename ctx proc send_pack.send_msg_spec
        and succ_env = BorrowEnv.clone env
        and fail_env = BorrowEnv.clone env in
        let succ_res = codegen_expr ctx proc (superposition_extra_conds data_res.superpos [{w = ack_w; neg = false}]) succ_env in_delay e_succ
        and fail_res = codegen_expr ctx proc (superposition_extra_conds data_res.superpos [{w = ack_w; neg = true}]) fail_env in_delay e_fail in
        let succ_w = List.hd succ_res.v and fail_w = List.hd fail_res.v in
        let w = codegen_context_new_wire ctx succ_w.ty (succ_w.borrow_src @ fail_w.borrow_src) in
        let expr_str = Printf.sprintf "(%s) ? %s : %s" ack_w succ_w.name fail_w.name in
        codegen_context_new_assign ctx {wire = w.name; expr_str = expr_str};
        BorrowEnv.merge succ_env fail_env |> BorrowEnv.assign env;
        {v = [w]; superpos = succ_res.superpos @ fail_res.superpos}
      else
        (* TODO: static checks *)
        codegen_expr ctx proc cur_cycles env in_delay e_succ
  | TryRecv (recv_pack, e_succ, e_fail) ->
      let has_valid = lookup_message_def_by_msg ctx proc recv_pack.recv_msg_spec |> Option.get |> message_has_valid_port in
      let succ_bindings = gather_data_wires_from_msg ctx proc recv_pack.recv_msg_spec |>
        List.map fst |>
        List.combine recv_pack.recv_binds |> map_of_list
      in
      if has_valid then
        let valid_w = message_valid_wirename ctx proc recv_pack.recv_msg_spec
        and succ_env = BorrowEnv.add_bindings (BorrowEnv.clone env) succ_bindings
        and fail_env = BorrowEnv.clone env in
        let succ_res = codegen_expr ctx proc (superposition_extra_conds cur_cycles [{w = valid_w; neg = false}]) succ_env in_delay e_succ
        and fail_res = codegen_expr ctx proc (superposition_extra_conds cur_cycles [{w = valid_w; neg = true}]) fail_env in_delay e_fail in
        let succ_w = List.hd succ_res.v and fail_w = List.hd fail_res.v in
        let w = codegen_context_new_wire ctx succ_w.ty (succ_w.borrow_src @ fail_w.borrow_src) in
        let expr_str = Printf.sprintf "(%s) ? %s : %s" valid_w succ_w.name fail_w.name in
        codegen_context_new_assign ctx {wire = w.name; expr_str = expr_str};
        BorrowEnv.merge succ_env fail_env |> BorrowEnv.assign env;
        {v = [w]; superpos = succ_res.superpos @ fail_res.superpos}
      else
        (* TODO: static checks *)
        let succ_env = BorrowEnv.add_bindings env succ_bindings in
        codegen_expr ctx proc cur_cycles succ_env in_delay e_succ
  | Apply _ -> raise (UnimplementedError "Application expression unimplemented!")
  | Binop (binop, e1, e2) ->
      let e1_res = codegen_expr ctx proc cur_cycles env in_delay e1 in
      let e2_res = codegen_expr ctx proc e1_res.superpos env in_delay e2 in
      begin
        match e1_res.v, e2_res.v with
        | [w1], [w2] ->
            (* if w1.ty.dtype = w2.ty.dtype then *)
            (* TODO: compute the new lifetime; type checking *)
            let w = codegen_context_new_wire ctx w1.ty (w1.borrow_src @ w2.borrow_src) in
            let expr_str = Printf.sprintf "%s %s %s"
              w1.name (string_of_binop binop) w2.name in
            codegen_context_new_assign ctx {wire = w.name; expr_str = expr_str};
            {v = [w]; superpos = e2_res.superpos}
        | _ -> raise (TypeError "Invalid types for binary operation!")
      end
  | Unop (unop, e') ->
    let e_res = codegen_expr ctx proc cur_cycles env in_delay e' in
    let w = List.hd e_res.v in
    let new_dtype =
      match unop with
      | Neg | Not -> w.ty.dtype
      | AndAll | OrAll -> `Logic
    in
    let w' = codegen_context_new_wire ctx {w.ty with dtype = new_dtype} w.borrow_src in
    let expr_str = Printf.sprintf "%s%s" (string_of_unop unop) w.name in
    codegen_context_new_assign ctx {wire = w'.name; expr_str = expr_str};
    {v = [w']; superpos = e_res.superpos}
  | Tuple _elist -> raise (UnimplementedError "Tuple expression unimplemented!")
  | LetIn (ident, v_expr, body_expr) ->
      let v_res = codegen_expr ctx proc cur_cycles env in_delay v_expr in
      let new_bindings =
        if ident = "_" then env.bindings
        else
          let w = List.hd v_res.v in
          StringMap.add ident w env.bindings
      in
      let expr_res = codegen_expr ctx proc v_res.superpos {env with bindings = new_bindings} in_delay body_expr in
      {
        v = expr_res.v;
        superpos = expr_res.superpos
      }
  | IfExpr (cond_expr, e1, e2) ->
      let cond_res = codegen_expr ctx proc cur_cycles env in_delay cond_expr in
      let cond_w = List.hd cond_res.v in
      if not @@ Wire.live_now cond_w then
        raise (BorrowCheckError "Attempting to use a dead signal in a if-condition!")
      else ();
      let e1_res = codegen_expr ctx proc (superposition_extra_conds cond_res.superpos [{w = cond_w.name; neg = false}]) env in_delay e1 in
      let e2_res = codegen_expr ctx proc (superposition_extra_conds cond_res.superpos [{w = cond_w.name; neg = true}]) env in_delay e2 in
      (* get the results *)
      let gen_result = fun (w1 : wire_def) (w2 : wire_def) ->
        (* compute new lifetime *)
        let new_w = codegen_context_new_wire ctx w1.ty (cond_w.borrow_src @ w1.borrow_src @ w2.borrow_src) in
        let expr_str = Printf.sprintf "(%s) ? %s : %s" cond_w.name w1.name w2.name in
        let _ = codegen_context_new_assign ctx {wire = new_w.name; expr_str = expr_str} in
        new_w
      in
      let v = List.map2 gen_result e1_res.v e2_res.v in
      {v = v; superpos = e1_res.superpos @ e2_res.superpos}
  | Assign (lval, e_val) ->
      let lval_eval = evaluate_lvalue ctx proc lval |> Option.get in
      let expr_res = codegen_expr ctx proc cur_cycles env in_delay e_val in
      if not @@ Wire.live_now (List.hd expr_res.v) then
        raise (BorrowCheckError "Attempting to use a dead signal in an assignment!")
      else ();
      if not @@ BorrowEnv.check_reg_mutate env lval_eval.ident then
        raise (BorrowCheckError "Attempting to mutate a borrowed register!")
      else ();
      let codegen_assign = fun (w : wire_def) ->
        (* assign borrows for one cycle *)
        {
          superpos = expr_res.superpos;
          lval = lval_eval;
          expr_str = w.name
        } |> superposition_add_reg_assigns ctx;
      in
      List.iter codegen_assign expr_res.v;
      {v = []; superpos = expr_res.superpos}
      (* assign evaluates to unit val *)
  | Construct (_cstr_ident, _cstr_args) ->
      raise (UnimplementedError "Construct expression unimplemented!")
  | Index (e', ind) ->
      let expr_res = codegen_expr ctx proc cur_cycles env in_delay e' in
      let w_res = List.hd expr_res.v in
      let (offset_le, offset_ri, new_dtype) = data_type_index ctx.cunit.type_defs w_res.ty.dtype ind |> Option.get in
      let new_w = codegen_context_new_wire ctx {w_res.ty with dtype = new_dtype} w_res.borrow_src in
      let expr_str = Printf.sprintf "%s[%d:%d]" w_res.name offset_le (offset_ri - 1) in
      codegen_context_new_assign ctx {wire = new_w.name; expr_str = expr_str};
      (* TODO: non-literal index *)
      {v = [new_w]; superpos = expr_res.superpos}
  | Indirect (e', fieldname) ->
      let expr_res = codegen_expr ctx proc cur_cycles env in_delay e' in
      let w_res = List.hd expr_res.v in
      let (offset_le, offset_ri, new_dtype) = data_type_indirect ctx.cunit.type_defs w_res.ty.dtype fieldname |> Option.get in
      let new_w = codegen_context_new_wire ctx {w_res.ty with dtype = new_dtype} w_res.borrow_src in
      let expr_str = Printf.sprintf "%s[%d:%d]" w_res.name offset_le (offset_ri - 1) in
      codegen_context_new_assign ctx {wire = new_w.name; expr_str = expr_str};
      {v = [new_w]; superpos = expr_res.superpos}
  | Concat components ->
      let cur_superpos = ref cur_cycles in
      let comp_res = List.map (
        fun x ->
        let v = codegen_expr ctx proc !cur_superpos env in_delay x in
        cur_superpos := v.superpos;
        v) components in
      let wires = List.map (fun x -> List.hd x.v) comp_res in
      let borrow_src = let open Wire in List.concat_map (fun x -> x.borrow_src) wires in
      let size = List.fold_left (fun s (x : wire_def) -> s + (data_type_size ctx.cunit.type_defs x.ty.dtype)) 0 wires in
      (* TODO: lifetime not correct *)
      let new_w = codegen_context_new_wire ctx {(List.hd wires).ty with dtype =`Array (`Logic, size)} borrow_src in
      let expr_str = List.map (fun (x: wire_def) -> x.name) wires |> String.concat ", " |>  Printf.sprintf "{%s}" in
      codegen_context_new_assign ctx {wire = new_w.name; expr_str = expr_str};
      {v = [new_w]; superpos = !cur_superpos}
  | Match (e', match_arm_list) ->
      let expr_res = codegen_expr ctx proc cur_cycles env in_delay e' in
      let w = List.hd expr_res.v in
      if not @@ Wire.live_now w then
        raise (BorrowCheckError "Attempting to use a dead signal in a match expression!")
      else ();
      let dtype_resolved = data_type_name_resolve ctx.cunit.type_defs w.ty.dtype in
      begin
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
                let new_bindings =
                  match range_opt, pattern.bind_name with
                  | None, None -> StringMap.empty
                  | Some range, Some bind_name ->
                      let arm_vw = codegen_context_new_wire ctx {w.ty with dtype} w.borrow_src in
                      let arm_v_expr_str = Printf.sprintf "%s[%d:%d]" w.name range.ri range.le in
                      codegen_context_new_assign ctx {wire = arm_vw.name; expr_str = arm_v_expr_str};
                      StringMap.add bind_name arm_vw StringMap.empty
                  | _ -> raise (TypeError "Pattern matching incompatible with type definition!")
                in
                let arm_env = BorrowEnv.add_bindings (BorrowEnv.clone env) new_bindings in
                let arm_res = codegen_expr ctx proc
                  (superposition_extra_conds expr_res.superpos [{w = cond_str; neg = false}]) arm_env in_delay e_arm in
                arm_conds := [];
                Some (new_cond, arm_res, arm_env)
              in
              let arm_res_list = List.filter_map process_arm match_arm_list in
              let new_env = ref (BorrowEnv.empty ())
              and first_arm = ref true in
              let arm_expr_str = List.map (fun ((c, r, e) : (identifier * expr_result * borrow_env)) ->
                if !first_arm then begin
                  new_env := e;
                  first_arm := false
                end else new_env := BorrowEnv.merge !new_env e;
                Printf.sprintf "(%s) ? %s" c (List.hd r.v).name) arm_res_list |> String.concat " : " |> Printf.sprintf "%s : '0" in
              let new_dtype = (List.hd arm_res_list |> (fun (_, x, _) -> x) |>  (fun (x : expr_result) -> x.v) |> List.hd).ty in
              let new_w = codegen_context_new_wire ctx new_dtype (List.concat_map
                (fun ((_, x, _) : (identifier * expr_result * borrow_env)) -> (List.hd x.v).borrow_src) arm_res_list) in
              codegen_context_new_assign ctx {wire = new_w.name; expr_str = arm_expr_str};
              BorrowEnv.assign env !new_env;
              {v = [new_w];
              superpos = List.concat_map (fun ((_, x, _) : (identifier * expr_result * borrow_env)) -> x.superpos) arm_res_list}
            | _ ->  raise (TypeError "Illegal match: value is not of a variant type!")
          end
        | None -> raise (TypeError "Illegal match: value is not of a variant type!")
        end
  | Wait (delay, body) ->
      BorrowEnv.transit env (`Cycles 1);
      let init_bindings = match delay with
      | `Send {send_msg_spec = msg_specifier; send_data = expr} ->
          (* gather the data to send *)
          let expr_res = codegen_expr ctx proc [] env true expr in
          BorrowEnv.transit env (delay :> Lifetime.event);
          assign_message_data ctx proc env true msg_specifier expr_res.v;
          StringMap.empty
      | `Recv {recv_binds = idents; recv_msg_spec = msg_specifier} ->
          BorrowEnv.transit env (delay :> Lifetime.event);
          let env = gather_data_wires_from_msg ctx proc msg_specifier |>
            List.map fst |>
            List.combine idents |> map_of_list in
          env
      | _ -> StringMap.empty (* TODO: support cycle delays *)
      in
      let superpos = if first_cycle_nonempty then
        let new_cycle = codegen_context_new_cycle ctx delay in
        (* connect cycles *)
        connect_cycles cur_cycles new_cycle.id;
        [([], new_cycle)]
      else
        cur_cycles
      in
      codegen_expr ctx proc superpos (BorrowEnv.add_bindings env init_bindings) false body

let codegen_post_declare (ctx : codegen_context) (_proc : proc_def)=
  (* wire declarations *)
  let codegen_wire = fun (w: wire_def) ->
    Printf.printf "  %s %s;\n" (format_dtype ctx w.ty.dtype) w.name
  in List.iter codegen_wire ctx.wires;
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
      in List.iter assign_reg_default proc.body.regs;

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
        let assign_reg = fun (assign : reg_assign) ->
          let to_assign = ref false in
          let conds = List.filter_map
            (fun (c, cy) -> if cy.id = cycle.id then begin
              to_assign := true;
              if c = [] then
                None
              else
                Some (format_condition_list c |> Printf.sprintf "(%s)")
            end else None) assign.superpos |>
            String.concat " || " in
          if !to_assign then
            let cond_str = if conds = "" then "" else
              Printf.sprintf "if (%s) " conds
            in
            Printf.printf "        %s%s = %s;\n" cond_str (format_lval_next assign.lval) assign.expr_str
          else ()
        in
        begin
          List.iter assign_reg ctx.reg_assigns;

          if state_cnt > 1 then
          begin
            (* state transition *)
            print_string transition_begin;
            (* default next state *)
            let cond_next_state = fun (w_cond, c) ->
              if w_cond = "" then
                Printf.printf "          _st_n = %s;\n" @@ format_statename c
              else
                Printf.printf "          if (%s) _st_n = %s;\n" w_cond @@ format_statename c
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
  in List.iteri gen_spawn proc.body.spawns



let codegen_proc ctx (proc : proc_def) =
  (* generate ports *)
  Printf.printf "module %s (\n" proc.name;
  codegen_ports ctx proc.args;
  print_endline ");";

  (* implicit state *)
  (* let first_cycle_op = codegen_proc_body_list ctx proc proc.body None in *)
  (* Option.iter (fun c -> ctx.first_cycle <- c) first_cycle_op; *)
  let init_cycle = codegen_context_new_cycle ctx delay_immediate in
  ctx.first_cycle <- init_cycle.id;
  let body_res = codegen_expr ctx proc [([], init_cycle)] (BorrowEnv.empty ()) false proc.body.prog in
  connect_cycles body_res.superpos init_cycle.id;
  let regs = if List.length ctx.cycles > 1 then
    ({name = "_st"; dtype = `Opaque "_state_t"; init = Some (format_statename ctx.first_cycle)}::proc.body.regs)
  else proc.body.regs in
  begin
    codegen_regs_declare ctx regs;
    codegen_state_transition regs
  end;
  codegen_channels ctx proc.body.channels;
  codegen_endpoints ctx;
  ctx.local_messages <- gather_local_messages ctx proc;
  codegen_spawns ctx proc;
  codegen_state_machine ctx proc;
  codegen_post_declare ctx proc;

  print_endline "endmodule"

let rec codegen_with_context (ctx : codegen_context) (procs: proc_def list) =
  match procs with
  | [] -> ()
  | proc :: procs' ->
      codegen_context_proc_clear ctx;
      codegen_proc ctx proc;
      codegen_with_context ctx procs'

let codegen_preamble (_cunit : compilation_unit) = ()

let codegen (cunit : compilation_unit) (config : Config.compile_config)=
  (* let msgs = List.map (fun (p: proc_def) -> (p.name, p.msgs)) cunit in *)
  codegen_preamble cunit;
  let ctx : codegen_context = {
    wires = [];
    wires_n = 0;
    cycles = [];
    cycles_n = 0;
    first_cycle = 0;
    first_cycle_nonempty = false;
    assigns = [];
    reg_assigns = [];
    cunit = cunit;
    endpoints = [];
    local_messages = [];
    config;
  } in
  codegen_with_context ctx cunit.procs
