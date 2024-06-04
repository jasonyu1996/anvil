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

module ControlFlowGraph = struct
  type node = {
    delay: delay_def;
    mutable successors: node list; (* successor nodes *)
    mutable mutated: identifier list; (* registers mutated *)
  }

  let add_mutations (n : node) (mutations : identifier list) : unit =
    n.mutated <- mutations @ n.mutated

  let add_successors (n : node) (successors : node list) : unit =
    n.successors <- successors @ n.successors

  let new_node (delay : delay_def) : node =
    {
      delay; successors = []; mutated = []
    }

  let join_nodes (ns : node list) : node =
    let n = List.hd ns in
    if List.exists (fun x -> x != n) ns then
      let n = new_node delay_immediate in
      List.iter (fun x -> add_successors x [n]) ns;
      n
    else n
end

type cycle_node = {
  id: cycle_id;
  mutable delay: delay_def;
  mutable sends : (condition list * message_specifier * int) list;
  mutable next_switch: (string * cycle_id) list;
  mutable debug_statements: string list;
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
  out_cf_node: ControlFlowGraph.node; (* the out node of the control flow graph *)
}

type assign = {
  wire : string;
  expr_str : string;
}


module BorrowEnv = struct
  exception BorrowEnvException of string

  type borrow_info = {
    reg: identifier;
    duration: sig_lifetime;
  }

  type t = {
    bindings: (wire_def * int) string_map; (* wire and version number *)
    retired_bindings: wire_def string_map; (* name$version -> wire *)
    borrows: borrow_info list ref;
  }

  type binding_version_map = int string_map

  let merge (a : t) (b : t) : t =
    let bindings_merge _idx op1 op2 =
      ( match op1, op2 with
        | Some (u, u_ver), Some (v, v_ver) ->
            assert (u_ver = v_ver);
            Some (Wire.merge u v, u_ver)
        | _ -> raise (BorrowEnvException "Invalid arguments for merging borrowing environment!")
      )
    and retired_merge _idx op1 op2 =
      (
        match op1, op2 with
        | Some u, Some v ->
            Some (Wire.merge u v)
        | _ -> raise (BorrowEnvException "Invalid arguments for merging borrowing environment!")
      )
    in
    let new_bindings = StringMap.merge bindings_merge a.bindings b.bindings
    and new_retired = StringMap.merge retired_merge a.retired_bindings b.retired_bindings in
    let new_borrows = !(a.borrows) @ !(b.borrows) in
    {bindings = new_bindings; retired_bindings = new_retired; borrows = ref new_borrows}

  let assign (env : t) (other : t) : unit =
    let update_binding s ((w, v) : (wire_def * int)) =
      let (ow, ov) = StringMap.find s other.bindings in
      assert (v = ov);
      w.ty <- ow.ty
    in
    StringMap.iter update_binding env.bindings;
    let update_retired s (w : wire_def) =
      let ow = StringMap.find s other.retired_bindings in
      w.ty <- ow.ty
    in
    StringMap.iter update_retired env.retired_bindings;
    env.borrows := !(other.borrows)

  let empty () : t = {bindings = StringMap.empty;
    retired_bindings = StringMap.empty; borrows = ref []}

  (** Checks if it is okay to mutate the register now.
    If so, adjust the binding lifetimes accordingly.
   *)
  let check_reg_mutate (env : t) (reg_name : identifier) : bool =
    let check_borrow (b : borrow_info) : bool =
      b.reg = reg_name && Lifetime.lifetime_overlaps_current_mutation b.duration
    in
    let borrow_ok = not @@ List.exists check_borrow !(env.borrows) in
    if borrow_ok then
      let adjust_binding_lifetime _ ((w, _) : wire_def * int) =
        (* now the register value can only be used in this cycle *)
        w.ty <- {w.ty with lifetime = sig_lifetime_this_cycle}
      in StringMap.iter adjust_binding_lifetime env.bindings;
      let adjust_retired_binding_lifetime s w = adjust_binding_lifetime s (w, 0) in
      StringMap.iter adjust_retired_binding_lifetime env.retired_bindings
    else ();
    borrow_ok

  (** Transit in-place. *)
  let transit (env : t) (d: Lifetime.event) : unit =
    let adjust_binding_lifetime _ ((w, _) : wire_def * int) =
      w.ty <- {w.ty with lifetime = Lifetime.consume_lifetime_must_live d w.ty.lifetime}
    in
    let adjust_retired_binding_lifetime a b = adjust_binding_lifetime a (b, 0) in
    StringMap.iter adjust_binding_lifetime env.bindings;
    StringMap.iter adjust_retired_binding_lifetime env.retired_bindings;
    let adjust_borrow_lifetime (b : borrow_info) =
      {b with duration = Lifetime.consume_lifetime_might_live d b.duration}
    in
    env.borrows := List.map adjust_borrow_lifetime !(env.borrows)

  (** Create a clone of the environment. The borrows and bindings are all cloned. *)
  let clone (env : t) : t =
    let new_bindings = StringMap.map (fun (x, v) -> (Wire.clone x, v)) env.bindings
    and new_retired = StringMap.map (fun x -> Wire.clone x) env.retired_bindings in
    {bindings = new_bindings; retired_bindings = new_retired; borrows = ref !(env.borrows)}

  let add_bindings (ver_map: binding_version_map ref) (env : t) (bindings : wire_def string_map) : t =
    let new_retired = ref env.retired_bindings in
    let add s a b =
      match b with
      | Some w ->
          (* retire the existing binding if any *)
          (
            match a with
            | Some (ow, ov) ->
                let retired_name = Printf.sprintf "%s$%d" s ov in
                new_retired := StringMap.add retired_name ow !new_retired
            | None -> ()
          );
          let new_v = (StringMap.find_opt s !ver_map |> Option.value ~default:0) + 1 in
          ver_map := StringMap.add s new_v !ver_map;
          Some (w, new_v)
      | _ -> a
    in
    let new_bindings = StringMap.merge add env.bindings bindings in
    {env with bindings = new_bindings; retired_bindings = !new_retired}

  let add_borrows (env : t) (borrows : borrow_info list) : unit =
    env.borrows := borrows @ !(env.borrows)

  let name_resolve (env : t) (name : identifier) : wire_def option = StringMap.find_opt name env.bindings |> Option.map fst

  let debug_dump (env : t) (config : Config.compile_config) : unit =
    if config.verbose then begin
      Config.debug_println config "Borrow environment dump: ";
      List.map (fun x -> Printf.sprintf "%s@%s" x.reg (string_of_lifetime x.duration)) !(env.borrows) |> String.concat ", "
        |> Printf.sprintf "- Borrows = %s" |> Config.debug_println config;
      StringMap.to_seq env.bindings |> Seq.map
        (fun ((s, (w, v)) : (borrow_source * (wire_def * int))) -> Printf.sprintf "%s$%d@%s" s v (string_of_lifetime w.ty.lifetime))
        |> List.of_seq |> String.concat ", " |> Printf.sprintf "- Bindings = %s" |> Config.debug_println config;
      StringMap.to_seq env.retired_bindings |> Seq.map
        (fun ((s, w) : (borrow_source * wire_def)) -> Printf.sprintf "%s@%s" s (string_of_lifetime w.ty.lifetime))
        |> List.of_seq |> String.concat ", " |> Printf.sprintf "- Retired = %s" |> Config.debug_println config
    end else ()
end

type borrow_env = BorrowEnv.t


let rec type_is_integral (type_defs : type_def_map) (dtype : data_type) : bool =
  let dtype_resolved = data_type_name_resolve type_defs dtype |> Option.get in
  match dtype_resolved with
  | `Logic -> true
  | `Array (dtype', _) -> type_is_integral type_defs dtype'
  | _ -> false

let type_check_binop (type_defs : type_def_map) binop dtype1 dtype2 =
  let dtype1_resolved = data_type_name_resolve type_defs dtype1 |> Option.get
  and dtype2_resolved = data_type_name_resolve type_defs dtype2 |> Option.get in
  (* only integral types can be used here *)
  if (not @@ type_is_integral type_defs dtype1_resolved)
    || (not @@ type_is_integral type_defs dtype2_resolved) then
    None
  else
  match binop with
  | Add | Sub | Xor | And | Or ->
    (* TODO: performance improvement *)
    if dtype1_resolved = dtype2_resolved then
      Some dtype1_resolved
    else None
  | Lt | Gt | Lte | Gte | Eq | Neq -> Some `Logic
  | Shl | Shr -> Some dtype1_resolved

type send_info = {
  msg_spec: message_specifier;
  mutable select: identifier list list;
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
  mutable in_cf_node: ControlFlowGraph.node option;
  mutable out_cf_node: ControlFlowGraph.node option;
  mutable out_borrows: BorrowEnv.borrow_info list;
  mutable sends: send_info string_map;
  mutable mux_state_regs : string_set;
  binding_versions: BorrowEnv.binding_version_map ref;
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
  ctx.sends <- StringMap.empty;
  ctx.mux_state_regs <- StringSet.empty;
  ctx.reg_assigns <- [];
  ctx.first_cycle_nonempty <- false

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
    debug_statements = [];
    sends = [];
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

let format_msg_prefix (endpoint_name : identifier) (message_name : identifier) : identifier =
  Printf.sprintf "_%s_%s" endpoint_name message_name

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

let codegen_state_transition (ctx : codegen_context) (regs : reg_def list) =
  print_endline "  always_ff @(posedge clk_i or negedge rst_ni) begin: state_transition";
  print_endline "    if (~rst_ni) begin";
  let codegen_reg_reset = fun (r: reg_def) ->
    let init_val_str = Option.value ~default:"'0" r.init in
    Printf.printf "      %s <= %s;\n" (format_regname_current r.name) init_val_str
  in List.iter codegen_reg_reset regs;
  StringSet.iter (Printf.printf "      %s_mux_q <= '0;\n") ctx.mux_state_regs;
  print_endline "    end else begin";

  (* print out debug statements *)
  if ctx.cycles_n > 1 then
    Printf.printf "      unique case (_st_q)\n"
  else ();
  let print_cycle_debug_statements (cycle : cycle_node) =
    if ctx.cycles_n > 1 then
      Printf.printf "        %s: begin\n" @@ format_statename cycle.id
    else ();
    List.iter (Printf.printf "          %s\n") cycle.debug_statements;
    if ctx.cycles_n > 1 then
      Printf.printf "        end\n"
    else ()
  in
  List.iter print_cycle_debug_statements ctx.cycles;
  if ctx.cycles_n > 1 then
    Printf.printf "      endcase\n"
  else ();

  let codegen_reg_next = fun (r: reg_def) ->
    Printf.printf "      %s <= %s;\n"
      (format_regname_current r.name) (format_regname_next r.name)
  in List.iter codegen_reg_next regs;
  StringSet.iter (fun x -> Printf.printf "      %s_mux_q <= %s_mux_n;\n" x x) ctx.mux_state_regs;
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
  | Binary (len, b) -> Printf.sprintf "%d'b%s" len (List.map string_of_digit b |> List.rev |> String.concat "")
  | Decimal (len, d) -> Printf.sprintf "%d'd%s" len (List.map string_of_digit d |> List.rev |> String.concat "")
  | Hexadecimal (len, h) -> Printf.sprintf "%d'h%s" len (List.map string_of_digit h |> List.rev |> String.concat "")
  | NoLength n -> string_of_int n

let get_identifier_dtype (_ctx : codegen_context) (proc : proc_def) (ident : identifier) : data_type option =
  let m = fun (r: reg_def) -> if r.name = ident then Some r.dtype else None in
  List.find_map m proc.body.regs
(* TODO: here we should use mux instead direct assigns *)

let assign_message_data (ctx : codegen_context) (proc : proc_def) (cur_cycles : superposition) (env : borrow_env)
                        (msg_spec : message_specifier) (data_ws : wire_def list) =

  (* type checks and borrow checks *)
  gather_data_wires_from_msg ctx proc msg_spec |>
    (* data_wire is not a real wire with lifetimes and borrows, res_wire is the actual data *)
    List.iter2 (fun (res_wire : wire_def) ((_, lt) : (wire_def * sig_lifetime)) ->
        let open BorrowEnv in
        if not @@ Wire.live_now res_wire then
          raise (BorrowCheckError "Attempting to send a dead signal!")
        else ();
        if not @@ Wire.live_for res_wire lt then
          raise (BorrowCheckError "Attempting to send a signal that does not live long enough!")
        else ();
        let borrows = List.map (fun x -> {reg = x; duration = lt}) res_wire.borrow_src in
        add_borrows env borrows) data_ws;
        (* codegen_context_new_assign ctx {wire = data_wire.name; expr_str = res_wire.name}) data_ws *)

  let msg_str = format_msg_prefix msg_spec.endpoint msg_spec.msg in
  let send_info = StringMap.find_opt msg_str ctx.sends |> Option.value ~default:{msg_spec; select = []} in
  let send_id = List.length send_info.select in
  send_info.select <- (List.map (fun (w : wire_def) -> w.name) data_ws)::send_info.select;
  let add_send_to_cycle ((conds, cycle) : condition list * cycle_node) =
    cycle.sends <- (conds, msg_spec, send_id)::cycle.sends
  in
  List.iter add_send_to_cycle cur_cycles;
  ctx.sends <- StringMap.add msg_str send_info ctx.sends

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

let cycle_add_debug_statement (superpos : superposition) (unconditioned : string) : unit =
  let add_debug_statement ((conds, cycle) : (condition list * cycle_node)) =
    let cond_str = (format_condition_list conds) in
    let if_cond_str = if cond_str <> "" then Printf.sprintf "if (%s) " cond_str else "" in
    let full_statement = Printf.sprintf "%s%s" if_cond_str unconditioned in
    cycle.debug_statements <- full_statement::cycle.debug_statements
  in
  List.iter add_debug_statement superpos


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

let leaf_expression_result (cur_cycles : superposition) (in_cf_node : ControlFlowGraph.node)
                           (ws : wire_def list) : expr_result =
  {v = ws; superpos = cur_cycles; out_cf_node = in_cf_node}

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
      | LetIn (idents, _, _) -> Printf.sprintf "let(%s)" @@ String.concat ", " idents
      | Wait (delay, _) -> Printf.sprintf "delay(%s)" (string_of_delay delay)
      | IfExpr _ -> "if"
      | Construct _ -> "cons"
      | Record _ -> "record"
      | Index _ -> "index"
      | Indirect (_, ident) -> Printf.sprintf "indirect(%s)" ident
      | Concat _ -> "concat"
      | Match _ -> "match"
      | Assign _ -> "assign"
      | Tuple _ -> "tuple"
      | Debug _ -> "debug"
    ) |> Printf.sprintf "Expr: %s" |> Config.debug_println config;
  end

let rec codegen_expr_list (ctx : codegen_context) (proc : proc_def)
                     (cur_cycles : superposition) (* possible cycles the start of the expression is in *)
                     (in_cf_node : ControlFlowGraph.node)
                     (env : borrow_env)
                     (elist : expr list)
                     : expr_result list * superposition * ControlFlowGraph.node  =
  let cur_superpos = ref cur_cycles
  and cur_in_cf_node = ref in_cf_node in
  let comp_res = List.map (
    fun x ->
    let v = codegen_expr ctx proc !cur_superpos !cur_in_cf_node env x in
    cur_superpos := v.superpos;
    cur_in_cf_node := v.out_cf_node;
    v) elist in
  (comp_res, !cur_superpos, !cur_in_cf_node)
and codegen_expr (ctx : codegen_context) (proc : proc_def)
                     (cur_cycles : superposition) (* possible cycles the start of the expression is in *)
                     (in_cf_node : ControlFlowGraph.node)
                     (env : borrow_env)
                     (e : expr) : expr_result =
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
      leaf_expression_result cur_cycles in_cf_node [w]
  | Identifier ident ->
      (* only supports registers for now *)
      let w =
        try
          BorrowEnv.name_resolve env ident |> Option.get
        with Invalid_argument _ -> raise (TypeError (Printf.sprintf "Identifier %s is not bound!" ident))
      in leaf_expression_result cur_cycles in_cf_node [w]
  | Read reg_ident ->
      let dtype = Option.get (get_identifier_dtype ctx proc reg_ident) in
      (* this lifetime is bounded by when the register gets mutated next *)
      let w = codegen_context_new_wire ctx {dtype; lifetime = sig_lifetime_const} [reg_ident]  in
      codegen_context_new_assign ctx {wire = w.name; expr_str = format_regname_current reg_ident};
        leaf_expression_result cur_cycles in_cf_node [w]
  | Function _  -> raise (UnimplementedError "Function expression unimplemented!")
  | TrySend (send_pack, e_succ, e_fail) ->
      let data_res = codegen_expr ctx proc cur_cycles in_cf_node env send_pack.send_data in
      assign_message_data ctx proc cur_cycles env send_pack.send_msg_spec data_res.v;
      let in_cf_node' = data_res.out_cf_node in
      let has_ack = lookup_message_def_by_msg ctx proc send_pack.send_msg_spec |> Option.get |> message_has_ack_port in
      if has_ack then
        let ack_w = message_ack_wirename ctx proc send_pack.send_msg_spec
        and succ_env = BorrowEnv.clone env
        and fail_env = BorrowEnv.clone env
        and succ_in_cf_node = ControlFlowGraph.new_node (`Send send_pack)
        and fail_in_cf_node = ControlFlowGraph.new_node delay_immediate in
        ControlFlowGraph.add_successors in_cf_node' [succ_in_cf_node; fail_in_cf_node];
        let succ_res = codegen_expr ctx proc (superposition_extra_conds data_res.superpos [{w = ack_w; neg = false}]) succ_in_cf_node succ_env e_succ
        and fail_res = codegen_expr ctx proc (superposition_extra_conds data_res.superpos [{w = ack_w; neg = true}]) fail_in_cf_node fail_env e_fail in
        let succ_w = List.hd succ_res.v and fail_w = List.hd fail_res.v in
        let w = codegen_context_new_wire ctx succ_w.ty (succ_w.borrow_src @ fail_w.borrow_src) in
        let expr_str = Printf.sprintf "(%s) ? %s : %s" ack_w succ_w.name fail_w.name in
        codegen_context_new_assign ctx {wire = w.name; expr_str = expr_str};
        BorrowEnv.merge succ_env fail_env |> BorrowEnv.assign env;
        {v = [w]; superpos = succ_res.superpos @ fail_res.superpos;
          out_cf_node = ControlFlowGraph.join_nodes [succ_res.out_cf_node; fail_res.out_cf_node]}
      else
        (* TODO: static checks *)
        let succ_in_cf_node = ControlFlowGraph.new_node (`Send send_pack) in
        ControlFlowGraph.add_successors in_cf_node [succ_in_cf_node];
        codegen_expr ctx proc cur_cycles succ_in_cf_node env e_succ
  | TryRecv (recv_pack, e_succ, e_fail) ->
      let has_valid = lookup_message_def_by_msg ctx proc recv_pack.recv_msg_spec |> Option.get |> message_has_valid_port in
      let succ_bindings = gather_data_wires_from_msg ctx proc recv_pack.recv_msg_spec |>
        List.map fst |>
        List.combine recv_pack.recv_binds |> map_of_list
      in
      if has_valid then
        let valid_w = message_valid_wirename ctx proc recv_pack.recv_msg_spec
        and succ_env = BorrowEnv.add_bindings ctx.binding_versions (BorrowEnv.clone env) succ_bindings
        and fail_env = BorrowEnv.clone env
        and succ_in_cf_node = ControlFlowGraph.new_node (`Recv recv_pack)
        and fail_in_cf_node = ControlFlowGraph.new_node delay_immediate in
        let succ_res = codegen_expr ctx proc (superposition_extra_conds cur_cycles [{w = valid_w; neg = false}]) succ_in_cf_node succ_env e_succ
        and fail_res = codegen_expr ctx proc (superposition_extra_conds cur_cycles [{w = valid_w; neg = true}]) fail_in_cf_node fail_env e_fail in
        let succ_w = List.hd succ_res.v and fail_w = List.hd fail_res.v in
        let w = codegen_context_new_wire ctx succ_w.ty (succ_w.borrow_src @ fail_w.borrow_src) in
        let expr_str = Printf.sprintf "(%s) ? %s : %s" valid_w succ_w.name fail_w.name in
        codegen_context_new_assign ctx {wire = w.name; expr_str = expr_str};
        BorrowEnv.merge succ_env fail_env |> BorrowEnv.assign env;
        {v = [w]; superpos = succ_res.superpos @ fail_res.superpos;
          out_cf_node = ControlFlowGraph.join_nodes [succ_res.out_cf_node; fail_res.out_cf_node]}
      else
        (* TODO: static checks *)
        let succ_env = BorrowEnv.add_bindings ctx.binding_versions env succ_bindings
        and succ_in_cf_node = ControlFlowGraph.new_node (`Recv recv_pack) in
        ControlFlowGraph.add_successors in_cf_node [succ_in_cf_node];
        codegen_expr ctx proc cur_cycles succ_in_cf_node succ_env e_succ
  | Apply _ -> raise (UnimplementedError "Application expression unimplemented!")
  | Binop (binop, e1, e2) ->
      let e1_res = codegen_expr ctx proc cur_cycles in_cf_node env e1 in
      let in_cf_node' = e1_res.out_cf_node in
      let e2_res = codegen_expr ctx proc e1_res.superpos in_cf_node' env e2 in
      begin
        match e1_res.v, e2_res.v with
        | [w1], [w2] ->
            (* if w1.ty.dtype = w2.ty.dtype then *)
            let new_dtype =
              match type_check_binop ctx.cunit.type_defs binop w1.ty.dtype w2.ty.dtype with
              | Some new_dtype -> new_dtype
              | _ -> raise (TypeError "Binary operator type checking failed!")
            and
            lt = Lifetime.lifetime_merge_tight w1.ty.lifetime w2.ty.lifetime in
            let w = codegen_context_new_wire ctx {dtype = new_dtype; lifetime = lt} (w1.borrow_src @ w2.borrow_src) in
            let expr_str = Printf.sprintf "%s %s %s"
              w1.name (string_of_binop binop) w2.name in
            codegen_context_new_assign ctx {wire = w.name; expr_str = expr_str};
            {e2_res with v = [w]}
        | _ -> raise (TypeError "Invalid types for binary operation!")
      end
  | Unop (unop, e') ->
      let e_res = codegen_expr ctx proc cur_cycles in_cf_node env e' in
      let w = List.hd e_res.v in
      let new_dtype =
        match unop with
        | Neg | Not -> w.ty.dtype
        | AndAll | OrAll -> `Logic
      in
      let w' = codegen_context_new_wire ctx {w.ty with dtype = new_dtype} w.borrow_src in
      let expr_str = Printf.sprintf "%s%s" (string_of_unop unop) w.name in
      codegen_context_new_assign ctx {wire = w'.name; expr_str = expr_str};
      {e_res with v = [w']}
  | Tuple expr_list ->
      let (w_res, superpos, out_cf_node) = codegen_expr_list ctx proc cur_cycles in_cf_node env expr_list in
      {v = List.map (fun x -> List.hd x.v) w_res; superpos; out_cf_node}
  | LetIn (idents, v_expr, body_expr) ->
      let v_res = codegen_expr ctx proc cur_cycles in_cf_node env v_expr in
      let in_cf_node' = v_res.out_cf_node in
      let new_bindings = ref StringMap.empty in
      begin
        try
          List.iter2 (fun i w -> new_bindings := StringMap.add i w !new_bindings) idents v_res.v
        with
        | Invalid_argument _ -> raise (TypeError "Invalid let binding expression!")
      end;
      let new_env = BorrowEnv.add_bindings ctx.binding_versions env !new_bindings in
      codegen_expr ctx proc v_res.superpos in_cf_node' new_env body_expr
  | IfExpr (cond_expr, e1, e2) ->
      let cond_res = codegen_expr ctx proc cur_cycles in_cf_node env cond_expr in
      let cond_w = List.hd cond_res.v
      and in_cf_node' = cond_res.out_cf_node in
      if not @@ Wire.live_now cond_w then
        raise (BorrowCheckError "Attempting to use a dead signal in a if-condition!")
      else ();
      let e1_in_cf_node = ControlFlowGraph.new_node delay_immediate
      and e2_in_cf_node = ControlFlowGraph.new_node delay_immediate in
      ControlFlowGraph.add_successors in_cf_node' [e1_in_cf_node; e2_in_cf_node];
      let e1_res = codegen_expr ctx proc
        (superposition_extra_conds cond_res.superpos [{w = cond_w.name; neg = false}])
        e1_in_cf_node env e1 in
      let e2_res = codegen_expr ctx proc
        (superposition_extra_conds cond_res.superpos [{w = cond_w.name; neg = true}])
        e2_in_cf_node env e2 in
      (* get the results *)
      let gen_result = fun (w1 : wire_def) (w2 : wire_def) ->
        (* compute new lifetime *)
        let new_w = codegen_context_new_wire ctx w1.ty (cond_w.borrow_src @ w1.borrow_src @ w2.borrow_src) in
        let expr_str = Printf.sprintf "(%s) ? %s : %s" cond_w.name w1.name w2.name in
        let _ = codegen_context_new_assign ctx {wire = new_w.name; expr_str = expr_str} in
        new_w
      in
      let v = List.map2 gen_result e1_res.v e2_res.v in
      {v = v; superpos = e1_res.superpos @ e2_res.superpos;
        out_cf_node = ControlFlowGraph.join_nodes [e1_res.out_cf_node; e2_res.out_cf_node]}
  | Assign (lval, e_val) ->
      let lval_eval = evaluate_lvalue ctx proc lval |> Option.get in
      let expr_res = codegen_expr ctx proc cur_cycles in_cf_node env e_val in
      let in_cf_node' = expr_res.out_cf_node in
      if not @@ Wire.live_now (List.hd expr_res.v) then
        raise (BorrowCheckError "Attempting to use a dead signal in an assignment!")
      else ();
      if not @@ BorrowEnv.check_reg_mutate env lval_eval.ident then
        raise (BorrowCheckError "Attempting to mutate a borrowed register!")
      else ();
      ControlFlowGraph.add_mutations in_cf_node' [lval_eval.ident];
      let codegen_assign = fun (w : wire_def) ->
        (* assign borrows for one cycle *)
        {
          superpos = expr_res.superpos;
          lval = lval_eval;
          expr_str = w.name
        } |> superposition_add_reg_assigns ctx;
      in
      List.iter codegen_assign expr_res.v;
      {v = []; superpos = expr_res.superpos; out_cf_node = in_cf_node'}
      (* assign evaluates to unit val *)
  | Construct (cstr_spec, cstr_expr_opt) ->
      let dtype_opt = data_type_name_resolve ctx.cunit.type_defs @@ `Named cstr_spec.variant_ty_name in
      (
        match dtype_opt with
        | Some(`Variant _ as dtype) ->
            let e_dtype_opt = variant_lookup_dtype dtype cstr_spec.variant in
            (
              match e_dtype_opt, cstr_expr_opt with
              | Some _e_dtype, Some cstr_expr ->
                  let res = codegen_expr ctx proc cur_cycles in_cf_node env cstr_expr in
                  let v = List.hd res.v in
                  (* TODO: type check *)
                  let w = codegen_context_new_wire ctx {dtype; lifetime = v.ty.lifetime} v.borrow_src in
                  let tag_size = variant_tag_size dtype
                  and var_idx = variant_lookup_index dtype cstr_spec.variant |> Option.get
                  and data_range = variant_lookup_range dtype cstr_spec.variant ctx.cunit.type_defs |> Option.get
                  and tot_size = data_type_size ctx.cunit.type_defs dtype in
                  let expr_str_tag = Printf.sprintf "%d'd%d" tag_size var_idx
                  and expr_str_tag_wire = Printf.sprintf "%s[%d:0]" w.name (tag_size - 1)
                  and expr_str_data = v.name
                  and expr_str_data_wire = Printf.sprintf "%s[%d:%d]" w.name data_range.ri data_range.le in
                  codegen_context_new_assign ctx {wire = expr_str_tag_wire; expr_str = expr_str_tag};
                  codegen_context_new_assign ctx {wire = expr_str_data_wire; expr_str = expr_str_data};
                  if tot_size <> data_range.ri + 1 then
                    let expr_str_unused = Printf.sprintf "'0"
                    and expr_str_unused_wire = Printf.sprintf "%s[%d:%d]" w.name (tot_size - 1) (data_range.ri + 1) in
                    codegen_context_new_assign ctx {wire = expr_str_unused_wire; expr_str = expr_str_unused}
                  else ();
                  {res with v = [w]}
              | None, None ->
                  let w = codegen_context_new_wire ctx {dtype; lifetime = sig_lifetime_const} []
                  and tag_size = variant_tag_size dtype
                  and var_idx = variant_lookup_index dtype cstr_spec.variant |> Option.get
                  and tot_size = data_type_size ctx.cunit.type_defs dtype in
                  let expr_str_tag = Printf.sprintf "%d'd%d" tag_size var_idx
                  and expr_str_tag_wire = Printf.sprintf "%s[%d:0]" w.name (tag_size - 1) in
                  codegen_context_new_assign ctx {wire = expr_str_tag_wire; expr_str = expr_str_tag};
                  if tot_size <> tag_size then
                    let expr_str_unused = Printf.sprintf "'0"
                    and expr_str_unused_wire = Printf.sprintf "%s[%d:%d]" w.name (tot_size - 1) tag_size in
                    codegen_context_new_assign ctx {wire = expr_str_unused_wire; expr_str = expr_str_unused}
                  else ();
                  {v = [w]; superpos = cur_cycles; out_cf_node = in_cf_node}
              | _ -> raise (TypeError "Invalid constructor argument!")
          )
        | _ -> raise (TypeError "Invalid constructor!")
      )
  | Record (record_ty_name, field_exprs) ->
      let dtype = data_type_name_resolve ctx.cunit.type_defs @@ `Named record_ty_name |> Option.get in
      (
        match dtype with
        | `Record record_fields ->
            let expr_reordered_opt = Utils.list_match_reorder (List.map fst record_fields) field_exprs in
            (
              match expr_reordered_opt with
              | Some expr_reordered ->
                  let (res, superpos, out_cf_node) = codegen_expr_list ctx proc cur_cycles
                                        in_cf_node env expr_reordered in
                  let ws = List.map (fun {v; _} -> List.hd v) res in
                  let lifetime = List.map (fun ({ty; _} : wire_def) -> ty.lifetime) ws |>
                                List.fold_left Lifetime.lifetime_merge_tight sig_lifetime_const
                  and borrows = List.concat_map (fun ({borrow_src; _} : wire_def) -> borrow_src) ws in
                  let cur_offset = ref 0
                  and w = codegen_context_new_wire ctx {dtype; lifetime} borrows in
                  let assign_field (_, field_dtype) (value_w : wire_def) =
                    let dtype_size = data_type_size ctx.cunit.type_defs field_dtype in
                    codegen_context_new_assign ctx
                      {
                        wire = Printf.sprintf "%s[%d:%d]" w.name (!cur_offset + dtype_size - 1) !cur_offset;
                        expr_str = Printf.sprintf "%s" value_w.name ;
                      };
                    cur_offset := !cur_offset + dtype_size;
                  in
                  List.iter2 assign_field record_fields ws;
                  { v = [w]; superpos; out_cf_node }
              | _ -> raise (TypeError "Invalid record-type value!")
            )

        | _ -> raise (TypeError "Invalid record type name!")
      )
  | Index (e', ind) ->
      let expr_res = codegen_expr ctx proc cur_cycles in_cf_node env e' in
      let w_res = List.hd expr_res.v in
      let (offset_le, offset_ri, new_dtype) = data_type_index ctx.cunit.type_defs w_res.ty.dtype ind |> Option.get in
      let new_w = codegen_context_new_wire ctx {w_res.ty with dtype = new_dtype} w_res.borrow_src in
      let expr_str = Printf.sprintf "%s[%d:%d]" w_res.name offset_le (offset_ri - 1) in
      codegen_context_new_assign ctx {wire = new_w.name; expr_str = expr_str};
      (* TODO: non-literal index *)
      {expr_res with v = [new_w]}
  | Indirect (e', fieldname) ->
      let expr_res = codegen_expr ctx proc cur_cycles in_cf_node env e' in
      let w_res = List.hd expr_res.v in
      let (offset_le, offset_ri, new_dtype) = data_type_indirect ctx.cunit.type_defs w_res.ty.dtype fieldname |> Option.get in
      let new_w = codegen_context_new_wire ctx {w_res.ty with dtype = new_dtype} w_res.borrow_src in
      let expr_str = Printf.sprintf "%s[%d:%d]" w_res.name offset_le (offset_ri - 1) in
      codegen_context_new_assign ctx {wire = new_w.name; expr_str = expr_str};
      {expr_res with v = [new_w]}
  | Concat components ->
      let (comp_res, cur_superpos, cur_in_cf_node) =
        codegen_expr_list ctx proc cur_cycles in_cf_node env components in
      let wires = List.map (fun x -> List.hd x.v) comp_res in
      let borrow_src = let open Wire in List.concat_map (fun x -> x.borrow_src) wires in
      let (size, lt) = List.fold_left
        (fun (s, l) (x : wire_def) -> (s + (data_type_size ctx.cunit.type_defs x.ty.dtype), Lifetime.lifetime_merge_tight l x.ty.lifetime))
        (0, sig_lifetime_const) wires in
      let new_w = codegen_context_new_wire ctx {dtype =`Array (`Logic, size); lifetime = lt} borrow_src in
      let expr_str = List.map (fun (x: wire_def) -> x.name) wires |> String.concat ", " |>  Printf.sprintf "{%s}" in
      codegen_context_new_assign ctx {wire = new_w.name; expr_str = expr_str};
      {v = [new_w]; superpos = cur_superpos; out_cf_node = cur_in_cf_node}
  | Match (e', match_arm_list) ->
      let expr_res = codegen_expr ctx proc cur_cycles in_cf_node env e' in
      let in_cf_node' = expr_res.out_cf_node in
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
                      let arm_vw = codegen_context_new_wire ctx
                        {w.ty with dtype = `Array (`Logic, range.ri - range.le + 1)}
                        w.borrow_src in
                      let arm_v_expr_str = Printf.sprintf "%s[%d:%d]" w.name range.ri range.le in
                      codegen_context_new_assign ctx {wire = arm_vw.name; expr_str = arm_v_expr_str};
                      StringMap.add bind_name arm_vw StringMap.empty
                  | _ -> raise (TypeError "Pattern matching incompatible with type definition!")
                in
                let arm_env_pre = BorrowEnv.clone env in
                let arm_env = BorrowEnv.add_bindings ctx.binding_versions arm_env_pre new_bindings
                and arm_in_cf_node = ControlFlowGraph.new_node delay_immediate in
                ControlFlowGraph.add_successors in_cf_node' [arm_in_cf_node];
                let arm_res = codegen_expr ctx proc
                  (superposition_extra_conds expr_res.superpos [{w = cond_str; neg = false}]) arm_in_cf_node arm_env e_arm in
                arm_conds := [];
                Some (new_cond, arm_res, arm_env_pre)
              in
              let arm_res_list = List.filter_map process_arm match_arm_list in
              let new_env = ref (BorrowEnv.empty ())
              and first_arm = ref true in
              List.iter (fun ((_, _, e) : (identifier * expr_result * borrow_env)) ->
                    if !first_arm then begin
                      new_env := e;
                      first_arm := false
                    end else new_env := BorrowEnv.merge !new_env e) arm_res_list;
              let v = List.hd arm_res_list |> (fun (_, x, _) -> x) |>  (fun (x : expr_result) -> x.v) |>
                function
                | [] -> []
                | w::_ ->
                  let lt = ref sig_lifetime_const in
                  let arm_expr_str = List.map (fun ((c, r, _) : (identifier * expr_result * borrow_env)) ->
                    let cw = List.hd r.v in
                    lt := Lifetime.lifetime_merge_tight !lt cw.ty.lifetime;
                    Printf.sprintf "(%s) ? %s" c (List.hd r.v).name) arm_res_list |> String.concat " : " |> Printf.sprintf "%s : '0" in
                  let new_w = codegen_context_new_wire ctx {w.ty with lifetime = !lt} (List.concat_map
                    (fun ((_, x, _) : (identifier * expr_result * borrow_env)) -> (List.hd x.v).borrow_src) arm_res_list) in
                  codegen_context_new_assign ctx {wire = new_w.name; expr_str = arm_expr_str};
                  [new_w]
              in
              BorrowEnv.assign env !new_env;
              {v; superpos = List.concat_map (fun ((_, x, _) : (identifier * expr_result * borrow_env)) -> x.superpos) arm_res_list;
              out_cf_node = List.map (fun ((_, x, _) : (identifier * expr_result * borrow_env)) -> x.out_cf_node) arm_res_list
                |> ControlFlowGraph.join_nodes}
            | _ ->  raise (TypeError "Illegal match: value is not of a variant type!")
          end
        | None -> raise (TypeError "Illegal match: value is not of a variant type!")
        end
  | Wait (delay, body) ->
      let (init_bindings, in_cf_node_wait, first_delay, msg_data_assign) = match delay with
      | `Send {send_msg_spec = msg_specifier; send_data = expr} ->
          BorrowEnv.transit env (`Cycles 1);
          let in_cf_node_wait = ControlFlowGraph.new_node (`Cycles 1) in
          ControlFlowGraph.add_successors in_cf_node [in_cf_node_wait];
          (* gather the data to send *)
          (* HACK: consider not allowing timing operations in arguments? *)
          let expr_res = codegen_expr ctx proc [] in_cf_node env expr in
          let in_cf_node_wait = expr_res.out_cf_node in
          BorrowEnv.transit env (delay :> Lifetime.event);
          (StringMap.empty, in_cf_node_wait, delay, Some (msg_specifier, expr_res.v))
      | `Recv {recv_binds = idents; recv_msg_spec = msg_specifier} ->
          BorrowEnv.transit env (`Cycles 1);
          let in_cf_node_wait = ControlFlowGraph.new_node (`Cycles 1) in
          ControlFlowGraph.add_successors in_cf_node [in_cf_node_wait];
          BorrowEnv.transit env (delay :> Lifetime.event);
          let env = gather_data_wires_from_msg ctx proc msg_specifier |>
            List.map fst |>
            List.combine idents |> map_of_list in
          (env, in_cf_node_wait, delay, None)
      | `Cycles 0 -> raise (TypeError "Invalid delay: #0 (n must be > 0 in #n)!")
      | _ ->
          BorrowEnv.transit env (delay :> Lifetime.event);
          (StringMap.empty, in_cf_node, `Cycles 1, None)
      in
      let in_cf_node_body = ControlFlowGraph.new_node delay in
      ControlFlowGraph.add_successors in_cf_node_wait [in_cf_node_body];
      let superpos = if first_cycle_nonempty then
        let new_cycle = codegen_context_new_cycle ctx first_delay in
        (* connect cycles *)
        connect_cycles cur_cycles new_cycle.id;
        [([], new_cycle)]
      else begin
        List.iter (fun (_, c) -> c.delay <- first_delay) cur_cycles;
        cur_cycles
      end in
      let superpos' =
        match delay with
        | `Cycles n ->
          let cur_cycles = ref superpos in
          (* n - 1 times *)
          for _ = 2 to n do
            let new_cycle = codegen_context_new_cycle ctx @@ `Cycles 1 in
            connect_cycles !cur_cycles new_cycle.id;
            cur_cycles := [([], new_cycle)]
          done;
          !cur_cycles
        | _ -> superpos
      in
      begin
        match msg_data_assign with
        | Some (message_specifier, wires) ->
            assign_message_data ctx proc superpos' env message_specifier wires
        | None -> ()
      end;
      codegen_expr ctx proc superpos' in_cf_node_body
        (BorrowEnv.add_bindings ctx.binding_versions env init_bindings) body
  | Debug debug_op ->
      (
        match debug_op with
        | DebugPrint (fmt, vlist) ->
            let (comp_res, cur_superpos, cur_in_cf_node) =
              codegen_expr_list ctx proc cur_cycles in_cf_node env vlist in
            let wires = List.map (fun x -> List.hd x.v) comp_res in
            if List.exists (fun x -> not @@ Wire.live_now x) wires then
              raise (BorrowCheckError "Attempting to print dead signal!")
            else ();
            let args = List.map (fun (x: wire_def) -> x.name) wires |> String.concat ", " in
            let statement = if args <> "" then
              Printf.sprintf "$display(\"%s\", %s);" fmt args
            else
              Printf.sprintf "$display(\"%s\");" fmt
            in
            cycle_add_debug_statement cur_superpos statement;
            {v = []; superpos = cur_superpos; out_cf_node = cur_in_cf_node}
        | DebugFinish ->
            cycle_add_debug_statement cur_cycles "$finish;";
            {v = []; superpos = cur_cycles; out_cf_node = in_cf_node}
      )

let codegen_post_declare (ctx : codegen_context) (proc : proc_def) =
  (* wire declarations *)
  let codegen_wire = fun (w: wire_def) ->
    Printf.printf "  %s %s;\n" (format_dtype ctx w.ty.dtype) w.name
  in List.iter codegen_wire ctx.wires;
  (* wire assignments *)
  List.iter print_assign ctx.assigns;
  (* set send signals *)
  StringMap.iter (fun _ {msg_spec; select} ->
    let data_wires = gather_data_wires_from_msg ctx proc msg_spec
    and msg_prefix = format_msg_prefix msg_spec.endpoint msg_spec.msg in
    match select with
    | [ws] ->
        (* just assign without mux *)
        List.iter2 (fun (res_wire : identifier) ((data_wire, _) : (wire_def * _)) ->
          print_assign {wire = data_wire.name; expr_str = res_wire}) ws data_wires
    | _ ->
        (* need to use mux *)
        let ws_bufs = List.map (fun ((w, _) : (wire_def * _)) -> Printf.sprintf "  assign %s =" w.name |>
          String.to_seq |> Buffer.of_seq) data_wires
        and cur_idx = ref @@ List.length select in
        List.iter (fun ws ->
          cur_idx := !cur_idx - 1;
          List.iter2 (fun buf w -> Buffer.add_string buf @@ Printf.sprintf " (%s_mux_n == %d) ? %s :"
              msg_prefix !cur_idx w) ws_bufs ws) select;
        List.iter (fun (buf : Buffer.t) -> Printf.printf "%s '0;\n" @@ Buffer.contents buf) ws_bufs
  ) ctx.sends

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
  let state_width = Utils.int_log2 state_cnt |> max 1 in
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

      (* generate the send mux states *)
      ctx.mux_state_regs <- StringSet.empty;
      let print_mux_state s si =
        let si_count = List.length si.select in
        if si_count > 1 then
          begin
            Printf.printf "  logic[%d:0] %s_mux_q, %s_mux_n;\n" ((Utils.int_log2 si_count) - 1) s s;
            ctx.mux_state_regs <- StringSet.add s ctx.mux_state_regs
          end
        else ()
      in
      StringMap.iter print_mux_state ctx.sends;

      (* send mux. We have to generate this in a separate always_comb to make Verilator happy *)
      (* https://verilator.org/guide/latest/warnings.html#cmdoption-arg-UNOPTFLAT *)
      Printf.printf "  always_comb begin : send_mux\n";
      (* default mux states *)
      StringSet.iter (fun s -> Printf.printf "    %s_mux_n = %s_mux_q;\n" s s ) ctx.mux_state_regs;
      if state_cnt > 1 then
        Printf.printf "    unique case (_st_q)\n"
      else ();
      let codegen_cycle cycle =
        (* print out the mux state change *)
        if state_cnt > 1 then
          Printf.printf "      %s: begin\n" (format_statename cycle.id)
        else ();
        let print_mux_state_change (conds, msg_spec, send_id) =
          let s = format_msg_prefix msg_spec.endpoint msg_spec.msg in
          if StringSet.find_opt s ctx.mux_state_regs |> Option.is_none then ()
          else begin
            if conds = [] then
              Printf.printf "        %s_mux_n = %d;\n" s send_id
            else
              Printf.printf "        if (%s) %s_mux_n = %d;\n"
                (format_condition_list conds) s send_id
          end
        in
        List.iter print_mux_state_change cycle.sends;
        if state_cnt > 1 then
          Printf.printf "      end\n"
        else ();
      in
      List.iter codegen_cycle ctx.cycles;
      if state_cnt > 1 then
        Printf.printf "    endcase\n"
      else ();
      Printf.printf "  end\n";

      (* state machine *)
      Printf.printf "  always_comb begin : state_machine\n";

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
  codegen_channels ctx proc.body.channels;
  ctx.local_messages <- gather_local_messages ctx proc;

  (* implicit state *)
  (* let first_cycle_op = codegen_proc_body_list ctx proc proc.body None in *)
  (* Option.iter (fun c -> ctx.first_cycle <- c) first_cycle_op; *)
  let init_cycle = codegen_context_new_cycle ctx delay_immediate in
  ctx.first_cycle <- init_cycle.id;
  let init_cf_node = ControlFlowGraph.new_node delay_immediate
  and init_env = BorrowEnv.empty () in
  let body_res = codegen_expr ctx proc [([], init_cycle)]
    init_cf_node init_env proc.body.prog in
  if body_res.v <> [] then
    raise (TypeError "Process body does not evaluate to ()!")
  else ();
  connect_cycles body_res.superpos init_cycle.id;
  ctx.in_cf_node <- Some init_cf_node;
  ctx.out_cf_node <- Some body_res.out_cf_node;
  ctx.out_borrows <- !(init_env.borrows);

  (* output generated code *)

  (* generate ports *)
  Printf.printf "module %s (\n" proc.name;
  codegen_ports ctx proc.args;
  print_endline ");";

  codegen_endpoints ctx;
  codegen_spawns ctx proc;
  codegen_state_machine ctx proc;
  let regs = if List.length ctx.cycles > 1 then
    ({name = "_st"; dtype = `Opaque "_state_t"; init = Some (format_statename ctx.first_cycle)}::proc.body.regs)
  else proc.body.regs in
  begin
    codegen_regs_declare ctx regs;
    codegen_state_transition ctx regs
  end;
  codegen_post_declare ctx proc;

  print_endline "endmodule"

let borrow_check_wrap_around (ctx : codegen_context) (proc : proc_def) =
  (* List.iter (fun (b : BorrowEnv.borrow_info) ->
    Printf.eprintf "Borrow %s = %s\n" b.reg @@ string_of_lifetime b.duration) ctx.out_borrows; *)
  (
    match proc.body.prog with
    | Wait _ -> ()
    | _ -> raise (TypeError "The program body must start with `cycle` or `wait` operations!")
  );
  let rec borrow_check_end (n : ControlFlowGraph.node) (borrows : BorrowEnv.borrow_info list) =
    let check_mut_bad (mut_reg : borrow_source) : bool =
      List.exists
        (fun (x : BorrowEnv.borrow_info) -> x.reg = mut_reg && Lifetime.lifetime_overlaps_current_mutation x.duration)
        borrows
    in
    if List.exists check_mut_bad n.mutated then
      raise (BorrowCheckError "Mutated register is still borrowed (wrapped around)!")
    else ();
    let check_next (next : ControlFlowGraph.node) =
      let new_borrows = List.map
        (fun (x : BorrowEnv.borrow_info) -> {x with duration = Lifetime.consume_lifetime_might_live next.delay x.duration})
        borrows in
      borrow_check_end next new_borrows
    in
    List.iter check_next n.successors;
  in
  borrow_check_end (Option.get ctx.in_cf_node) ctx.out_borrows

let rec codegen_with_context (ctx : codegen_context) (procs: proc_def list) =
  match procs with
  | [] -> ()
  | proc :: procs' ->
      codegen_context_proc_clear ctx;
      codegen_proc ctx proc;
      borrow_check_wrap_around ctx proc;
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
    in_cf_node = None;
    out_cf_node = None;
    out_borrows = [];
    sends = StringMap.empty;
    mux_state_regs = StringSet.empty;
    local_messages = [];
    binding_versions = ref StringMap.empty; (* TODO: check if this is correct *)
    config;
  } in
  codegen_with_context ctx cunit.procs
