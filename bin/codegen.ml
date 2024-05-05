open Lang

type port_def = {
  dir: message_direction;
  dtype: data_type;
  name: identifier;
}

type wire_id = int
type cycle_id = int

type wire_def = {
  id: wire_id;
  dtype: data_type;
}

type cycle_node = {
  id: cycle_id;
  out_wires: wire_id list; (* the output of the cycle *)
  next_switch: (wire_id * cycle_id) list;
  next_default: cycle_id;
}

type codegen_context = {
  mutable wires: wire_def list;
  mutable wires_n: int;
  mutable cycles: cycle_node list;
  mutable cycles_n: int;
  msgs: (identifier * (message_def list)) list;
}

type expr_result = (wire_id * data_type) list

let codegen_context_proc_clear (ctx : codegen_context) =
  ctx.wires <- [];
  ctx.wires_n <- 0;
  ctx.cycles <- [];
  ctx.cycles_n <- 0

let codegen_context_new_wire (ctx: codegen_context) (dtype : data_type) : wire_id =
  let id = ctx.wires_n in
  ctx.wires_n <- id + 1;
  ctx.wires <- {id = id; dtype = dtype}::ctx.wires;
  id

let codegen_context_new_cycle (ctx : codegen_context) (c : cycle_node) : cycle_id =
  let id = ctx.cycles_n in
  let c' = {c with id = id} in
  ctx.cycles_n <- id + 1;
  ctx.cycles <- c'::ctx.cycles;
  id

let rec format_dtype_split dtype =
  match dtype with
  | Logic -> ("logic", "")
  | Array (dtype', n) ->
      let (base, arrs) = format_dtype_split dtype' in
      (base, "[" ^ (string_of_int n) ^ "]" ^ arrs)

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
      Printf.printf "      %s <= '0;\n" (format_regname_current r.name)
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

let format_wirename (id : wire_id) : string = "wire" ^ string_of_int(id)

let print_assign (wire_id : wire_id) (expr_str : string) =
  Printf.printf "  assign %s = %s;\n" (format_wirename wire_id) expr_str

let string_of_literal (lit : literal) : string =
  let len = List.length lit in
  let bit_str = String.concat "" (List.map string_of_bit lit) in
  (string_of_int len) ^ "'b" ^ bit_str

let get_identifier_dtype (_ctx : codegen_context) (proc : proc_def) (ident : identifier) : data_type option =
  let m = fun (r: reg_def) -> if r.name = ident then Some r.dtype else None in
  List.find_map m proc.regs


let rec codegen_expr (ctx : codegen_context) (proc : proc_def) (e : expr) : expr_result =
  match e with
  | Literal lit ->
      let dtype = Array (Logic, List.length lit) in
      let id = codegen_context_new_wire ctx dtype in
      print_assign id (string_of_literal lit);
      [(id, dtype)]
  | Identifier ident ->
      (* only supports registers for now *)
      let dtype = Option.get (get_identifier_dtype ctx proc ident) in
      let id = codegen_context_new_wire ctx dtype in
      print_assign id (format_regname_current ident);
      [(id, dtype)]
  | Function _  -> []
  | Send _ -> []
  | Recv _ -> []
  | Apply _ -> []
  | Binop (binop, e1, e2) ->
      let e1_res = codegen_expr ctx proc e1 in
      let e2_res = codegen_expr ctx proc e2 in
      (match e1_res, e2_res with
      | [(id1, dtype1)], [(id2, dtype2)] ->
          if dtype1 = dtype2 then
            let id = codegen_context_new_wire ctx dtype1 in
            let expr_str = Printf.sprintf "%s %s %s"
              (format_wirename id1) (string_of_binop binop) (format_wirename id2) in
            print_assign id expr_str;
            [(id, dtype1)]
          else []
      | _ -> [])
  | Unop _ -> []
  | Tuple elist -> List.concat (List.map (codegen_expr ctx proc) elist)
  | LetIn _ -> []
  | IfExpr _ -> []

let codegen_cycle (ctx : codegen_context) (proc : proc_def) (e : expr) =
  let _id = ctx.cycles_n in (* cycle id *)
  let _ = codegen_expr ctx proc e in ()
  (* TODO *)

let rec codegen_proc_body (ctx : codegen_context) (proc : proc_def) (proc_body : proc_body) =
  (* TODO *)
  match proc_body with
  | EmptyProcBody -> ()
  | Seq (e, next) -> codegen_cycle ctx proc e; codegen_proc_body ctx proc next
  | _ -> ()

let codegen_post_declare (ctx : codegen_context) =
  let codegen_wire = fun (w: wire_def) ->
    Printf.printf "  %s %s;\n" (format_dtype w.dtype) (format_wirename w.id)
  in
    List.iter codegen_wire ctx.wires

let codegen_proc ctx (proc : proc_def) =
  (* generate ports *)
  Printf.printf "module %s (\n" proc.name;
  codegen_ports proc.msgs;
  print_endline ");";

  codegen_regs_declare proc.regs;
  codegen_state_transition proc.regs;
  codegen_proc_body ctx proc proc.body;
  codegen_post_declare ctx;

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
    msgs = msgs;
  } in
  codegen_with_context ctx cunit
