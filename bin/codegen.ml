open Lang

type port_def = {
  dir: message_direction;
  dtype: data_type;
  name: identifier;
}

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

type wire_def = reg_def

let codegen_proc_body _ctx (_proc_body : proc_body) = ()

let codegen_proc ctx (proc : proc_def) =
  (* generate ports *)
  Printf.printf "module %s (\n" proc.name;
  codegen_ports proc.msgs;
  print_endline ");";

  codegen_regs_declare proc.regs;
  codegen_state_transition proc.regs;
  codegen_proc_body ctx proc.body;

  print_endline "endmodule"

let rec codegen_with_context ctx (cunit : compilation_unit) =
  match cunit with
  | [] -> ()
  | proc :: cunit' ->
      codegen_proc ctx proc;
      codegen_with_context ctx cunit'

let codegen (cunit : compilation_unit) =
  let msgs = List.map (fun (p: proc_def) -> (p.name, p.msgs)) cunit in
  codegen_with_context msgs cunit
