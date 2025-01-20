(** This module includes definitions relevant to the language constructs, including
those that are part of the AST created during parsing. *)

(** A span of source code. *)
type code_span = {
  st : Lexing.position; (** start of the span *)
  ed : Lexing.position; (** end of the span *)
}

(** A dummy code span that does not represent any valid span. *)
let code_span_dummy = { st = Lexing.dummy_pos; ed = Lexing.dummy_pos }

type 'a maybe_param = 'a ParamEnv.maybe_param

type identifier = string

(** A node in AST. Containing the data plus the code span info. *)
type 'a ast_node = {
  span : code_span;
  d : 'a;
}

(** Construct an AST node with specified data and span. *)
let ast_node_of_data st ed d = { span = {st; ed}; d }
let tag_with_span s d = { span = s; d }
let dummy_ast_node_of_data d = { span = code_span_dummy; d }

(** Type of a parameter. *)
type param_type =
  | IntParam (** an integer constant *)
  | TypeParam (** a type *)

(** A compile-time parameter. *)
type param = {
  param_name : identifier;
  param_ty : param_type;
}

(** This identifies a message type within the context of a process.
It consists of the {{!endpoint}endpoint name} and the {{!msg}message type name}.
*)
type message_specifier = {
  endpoint : identifier;
  msg : identifier;
}

let string_of_msg_spec (msg_spec : message_specifier) : string =
  msg_spec.endpoint ^ "::" ^ msg_spec.msg

(** A delay pattern that matches a set of delays. *)
type delay_pat = [
  | `Cycles of int (** elapse of a number of cycles *)
  | `Message of message_specifier (** sending/receiving of a message of a specific type *)
  | `Eternal (** matches no delays *)
]

(** A delay pattern local to a specific channel. Being channel-local means that the message type
does not include an endpoint name component. *)
type delay_pat_chan_local = [ `Cycles of int | `Message of identifier | `Eternal ]

let string_of_delay_pat (t : delay_pat) : string =
  match t with
  | `Cycles n -> Printf.sprintf "#%d" n
  | `Message msg_spec -> Printf.sprintf "%s" (string_of_msg_spec msg_spec)
  | `Eternal -> "E"

(** Lifetime signature, specifying that the lifetime lasts until matching {{!sig_lifetime.e}ending} delay patterns. *)
type sig_lifetime = { e: delay_pat; }

(** Same as {!sig_lifetime} but local to a channel. *)
type sig_lifetime_chan_local = { e: delay_pat_chan_local; }

let string_of_lifetime (lt : sig_lifetime) : string =
  Printf.sprintf "%s" (string_of_delay_pat lt.e)

let sig_lifetime_this_cycle_chan_local : sig_lifetime_chan_local =
  { e = `Cycles 1 }

let sig_lifetime_const_chan_local : sig_lifetime_chan_local =
  { e = `Eternal }

let sig_lifetime_this_cycle : sig_lifetime =
  { e = `Cycles 1 }

let sig_lifetime_const : sig_lifetime =
  { e = `Eternal }


(** Direction of an endpoint. *)
type endpoint_direction = Left | Right

(** A data type. *)
type data_type = [
  | `Logic
  | `Array of data_type * int maybe_param
  | `Variant of (identifier * data_type option) list (** ADT sum type *)
  | `Record of (identifier * data_type) list (** ADT product type *)
  | `Tuple of data_type list
  | `Opaque of identifier (** type reserved for internal purposes *)
  | `Named of identifier * param_value list (** named type which can be concretised
                                            parameter values; the name itself might be
                                            a parameter *)
  (* | `Endpoint of endpoint_def * first-class endpoints *)
]
and param_value =
  | IntParamValue of int
  | TypeParamValue of data_type

(** Endpoint definition. A pair is
created once a channel class
is instantiated. *)
type endpoint_def = {
  name: identifier;
  channel_class: identifier;
  channel_params: param_value list;
  dir: endpoint_direction; (** direction of the endpoint *)
  (* used by this process? *)
  foreign: bool; (** must this endpoint be passed to other processes rather than
  used within this process? *)
  opp: identifier option; (** if the endpoint is created locally, the other endpoint associated
  with the same channel *)
}


type enum_def = {
  name: identifier;
  variants: identifier list;
}
type macro_def = {
  id: identifier;
  value : int;
}

(** A type definition ([type name = body])*)
and type_def = {
  name: identifier;
  body: data_type;
  params: param list; (** list of parameters *)
}

(** Unit type. Basically an empty tuple. *)
let unit_dtype = `Tuple []

(** Number of bits required to hold the tag for a variant type. *)
let variant_tag_size (v: [< `Variant of (identifier * data_type option) list]) : int =
  match v with
  | `Variant vlist -> List.length vlist |> Utils.int_log2

(** Data type a variant type constructor. *)
let variant_lookup_dtype (v: [< `Variant of (identifier * data_type option) list]) (cstr: identifier) : data_type option =
  match v with
  | `Variant vlist ->
      List.find_opt (fun x -> (fst x) = cstr) vlist |> Option.map snd |> Option.join

(** Index of a variant type constructor. *)
let variant_lookup_index (v: [< `Variant of (identifier * data_type option) list]) (cstr: identifier) : int option =
  let res : int option ref = ref None in
  match v with
  | `Variant vlist ->
      List.iteri (fun i x ->
        if Option.is_none !res then begin
          if (fst x) = cstr then
            res := Some i
          else ()
        end else ()) vlist;
  !res

type 'a sig_type_general = {
  dtype: data_type;
  lifetime: 'a;
}

(** Signal type, including both data type and the lifetime signature. *)
type sig_type = sig_lifetime sig_type_general

(** Same as {!sig_type} but local to a channel. *)
type sig_type_chan_local = sig_lifetime_chan_local sig_type_general

(** Convert a channel-local delay pattern to a global (process context) delay pattern. *)
let delay_pat_globalise (endpoint : identifier) (t : delay_pat_chan_local) : delay_pat =
  match t with
  | `Message msg -> `Message {endpoint = endpoint; msg = msg}
  | `Cycles n -> `Cycles n
  | `Eternal -> `Eternal

(** Convert a channel-local lifetime signature to a global (process context) lifetime signature. *)
let lifetime_globalise (endpoint : identifier) (lt : sig_lifetime_chan_local) : sig_lifetime =
  { e = delay_pat_globalise endpoint lt.e }

(** Convert a channel-local signal type to a global (process context) signal type. *)
let sig_type_globalise (endpoint : identifier) (s : sig_type_chan_local) : sig_type =
  {
    dtype = s.dtype;
    lifetime = lifetime_globalise endpoint s.lifetime
  }

(** A register definition. {!reg_def.init} specifies the initial value of the register. *)
type reg_def = {
  name: string;
  dtype: data_type;
  init: string option;
}

type reg_def_list = reg_def list

type message_direction = In | Out

(** Synchronisation mode of a message type. *)
type message_sync_mode =
  | Dynamic (** dynamic synchronisation, e.g., through [valid]/[ack] handshakes *)
  | Dependent of delay_pat_chan_local (** we have some static knowledge about
              when the synchronisation takes place. {b Currently ignored.} *)

(** A message type definition, as part of a channel definition. *)
type message_def = {
  name: identifier;
  dir: message_direction;
  send_sync: message_sync_mode; (** how to synchronise when data is available *)
  recv_sync: message_sync_mode; (** how to synchronise when data is acknowledged *)
  sig_types: sig_type_chan_local list; (** the signal types of the values carried in the message *)
}

(** A channel class definition, containing a list of message type definitions. *)
type channel_class_def = {
  name: identifier;
  messages: message_def list;
  params: param list;  (** List of generic parameters *)
}

(** The visibility of a channel. *)
type channel_visibility =
| BothForeign (** not visible locally, must be passed to other processes *)
| LeftForeign (** the left endpoint is not visible locally but the right one is *)
| RightForeign (** the right endpoint is not visible locally but the left one is *)

(** A channel (instantiation of a channel class) definition. *)
type channel_def = {
  channel_class: identifier;
  channel_params: param_value list;
  endpoint_left: identifier;
  endpoint_right: identifier;
  visibility: channel_visibility;
}

(* expressions *)

type bit = [`Z0 | `Z1]
type digit = [`Z0 | `Z1 | `Z2 | `Z3 | `Z4 | `Z5 | `Z6 | `Z7 | `Z8 | `Z9]
type hexit = [`Z0 | `Z1 | `Z2 | `Z3 | `Z4 | `Z5 | `Z6 | `Z7 | `Z8 | `Z9 |
            `Za | `Zb | `Zc | `Zd | `Ze | `Zf]
type all_digit = hexit

let string_of_digit d : string =
  match d with
  | `Z0 -> "0"
  | `Z1 -> "1"
  | `Z2 -> "2"
  | `Z3 -> "3"
  | `Z4 -> "4"
  | `Z5 -> "5"
  | `Z6 -> "6"
  | `Z7 -> "7"
  | `Z8 -> "8"
  | `Z9 -> "9"
  | `Za -> "a"
  | `Zb -> "b"
  | `Zc -> "c"
  | `Zd -> "d"
  | `Ze -> "e"
  | `Zf -> "f"

let value_of_digit d : int =
  match d with
  | `Z0 -> 0x0
  | `Z1 -> 0x1
  | `Z2 -> 0x2
  | `Z3 -> 0x3
  | `Z4 -> 0x4
  | `Z5 -> 0x5
  | `Z6 -> 0x6
  | `Z7 -> 0x7
  | `Z8 -> 0x8
  | `Z9 -> 0x9
  | `Za -> 0xa
  | `Zb -> 0xb
  | `Zc -> 0xc
  | `Zd -> 0xd
  | `Ze -> 0xe
  | `Zf -> 0xf

type literal =
| Binary of int * bit list
| Decimal of int * digit list
| Hexadecimal of int * hexit list
| WithLength of int * int
| NoLength of int

let literal_bit_len (lit : literal) : int option =
  match lit with
  | Binary (n, _) | Decimal (n, _) | Hexadecimal (n, _) | WithLength (n, _) -> Some n
  | NoLength v -> Some (Utils.int_log2 (v + 1))

let literal_eval (lit : literal) : int =
  match lit with
  | Binary (_, b) ->
      List.fold_left (fun n x -> n * 2 + (value_of_digit x)) 0 b
  | Decimal (_, d) ->
      List.fold_left (fun n x -> n * 10 + (value_of_digit x)) 0 d
  | Hexadecimal (_, h) ->
      List.fold_left (fun n x -> n * 16 + (value_of_digit x)) 0 h
  | WithLength (_, v) -> v
  | NoLength v -> v

let dtype_of_literal (lit : literal) =
  let n = literal_bit_len lit |> Option.get in
  `Array (`Logic, ParamEnv.Concrete n)

type binop = Add | Sub | Xor | And | Or | Lt | Gt | Lte | Gte |
             Shl | Shr | Eq | Neq | Mul
type unop  = Neg | Not | AndAll | OrAll

(* TODO: these are SV-specific; move elsewhere *)
let string_of_binop (binop: binop) : string =
  match binop with
  | Add -> "+"
  | Sub -> "-"
  | Xor -> "^"
  | And -> "&"
  | Or -> "|"
  | Lt -> "<"
  | Gt -> ">"
  | Lte -> "<="
  | Gte -> ">="
  | Shl -> "<<"
  | Shr -> ">>"
  | Eq -> "=="
  | Neq -> "!="
  | Mul -> "*"

let string_of_unop (unop: unop) : string =
  match unop with
  | Neg -> "-"
  | Not -> "~"
  | AndAll -> "&"
  | OrAll -> "|"

(** Information specified in a message send operation. *)
type send_pack = {
  send_msg_spec: message_specifier;
  send_data: expr_node;
}

(** Information specified in a message receive operation. *)
and recv_pack = {
  recv_msg_spec: message_specifier;
}
and constructor_spec = {
  variant_ty_name: identifier;
  variant: identifier;
}

(** An expression. This is the basic building block for a program. *)
and expr =
  | Literal of literal
  | Identifier of identifier
  | Call of identifier *(expr_node list)
  (* send and recv *)
  | Assign of lvalue * expr_node
  | Binop of binop * expr_node * expr_node
  | Unop of unop * expr_node
  | Tuple of expr_node list
  | LetIn of identifier list * expr_node * expr_node
  | Wait of expr_node * expr_node (** [a => b] *)
  | Cycle of int
  | Sync of identifier (** synchronise on a shared value *)
  | IfExpr of expr_node * expr_node * expr_node
  | Construct of constructor_spec * expr_node option (** construct a variant type value with a constructor *)
  | Record of identifier * (identifier * expr_node) list (** constructing a record-type value *)
  | Index of expr_node * index (** an element of an array ([a[3]]) *)
  | Indirect of expr_node * identifier (** a member of a record ([a.b]) *)
  | EnumRef of identifier * identifier (** a reference to an enum variant *)
  | Concat of expr_node list
  | Ready of message_specifier (** [ready(a, b)] *)
  | Match of expr_node * ((match_pattern * expr_node option) list) (** Currently unused. Match expressions are
                                  converted into if expressions in the parser directly. *)
  | Read of identifier (** reading a value from a register (leading to a borrow) *)
  | Debug of debug_op
  | Send of send_pack
  | Recv of recv_pack
  | SharedAssign of identifier * expr_node (** make ready a shared value *)
  | List of expr_node list (** array/list of expressions *)
and expr_node = expr ast_node

(** A "location" that can be assigned to. *)
and lvalue =
  | Reg of identifier (** a register *)
  | Indexed of lvalue * index (** lvalue[index] *)
  | Indirected of lvalue * identifier (** lvalue.field *)

(** Indexing, either a single point or a range. *)
and index =
  | Single of expr_node
  | Range of expr_node * expr_node (** a range, the second component is the size which must be literal *)

(** Pattern in an arm of a {{!Match}match} expression. *)
and match_pattern = {
  cstr: identifier; (* constructor identifier *)
  bind_name: identifier option; (* name of the binding for the unboxed value *)
}
and debug_op =
  | DebugPrint of string * expr_node list
  | DebugFinish

let delay_immediate = `Cycles 0
let delay_single_cycle = `Cycles 1

type sig_def = {
  name: identifier;
  stype: sig_type;
}

type cycle_proc = {
  trans_func: expr_node;
  sigs: sig_def list;
}

(** A spawn of a process. *)
type spawn_def = {
  proc: identifier;
  (* channels to pass as args *)
  (* TODO: this naming collides with compile-time parameters *)
  params: identifier list; (** names of the endpoints passed to the spawned process *)
  compile_params: param_value list; (** concrete param value list *)
}

(* TODO: specify the type? *)
type shared_var_def = {
  ident: identifier;
  assigning_thread: int;
  shared_lifetime: sig_lifetime;
}

(** Process body. *)
type proc_def_body = {
  (* new channels available to this process *)
  channels: channel_def list;
  (* processes spawned by this process *)
  spawns: spawn_def list;
  regs: reg_def list;
  shared_vars: shared_var_def list;  (* New field *)
  (* prog: expr; *)
  loops: expr_node list;
}

(** Extern process definition *)
type proc_def_body_extern = {
  named_ports : (string * string) list;
  msg_ports : (message_specifier * string option * string option * string option) list;
  (** data, valid, and ack ports *)
}

type proc_def_body_maybe_extern =
  | Native of proc_def_body
  | Extern of string * proc_def_body_extern (** module name and port bindings *)

(** Process definition. *)
type proc_def = {
  name: string;
  (* arguments are endpoints passed from outside *)
  args: endpoint_def list; (** endpoints passed from outside *)
  body: proc_def_body_maybe_extern; (** process body *)
  params: param list; (** compile-time parameters *)
}

(** An import directive for importing code from other files. *)
type import_directive = {
  file_name : string;
  is_extern : bool; (** is this import external?
      Currently an external import means importing SystemVerilog code *)
}
type func_def =  {
  name: identifier;
  args: identifier list;
  body: expr_node;
}

(** A compilation unit, corresponding to a source file. *)
type compilation_unit = {
  channel_classes: channel_class_def list;
  type_defs: type_def list;
  enum_defs: enum_def list;
  macro_defs: macro_def list;
  func_defs : func_def list;
  procs: proc_def list;
  imports : import_directive list;
  _extern_procs : proc_def list; (** processes that are external, usable but not built *)
}

let cunit_empty : compilation_unit =
  { channel_classes = []; type_defs = []; procs = []; imports = []; _extern_procs = [];enum_defs = [];macro_defs = [];func_defs = []}

let cunit_add_channel_class
  (c : compilation_unit) (cc : channel_class_def) : compilation_unit =
  {c with channel_classes = cc::c.channel_classes}

let cunit_add_type_def (c : compilation_unit) (ty : type_def) : compilation_unit =
  {c with type_defs = ty::c.type_defs}

let cunit_add_func_def (c : compilation_unit) (f : func_def) : compilation_unit =
  {c with func_defs = f::c.func_defs}
let cunit_add_enum_def (c : compilation_unit) (enum : enum_def) :
  compilation_unit = {c with enum_defs = enum::c.enum_defs}
let cunit_add_macro_def (c : compilation_unit) (macro : macro_def) :
  compilation_unit = {c with macro_defs = macro::c.macro_defs}
let cunit_add_proc
  (c : compilation_unit) (p : proc_def) : compilation_unit =
  {c with procs = p::c.procs}

let cunit_add_import (c : compilation_unit) (im : import_directive) : compilation_unit =
  {c with imports = im::c.imports}

(** Reverse a message direction. *)
let reverse (msg_dir : message_direction) : message_direction =
  match msg_dir with
  | In -> Out
  | Out -> In

let get_message_direction (msg_dir : message_direction)
            (endpoint_dir : endpoint_direction) : message_direction =
  match endpoint_dir with
  | Left -> msg_dir
  | Right -> reverse msg_dir

let string_of_channel_class (cc : channel_class_def) : string =
  Printf.sprintf "Channel class %s" cc.name
let rec string_of_expr (e : expr) : string =
  match e with
  | Literal lit -> "Literal " ^ string_of_literal lit
  | Identifier id -> "Identifier " ^ id
  | LetIn (ids, e1, e2) -> "LetIn (" ^ String.concat ", " ids ^ ", " ^ string_of_expr e1.d ^ ", " ^ string_of_expr e2.d ^ ")"
  | Assign (lv, n) -> "Assign (" ^ string_of_lvalue lv ^ ", " ^ string_of_expr n.d ^ ")"
  | _ -> "..."

and string_of_lvalue (lv : lvalue) : string =
  match lv with
  | Reg id -> "Reg " ^ id
  | Indexed (lv, idx) -> "Indexed (" ^ string_of_lvalue lv ^ ", " ^ string_of_index idx ^ ")"
  | Indirected (lv, id) -> "Indirected (" ^ string_of_lvalue lv ^ ", " ^ id ^ ")"

and string_of_index (idx : index) : string =
  match idx with
  | Single e -> "Single (" ^ string_of_expr e.d ^ ")"
  | Range (e1, e2) -> "Range (" ^ string_of_expr e1.d ^ ", " ^ string_of_expr e2.d ^ ")"

and string_of_literal (lit : literal) : string =
  match lit with
  | Binary (n, bits) -> "Binary (" ^ string_of_int n ^ ", [" ^ String.concat ";" (List.map string_of_digit bits) ^ "])"
  | Decimal (n, digits) -> "Decimal (" ^ string_of_int n ^ ", [" ^ String.concat ";" (List.map string_of_digit digits) ^ "])"
  | Hexadecimal (n, hexits) -> "Hexadecimal (" ^ string_of_int n ^ ", [" ^ String.concat ";" (List.map string_of_digit hexits) ^ "])"
  | WithLength (n, v) -> "WithLength (" ^ string_of_int n ^ ", " ^ string_of_int v ^ ")"
  | NoLength v -> "NoLength " ^ string_of_int v

let generate_match_expression (e, match_arms) =

  let is_default_pattern = function
    | {d = Identifier "_"; _} -> true
    | _ -> false
  in

  let match_arms_node = List.map (fun (pat, body_opt) ->
    let pat_node = ast_node_of_data Lexing.dummy_pos Lexing.dummy_pos pat in
    let body_node_opt = Option.map (fun body ->
      ast_node_of_data Lexing.dummy_pos Lexing.dummy_pos body
    ) body_opt in
    (pat_node, body_node_opt)
  ) match_arms in

  let default_case, other_cases =
    List.partition (fun (pat, _) -> is_default_pattern pat) match_arms_node
  in

  match default_case with
  | [] -> failwith "Match expression must have a default case (_)"
  | [(_, None)] -> failwith "Default case must have a body"
  | [(_, Some default_body)] ->
      List.fold_right (fun (pattern, body_opt) acc ->
        match body_opt with
        | None -> failwith "Match arm must have a body"
        | Some body ->
            let condition = ast_node_of_data e.span.st e.span.ed (Binop (Eq, e, pattern)) in
            IfExpr (condition, body,  dummy_ast_node_of_data acc)
      ) other_cases default_body.d
  | _ -> failwith "Multiple default cases found in match expression"

  let rec substitute_expr_identifier (id: identifier) (value: expr_node) (expr: expr_node) : expr_node =
    let new_expr = match expr.d with
    | Identifier name when name = id ->
        value.d
    | LetIn (ids, e1, e2) ->
        if List.mem id ids then expr.d
        else LetIn (ids, substitute_expr_identifier id value e1, substitute_expr_identifier id value e2)
    | Binop (op, e1, e2) ->
        Binop (op, substitute_expr_identifier id value e1, substitute_expr_identifier id value e2)
    | Unop (op, e) ->
        Unop (op, substitute_expr_identifier id value e)
    | Tuple exprs ->
        Tuple (List.map (substitute_expr_identifier id value) exprs)
    | Wait (e1, e2) ->
        Wait (substitute_expr_identifier id value e1, substitute_expr_identifier id value e2)
    | Index (arr, idx) ->
        let new_arr = substitute_expr_identifier id value arr in
        let new_idx = match idx with
          | Single e -> Single (substitute_expr_identifier id value e)
          | Range (e1, e2) -> Range (substitute_expr_identifier id value e1, substitute_expr_identifier id value e2)
        in
        Index (new_arr, new_idx)
    | Concat exprs ->
        Concat (List.map (substitute_expr_identifier id value) exprs)
    | Assign (lv, e) ->
        let new_lv = substitute_lvalue id value lv in
        Assign (new_lv, substitute_expr_identifier id value e)
    | Send {send_msg_spec; send_data} ->
        Send {
          send_msg_spec;
          send_data = substitute_expr_identifier id value send_data
        }
    | Debug (DebugPrint (msg, exprs)) ->
        Debug (DebugPrint (msg, List.map (substitute_expr_identifier id value) exprs))
    | Debug other_debug -> Debug other_debug
    | IfExpr (cond, then_expr, else_expr) ->
        IfExpr (
          substitute_expr_identifier id value cond,
          substitute_expr_identifier id value then_expr,
          substitute_expr_identifier id value else_expr
        )
    | Indirect (e, field) ->
        Indirect (substitute_expr_identifier id value e, field)
    | Call (name, args) ->
        Call (name, List.map (substitute_expr_identifier id value) args)
    | _ -> expr.d
    in
    { expr with d = new_expr }

  and substitute_lvalue (id: identifier) (value: expr_node) (lv: lvalue) : lvalue =
      match (lv:lvalue) with
      | Reg name ->
          if name = id then
            match value.d with
            | Identifier new_id -> Reg new_id
            | _ -> failwith "Expected identifier"
          else
            lv
      | Indexed (lv', idx) ->
          let new_lv = substitute_lvalue id value lv' in
          let new_idx = match idx with
            | Single e -> Single (substitute_expr_identifier id value e)
            | Range (e1, e2) -> Range (substitute_expr_identifier id value e1, substitute_expr_identifier id value e2)
          in
          Indexed (new_lv, new_idx)
      | Indirected (lv', field) ->
          let new_lv = substitute_lvalue id value lv' in
          Indirected (new_lv, field)

  let substitute_func_args (func: func_def) (passed_args: expr_node list) : expr_node =
    if List.length func.args <> List.length passed_args then
      failwith (Printf.sprintf "Number of arguments does not match function definition of %s" func.name);

    List.fold_left2 (fun acc_body arg_name arg_value ->
      substitute_expr_identifier arg_name arg_value acc_body
    ) func.body func.args passed_args

    let generate_expr (id, start, end_v, offset, body) =
      let rec generate_exprs (curr:int) acc =
        if curr > end_v then
          match List.rev acc with
          | [] -> Tuple []
          | [single] -> single.d
          | hd::tl ->
              List.fold_left
                (fun acc expr -> LetIn ([], expr, dummy_ast_node_of_data acc))
                hd.d
                tl
        else
          let substituted = substitute_expr_identifier id (dummy_ast_node_of_data(Literal(NoLength(curr)))) body in
          generate_exprs (curr + offset) (substituted :: acc);

      in
      generate_exprs start []