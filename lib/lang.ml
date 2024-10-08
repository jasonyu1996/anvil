(** This module includes definitions relevant to the language constructs, including
those that are part of the AST created during parsing. *)

type identifier = string

(** This identifies a message type within the context of a process.
It consists of the {{!endpoint}endpoint name} and the {{!msg}message type name}.
*)
type message_specifier = {
  endpoint : identifier;
  msg : identifier;
}

let string_of_msg_spec (msg_spec : message_specifier) : string =
  msg_spec.endpoint ^ "::" ^ msg_spec.msg

(* TODO: more complex patterns *)
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

(** Lifetime signature consisting of a pair of {{!sig_lifetime.b}beginning} and {{!sig_lifetime.e}ending} delay patterns.
The beginning delay pattern is currently ignored.
*)
type sig_lifetime = { b: delay_pat; e: delay_pat;}

(** Same as {!sig_lifetime} but local to a channel. *)
type sig_lifetime_chan_local = { b: delay_pat_chan_local; e: delay_pat_chan_local; }

let string_of_lifetime (lt : sig_lifetime) : string =
  Printf.sprintf "%s-%s" (string_of_delay_pat lt.b) (string_of_delay_pat lt.e)

let sig_lifetime_this_cycle_chan_local : sig_lifetime_chan_local =
  { b = `Cycles 0; e = `Cycles 1 }

let sig_lifetime_null_chan_local : sig_lifetime_chan_local =
  { b = `Eternal; e = `Cycles 0 }

let sig_lifetime_const_chan_local : sig_lifetime_chan_local =
  { b = `Cycles 0; e = `Eternal }


let sig_lifetime_this_cycle : sig_lifetime =
  { b = `Cycles 0; e = `Cycles 1 }

let sig_lifetime_null : sig_lifetime =
  { b = `Eternal; e = `Cycles 0 }

let sig_lifetime_const : sig_lifetime =
  { b = `Cycles 0; e = `Eternal }

(** Type definition without named type. *)
type 'a data_type_generic_no_named = [
  | `Logic
  | `Array of 'a * int
  | `Variant of (identifier * 'a option) list (** ADT sum type *)
  | `Record of (identifier * 'a) list (** ADT product type *)
  | `Tuple of 'a list
  | `Opaque of identifier (** type reserved for internal purposes *)
]

type 'a data_type_generic = [
  | `Named of identifier (** named type *)
  | 'a data_type_generic_no_named
]

(** A data type after resolution (no named type). *)
type resolved_data_type = resolved_data_type data_type_generic_no_named

(** A data type. *)
type data_type = data_type data_type_generic

(** A type definition ([type name = body])*)
and type_def = {
  name: identifier;
  body: data_type;
}

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
  { b = delay_pat_globalise endpoint lt.b; e = delay_pat_globalise endpoint lt.e }

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
}

(** The visibility of a channel. *)
type channel_visibility =
| BothForeign (** not visible locally, must be passed to other processes *)
| LeftForeign (** the left endpoint is not visible locally but the right one is *)
| RightForeign (** the right endpoint is not visible locally but the left one is *)

(** A channel (instantiation of a channel class) definition. *)
type channel_def = {
  channel_class: identifier;
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
  | _ -> None

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

let dtype_of_literal (lit : literal) : resolved_data_type =
  let n = literal_bit_len lit |> Option.get in
  `Array (`Logic, n)

type binop = Add | Sub | Xor | And | Or | Lt | Gt | Lte | Gte |
             Shl | Shr | Eq | Neq
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

let string_of_unop (unop: unop) : string =
  match unop with
  | Neg -> "-"
  | Not -> "~"
  | AndAll -> "&"
  | OrAll -> "|"

(** Information specified in a message send operation. *)
type send_pack = {
  send_msg_spec: message_specifier;
  send_data: expr;
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
  (* send and recv *)
  | Assign of lvalue * expr
  | Binop of binop * expr * expr
  | Unop of unop * expr
  | Tuple of expr list
  | LetIn of identifier list * expr * expr
  | Wait of expr * expr (** [a => b] *)
  | Cycle of int
  | Sync of identifier * expr
  | IfExpr of expr * expr * expr
  (* construct a variant type value with a constructor *)
  | Construct of constructor_spec * expr option
  | Record of identifier * (identifier * expr) list (** constructing a record-type value *)
  | Index of expr * index (** an element of an array ([a[3]]) *)
  | Indirect of expr * identifier (** a member of a record ([a.b]) *)
  | Concat of expr list
  | Match of expr * ((match_pattern * expr option) list)
  | Read of identifier (** reading a value from a register (leading to a borrow) *)
  | Debug of debug_op
  | Send of send_pack
  | Recv of recv_pack
  | SharedAssign of identifier * expr * (expr option)

(** A "location" that can be assigned to. *)
and lvalue =
  | Reg of identifier (** a register *)
  | Indexed of lvalue * index (** lvalue[index]. Unimplemented *)
  | Indirected of lvalue * identifier (** lvalue.field. Unimplemented *)

(** Indexing, either a single point or a range. *)
and index =
  | Single of expr
  | Range of expr * expr

(** Pattern in an arm of a {{!Match}match} expression. *)
and match_pattern = {
  cstr: identifier; (* constructor identifier *)
  bind_name: identifier option; (* name of the binding for the unboxed value *)
}
and debug_op =
  | DebugPrint of string * expr list
  | DebugFinish

let delay_immediate = `Cycles 0
let delay_single_cycle = `Cycles 1

type sig_def = {
  name: identifier;
  stype: sig_type;
}

type cycle_proc = {
  trans_func: expr;
  sigs: sig_def list;
}

(** Direction of an endpoint. *)
type endpoint_direction = Left | Right

(** Endpoint definition. A pair is
created once a channel class
is instantiated. *)
type endpoint_def = {
  name: identifier;
  channel_class: identifier;
  dir: endpoint_direction; (** direction of the endpoint *)
  (* used by this process? *)
  foreign: bool; (** must this endpoint be passed to other processes rather than
  used within this process? *)
  opp: identifier option; (** if the endpoint is created locally, the other endpoint associated
  with the same channel *)
}

(** A spawn of a process. *)
type spawn_def = {
  proc: identifier;
  (* channels to pass as args *)
  params: identifier list; (** names of the endpoints passed to the spawned process *)
}

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
  loops: expr list;
}

(** Process definition. *)
type proc_def = {
  name: string;
  (* arguments are endpoints passed from outside *)
  args: endpoint_def list; (** endpoints passed from outside *)
  body: proc_def_body; (** process body *)
}

(** A compilation unit, corresponding to a source file. *)
type compilation_unit = {
  channel_classes: channel_class_def list;
  type_defs: type_def list;
  procs: proc_def list;
}

let cunit_empty : compilation_unit =
  { channel_classes = []; type_defs = []; procs = [] }

let cunit_add_channel_class
  (c : compilation_unit) (cc : channel_class_def) : compilation_unit =
  {c with channel_classes = cc::c.channel_classes}

let cunit_add_type_def (c : compilation_unit) (ty : type_def) : compilation_unit =
  {c with type_defs = ty::c.type_defs}

let cunit_add_proc
  (c : compilation_unit) (p : proc_def) : compilation_unit =
  {c with procs = p::c.procs}

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

let rec string_of_expr (e : expr) : string =
  match e with
  | Literal lit -> "Literal " ^ string_of_literal lit
  | Identifier id -> "Identifier " ^ id
  | Assign (lv, e) -> "Assign (" ^ string_of_lvalue lv ^ ", " ^ string_of_expr e ^ ")"
  | _ -> "..."

and string_of_lvalue (lv : lvalue) : string =
  match lv with
  | Reg id -> "Reg " ^ id
  | Indexed (lv, idx) -> "Indexed (" ^ string_of_lvalue lv ^ ", " ^ string_of_index idx ^ ")"
  | Indirected (lv, id) -> "Indirected (" ^ string_of_lvalue lv ^ ", " ^ id ^ ")"

and string_of_index (idx : index) : string =
  match idx with
  | Single e -> "Single (" ^ string_of_expr e ^ ")"
  | Range (e1, e2) -> "Range (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"

and string_of_literal (lit : literal) : string =
  match lit with
  | Binary (n, bits) -> "Binary (" ^ string_of_int n ^ ", [" ^ String.concat ";" (List.map string_of_digit bits) ^ "])"
  | Decimal (n, digits) -> "Decimal (" ^ string_of_int n ^ ", [" ^ String.concat ";" (List.map string_of_digit digits) ^ "])"
  | Hexadecimal (n, hexits) -> "Hexadecimal (" ^ string_of_int n ^ ", [" ^ String.concat ";" (List.map string_of_digit hexits) ^ "])"
  | WithLength (n, v) -> "WithLength (" ^ string_of_int n ^ ", " ^ string_of_int v ^ ")"
  | NoLength v -> "NoLength " ^ string_of_int v