type identifier = string

type message_specifier = {
  endpoint : identifier;
  msg : identifier;
}

let string_of_msg_spec (msg_spec : message_specifier) : string =
  msg_spec.endpoint ^ "::" ^ msg_spec.msg

type future = [ `Cycles of int | `Message of message_specifier | `Eternal ]

(* future definition that is local to a specific channel *)
type future_chan_local = [ `Cycles of int | `Message of identifier | `Eternal ]

let string_of_future (t : future) : string =
  match t with
  | `Cycles n -> Printf.sprintf "#%d" n
  | `Message msg_spec -> Printf.sprintf "%s" (string_of_msg_spec msg_spec)
  | `Eternal -> "E"

type sig_lifetime = { b: future; e: future;}
type sig_lifetime_chan_local = { b: future_chan_local; e: future_chan_local; }

let string_of_lifetime (lt : sig_lifetime) : string =
  Printf.sprintf "%s-%s" (string_of_future lt.b) (string_of_future lt.e)

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

(* type definition *)
type 'a data_type_generic_no_named = [
  | `Logic
  | `Array of 'a * int
  | `Variant of (identifier * 'a option) list
  | `Record of (identifier * 'a) list
  | `Tuple of 'a list
  | `Opaque of identifier (* reserved named type *)
]

type 'a data_type_generic = [
  | `Named of identifier (* named type *)
  | 'a data_type_generic_no_named
]

type resolved_data_type = resolved_data_type data_type_generic_no_named
type data_type = data_type data_type_generic

and type_def = {
  name: identifier;
  body: data_type;
}

let variant_tag_size (v: [< `Variant of (identifier * data_type option) list]) : int =
  match v with
  | `Variant vlist -> List.length vlist |> Utils.int_log2

let variant_lookup_dtype (v: [< `Variant of (identifier * data_type option) list]) (cstr: identifier) : data_type option =
  match v with
  | `Variant vlist ->
      List.find_opt (fun x -> (fst x) = cstr) vlist |> Option.map snd |> Option.join

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

type sig_type = sig_lifetime sig_type_general
type sig_type_chan_local = sig_lifetime_chan_local sig_type_general

let future_globalise (endpoint : identifier) (t : future_chan_local) : future =
  match t with
  | `Message msg -> `Message {endpoint = endpoint; msg = msg}
  | `Cycles n -> `Cycles n
  | `Eternal -> `Eternal

let lifetime_globalise (endpoint : identifier) (lt : sig_lifetime_chan_local) : sig_lifetime =
  { b = future_globalise endpoint lt.b; e = future_globalise endpoint lt.e }

let sig_type_globalise (endpoint : identifier) (s : sig_type_chan_local) : sig_type =
  {
    dtype = s.dtype;
    lifetime = lifetime_globalise endpoint s.lifetime
  }

type reg_def = {
  name: string;
  dtype: data_type;
  init: string option;
}

type reg_def_list = reg_def list

type message_direction = In | Out

type message_sync_mode =
  | Dynamic
  | Dependent of future_chan_local

(* message definition *)
type message_def = {
  name: identifier;
  dir: message_direction;
  send_sync: message_sync_mode; (* how to synchronise when data is available *)
  recv_sync: message_sync_mode; (* how to synchronise when data is acknowledged *)
  sig_types: sig_type_chan_local list;
}

type channel_class_def = {
  name: identifier;
  messages: message_def list;
}

type channel_visibility = BothForeign | LeftForeign | RightForeign

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
| NoLength of int

let literal_bit_len (lit : literal) : int option =
  match lit with
  | Binary (n, _) | Decimal (n, _) | Hexadecimal (n, _) -> Some n
  | _ -> None

let literal_eval (lit : literal) : int =
  match lit with
  | Binary (_, b) ->
      List.fold_left (fun n x -> n * 2 + (value_of_digit x)) 0 b
  | Decimal (_, d) ->
      List.fold_left (fun n x -> n * 10 + (value_of_digit x)) 0 d
  | Hexadecimal (_, h) ->
      List.fold_left (fun n x -> n * 16 + (value_of_digit x)) 0 h
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

type send_pack = {
  send_msg_spec: message_specifier;
  send_data: expr;
}
and recv_pack = {
  recv_binds: identifier list;
  recv_msg_spec: message_specifier;
}
and constructor_spec = {
  variant_ty_name: identifier;
  variant: identifier;
}
and expr =
  | Literal of literal
  | Identifier of identifier
  (* send and recv *)
  | Assign of lvalue * expr
  | Binop of binop * expr * expr
  | Unop of unop * expr
  | Tuple of expr list
  | LetIn of identifier list * expr * expr
  | Wait of expr * expr
  | Cycle of int
  | IfExpr of expr * expr * expr
  (* construct a variant type value with a constructor *)
  | Construct of constructor_spec * expr option
  | Record of identifier * (identifier * expr) list
  | Index of expr * index
  | Indirect of expr * identifier
  | Concat of expr list
  | Match of expr * ((match_pattern * expr option) list)
  | Read of identifier
  | Debug of debug_op
and lvalue =
  | Reg of identifier
  | Indexed of lvalue * index (* lvalue[index] *)
  | Indirected of lvalue * identifier (* lvalue.field *)
and index =
  | Single of expr
  | Range of expr * expr
and match_pattern = {
  cstr: identifier; (* constructor identifier *)
  bind_name: identifier option; (* name of the binding for the unboxed value *)
}
and debug_op =
  | DebugPrint of string * expr list
  | DebugFinish

type delay = [
  | `Ever
  | `Cycles of int
  | `Later of delay * delay
  | `Earlier of delay * delay
  | `Seq of delay * delay
]

type atomic_delay = [
  | `Cycles of int
]

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

type endpoint_direction = Left | Right

type endpoint_def = {
  name: identifier;
  channel_class: identifier;
  dir: endpoint_direction;
  (* used by this process? *)
  foreign: bool;
  (* the other end, if available *)
  opp: identifier option;
}

type spawn_def = {
  proc: identifier;
  (* channels to pass as args *)
  params: identifier list;
}

type proc_def_body = {
  (* new channels available to this process *)
  channels: channel_def list;
  (* processes spawned by this process *)
  spawns: spawn_def list;
  regs: reg_def list;
  prog: expr;
}

(* process definition *)
type proc_def = {
  name: string;
  (* arguments are endpoints passed from outside *)
  args: endpoint_def list;
  body: proc_def_body;
}

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

let reverse (msg_dir : message_direction) : message_direction =
  match msg_dir with
  | In -> Out
  | Out -> In

let get_message_direction (msg_dir : message_direction)
            (endpoint_dir : endpoint_direction) : message_direction =
  match endpoint_dir with
  | Left -> msg_dir
  | Right -> reverse msg_dir
