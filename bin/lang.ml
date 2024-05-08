type identifier = string

type message_specifier = {
  endpoint : identifier;
  msg : identifier;
}

type future =
| Cycles of int
| AtSend of message_specifier
| AtRecv of message_specifier

type sig_lifetime = { b: future; e: future;}

type data_type =
  | Logic
  | Type of string
  | Array of data_type * int

let rec data_type_size dtype =
  match dtype with
  | Logic -> 1
  | Type _ -> 1 (* this is a hack *)
  | Array (dtype', n) -> (data_type_size dtype') * n

type sig_type = {
  dtype: data_type;
  lifetime: sig_lifetime;
}

type reg_def = {
  name: string;
  dtype: data_type;
  init: string option;
}

type reg_def_list = reg_def list

type message_direction = In | Out

(* message definition *)
type message_def = {
  name: identifier;
  dir: message_direction;
  sig_types: sig_type list;
  ret_types: sig_type list;
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

type bit = Zero | One

let string_of_bit (b : bit) : string =
  match b with
  | Zero -> "0"
  | One -> "1"

type literal = bit list

type binop = Add | Sub | Xor | And | Or
type unop  = Neg | Not | AndAll | OrAll

(* TODO: these are SV-specific; move elsewhere *)
let string_of_binop (binop: binop) : string =
  match binop with
  | Add -> "+"
  | Sub -> "-"
  | Xor -> "^"
  | And -> "&"
  | Or -> "|"

type expr =
  | Literal of literal
  | Identifier of identifier
  | Function of identifier * expr
  (* send and recv *)
  | TrySend of identifier
  | TryRecv of identifier
  | Assign of identifier * expr
  | Apply of expr * expr
  | Binop of binop * expr * expr
  | Unop of unop * expr
  | Tuple of expr list
  | LetIn of identifier * expr * expr
  | IfExpr of expr * expr * expr
  | Return of identifier * identifier * expr

(* the delay before a cycle *)
type delay_def =
  | WaitCycles of int
  | Send of (identifier list) * message_specifier * expr
  | Recv of (identifier list) * message_specifier

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

(* process body*)
type proc_body = {
  delays: delay_def list;
  cycle : expr;
  transition : proc_transition;
}
and proc_transition =
  | Seq
  | If of proc_body_list
  | IfElse of proc_body_list * proc_body_list
  | While of proc_body_list
and proc_body_list = proc_body list

(* process definition *)
type proc_def = {
  name: string;
  (* arguments are endpoints passed from outside *)
  args: endpoint_def list;
  (* new channels available to this process *)
  channels: channel_def list;
  regs: reg_def list;
  body: proc_body_list;
}

type compilation_unit = {
  channel_classes: channel_class_def list;
  procs: proc_def list;
}

let reverse (msg_dir : message_direction) : message_direction =
  match msg_dir with
  | In -> Out
  | Out -> In

let get_message_direction (msg_dir : message_direction)
            (endpoint_dir : endpoint_direction) : message_direction =
  match endpoint_dir with
  | Left -> msg_dir
  | Right -> reverse msg_dir