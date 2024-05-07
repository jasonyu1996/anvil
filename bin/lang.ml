type identifier = string

type future = Cycles of int | Signal of string

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
  | Send of identifier
  | Recv of identifier
  | Assign of identifier * expr
  | Apply of expr * expr
  | Binop of binop * expr * expr
  | Unop of unop * expr
  | Tuple of expr list
  | LetIn of identifier * expr * expr
  | IfExpr of expr * expr * expr

type sig_def = {
  name: identifier;
  stype: sig_type;
}

type cycle_proc = {
  trans_func: expr;
  sigs: sig_def list;
}


(* process body*)
type proc_body = {
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
  msgs: message_def list;
  regs: reg_def list;
  body: proc_body_list;
}

type compilation_unit = proc_def list
