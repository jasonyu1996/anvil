type identifier = string

type future = Cycles of int | Signal of string

type sig_lifetime = { b: future; e: future;}

type data_type =
  | Logic
  | Array of data_type * int

let rec data_type_size dtype =
  match dtype with
  | Logic -> 1
  | Array (dtype', n) -> (data_type_size dtype') * n

type sig_type = {
  dtype: data_type;
  lifetime: sig_lifetime;
}

type reg_def = {
  name: string;
  dtype: data_type;
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
type proc_body =
  | EmptyProcBody
  | Seq of expr * proc_body
  | If of expr * proc_body
  | IfElse of expr * proc_body * proc_body
  | Until of expr * proc_body

(* process definition *)
type proc_def = {
  name: string;
  msgs: message_def list;
  regs: reg_def list;
  body: proc_body;
}

type compilation_unit = proc_def list
