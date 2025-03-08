(** A wire describes results of untimed computation.
This module provides definitions for creating and managing wires and wire collections.

The type {!t} maintains a collection of wires.
The [add_*] functions create new wires and add them to the collection.
*)


module Wire = struct
  type borrow_source = string

  (** A wire. *)
  type t = {
    id: int;
    thread_id: int;
    size: int;
    source: wire_source;
    is_const: bool;
  }
  and wire_source =
    | Literal of Lang.literal
    | Binary of Lang.binop * t * t
    | Unary of Lang.unop * t
    | Switch of (t * t) list * t (* (cond, val) list, default *)
    | Cases of t * (t * t) list * t (* (cond v, case pat, val) list, default *)
    | RegRead of Lang.identifier
    | MessagePort of Lang.message_specifier * int (* index of the port *)
    | MessageValidPort of Lang.message_specifier (* New wire source for message valid port *)
    | Concat of t list
    | Slice of t * t MaybeConst.maybe_int_const * int (** third component is size *)

  let new_literal id thread_id typedefs (macro_defs: Lang.macro_def list) lit =
    {
      id;
      thread_id;
      source = Literal lit;
      size = (Lang.dtype_of_literal lit :> Lang.data_type)
        |> TypedefMap.data_type_size typedefs macro_defs;
      is_const = true;
    }

  (* TODO: error handling *)
  let new_binary id thread_id _typedefs (_macro_defs: Lang.macro_def list) binop w1 w2 =
    let sz1 = w1.size
    and sz2 = w2.size in
    let sz = let open Lang in match binop with
    | Add | Sub | Xor | And | Or | Mul ->
      (* TODO: performance improvement *)
      Some (max sz1 sz2)
      (* if sz1 = sz2 then
        Some sz1
      else None *)
    | Lt | Gt | Lte | Gte | Eq | Neq -> Some 1
    | Shl | Shr -> Some sz1 in
    {
      id;
      thread_id;
      source = Binary (binop, w1, w2);
      size = Option.get sz;
      is_const = w1.is_const && w2.is_const;
    }

  let new_unary id thread_id _typedefs unop ow =
    {
      id;
      thread_id;
      source = Unary (unop, ow);
      size = ow.size;
      is_const = ow.is_const;
    }

  let new_switch id thread_id _typedefs sw def =
    let is_const =
      List.for_all (fun (c, v) -> c.is_const && v.is_const) sw
      && def.is_const in
    {
      id;
      thread_id;
      source = Switch (sw, def);
      size = def.size;
      is_const;
    }

  let new_cases id thread_id _typedefs v sw def =
    {
      id;
      thread_id;
      source = Cases (v, sw, def);
      size = def.size;
      is_const = false;
    }

  let new_reg_read id thread_id typedefs (macro_defs: Lang.macro_def list) (r : Lang.reg_def) =
    {
      id;
      thread_id;
      source = RegRead r.name;
      size = TypedefMap.data_type_size typedefs macro_defs r.dtype;
      is_const = false;
    }

  let new_concat id thread_id _typedefs _macro_defs ws =
    let sz = List.fold_left (fun sum w -> sum + w.size) 0 ws in
    let is_const = List.for_all (fun w -> w.is_const) ws in
    {
      id;
      thread_id;
      source = Concat ws;
      size = sz;
      is_const;
    }

  let new_list id thread_id _typedefs ws =
    if ws = [] then
      None (* empty list not allowed *)
    else (
      let hw = List.hd ws in
      let is_const = List.for_all (fun w -> w.is_const) ws in
      if List.for_all (fun w -> w.size = hw.size) (List.tl ws) then (
        Some {
          id;
          thread_id;
          source = Concat (List.rev ws);
          size = hw.size * (List.length ws);
          is_const;
        }
      ) else
        None
    )

  let new_msg_port id thread_id typedefs (macro_defs: Lang.macro_def list) msg_spec idx msg_def =
    let open Lang in
    let t = List.nth msg_def.sig_types idx in
    {
      id;
      thread_id;
      source = MessagePort (msg_spec, idx);
      size = TypedefMap.data_type_size typedefs macro_defs t.dtype;
      is_const = false;
    }

  let new_slice id thread_id w base_i len =
    let is_const =
      w.is_const
      &&
        match base_i with
        | MaybeConst.Const _ -> true
        | MaybeConst.NonConst _ -> false
    in
    {
      id;
      thread_id;
      source = Slice (w, base_i, len);
      size = len;
      is_const;
    }

  let new_msg_valid_port id thread_id _typedefs msg_spec =
    {
      id;
      thread_id;
      source = MessageValidPort msg_spec;
      size = 1;
      is_const = false;
    }
end

type wire = Wire.t

type t = {
  wire_li : wire list;
  wire_last_id : int;
}

let empty : t = { wire_li = []; wire_last_id = -1 }

let add_wire wc w = { wire_li = w::wc.wire_li; wire_last_id = w.id }

let add_literal thread_id (typedefs : TypedefMap.t) (macro_defs: Lang.macro_def list) (lit : Lang.literal) (wc : t) : t * wire =
  let id = wc.wire_last_id + 1 in
  let w = Wire.new_literal id thread_id typedefs macro_defs lit in
  (add_wire wc w, w)

let add_binary thread_id (typedefs : TypedefMap.t) (macro_defs: Lang.macro_def list) (op : Lang.binop)
              (w1 : wire) (w2 : wire) (wc : t) : t * wire =
  let id = wc.wire_last_id + 1 in
  let w = Wire.new_binary id thread_id typedefs macro_defs op w1 w2 in
  (add_wire wc w, w)

let add_unary thread_id (typedefs : TypedefMap.t) (op : Lang.unop)
              (ow : wire) (wc : t) : t * wire =
  let id = wc.wire_last_id + 1 in
  let w = Wire.new_unary id thread_id typedefs op ow in
  (add_wire wc w, w)

let add_switch thread_id (typedefs : TypedefMap.t) (sw : (wire * wire) list)
              (default : wire) (wc : t) : t * wire =
  let id = wc.wire_last_id + 1 in
  let w = Wire.new_switch id thread_id typedefs sw default in
  (add_wire wc w, w)

let add_cases thread_id (typedefs : TypedefMap.t) (v : wire) (sw : (wire * wire) list)
              (default : wire) (wc : t) : t * wire =
  let id = wc.wire_last_id + 1 in
  let w = Wire.new_cases id thread_id typedefs v sw default in
  (add_wire wc w, w)

let add_reg_read thread_id (typedefs : TypedefMap.t) (macro_defs: Lang.macro_def list) (r : Lang.reg_def) (wc : t) : t * wire =
  let id = wc.wire_last_id + 1 in
  let w = Wire.new_reg_read id thread_id typedefs macro_defs r in
  (add_wire wc w, w)

let add_concat thread_id (typedefs : TypedefMap.t) macro_defs (ws : wire list) (wc : t) : t * wire =
  let id = wc.wire_last_id + 1 in
  let w = Wire.new_concat id thread_id typedefs macro_defs ws in
  (add_wire wc w, w)

let add_msg_port thread_id (typedefs : TypedefMap.t) (macro_defs: Lang.macro_def list)
  (msg_spec : Lang.message_specifier) (idx : int) (msg_def : Lang.message_def) (wc : t) : t * wire =
  let id = wc.wire_last_id + 1 in
  let w = Wire.new_msg_port id thread_id typedefs macro_defs msg_spec idx msg_def in
  (add_wire wc w, w)

let add_slice thread_id  (w : wire) base_i len (wc : t) : t * wire =
  let id = wc.wire_last_id + 1 in
  let w = Wire.new_slice id thread_id w base_i len in
  (add_wire wc w, w)

let add_msg_valid_port thread_id (typedefs : TypedefMap.t)
  (msg_spec : Lang.message_specifier) (wc : t) : t * wire =
  let id = wc.wire_last_id + 1 in
  let w = Wire.new_msg_valid_port id thread_id typedefs msg_spec in
  (add_wire wc w, w)

let add_list thread_id (typedefs : TypedefMap.t) (ws : wire list) (wc : t) : (t * wire) option =
  let id = wc.wire_last_id + 1 in
  Wire.new_list id thread_id typedefs ws
  |> Option.map (fun w -> (add_wire wc w, w))
