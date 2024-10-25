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
    source: wire_source;
    dtype: Lang.data_type;
  }
  and wire_source =
    | Literal of Lang.literal
    | Binary of Lang.binop * t * t
    | Unary of Lang.unop * t
    | Switch of (t * t) list * t (* (cond, val) list, default *)
    | RegRead of Lang.identifier
    | MessagePort of Lang.message_specifier * int (* index of the port *)
    | Concat of t list
    | Slice of t * t MaybeConst.maybe_int_const * int (** third component is size *)

  let new_literal id thread_id lit =
    {
      id;
      thread_id;
      source = Literal lit;
      dtype = (Lang.dtype_of_literal lit :> Lang.data_type);
    }

  (* TODO: error handling *)
  let new_binary id thread_id typedefs binop w1 w2 =
    let sz1 = TypedefMap.data_type_size typedefs (w1.dtype :> Lang.data_type)
    and sz2 = TypedefMap.data_type_size typedefs (w2.dtype :> Lang.data_type) in
    let sz = let open Lang in match binop with
    | Add | Sub | Xor | And | Or | Mul ->
      (* TODO: performance improvement *)
      if sz1 = sz2 then
        Some sz1
      else None
    | Lt | Gt | Lte | Gte | Eq | Neq -> Some 1
    | Shl | Shr -> Some sz1 in
    {
      id;
      thread_id;
      source = Binary (binop, w1, w2);
      dtype = `Array (`Logic, Option.get sz);
    }

  let new_unary id thread_id _typedefs unop ow =
    {
      id;
      thread_id;
      source = Unary (unop, ow);
      dtype = ow.dtype;
    }

  let new_switch id thread_id _typedefs sw def =
    {
      id;
      thread_id;
      source = Switch (sw, def);
      dtype = def.dtype;
    }

  let new_reg_read id thread_id _typedefs (r : Lang.reg_def) =
    {
      id;
      thread_id;
      source = RegRead r.name;
      dtype = r.dtype;
    }

  let new_concat id thread_id typedefs ws =
    let sz = List.fold_left (fun sum w -> sum + (TypedefMap.data_type_size typedefs w.dtype)) 0 ws in
    {
      id;
      thread_id;
      source = Concat ws;
      dtype = `Array (`Logic, sz);
    }

  let new_list id thread_id _typedefs ws =
    if ws = [] then
      None (* empty list not allowed *)
    else (
      let hw = List.hd ws in
      (* TODO: might be necessary to resolve types *)
      if List.for_all (fun w -> w.dtype = hw.dtype) (List.tl ws) then (
        Some {
          id;
          thread_id;
          source = Concat (List.rev ws);
          dtype = `Array (hw.dtype, List.length ws)
        }
      ) else
        None
    )

  let new_msg_port id thread_id _typedefs msg_spec idx msg_def =
    let open Lang in
    let t = List.nth msg_def.sig_types idx in
    {
      id;
      thread_id;
      source = MessagePort (msg_spec, idx);
      dtype = t.dtype;
    }

  let new_slice id thread_id dtype w base_i len =
    {
      id;
      thread_id;
      source = Slice (w, base_i, len);
      dtype;
    }
end

type wire = Wire.t

type t = wire list

let empty : t = []

let add_literal thread_id (lit : Lang.literal) (wc : t) : t * wire =
  let id = List.length wc in
  let w = Wire.new_literal id thread_id lit in
  (w::wc, w)

let add_binary thread_id (typedefs : TypedefMap.t) (op : Lang.binop)
              (w1 : wire) (w2 : wire) (wc : t) : t * wire =
  let id = List.length wc in
  let w = Wire.new_binary id thread_id typedefs op w1 w2 in
  (w::wc, w)

let add_unary thread_id (typedefs : TypedefMap.t) (op : Lang.unop)
              (ow : wire) (wc : t) : t * wire =
  let id = List.length wc in
  let w = Wire.new_unary id thread_id typedefs op ow in
  (w::wc, w)

let add_switch thread_id (typedefs : TypedefMap.t) (sw : (wire * wire) list)
              (default : wire) (wc : t) : t * wire =
  let id = List.length wc in
  let w = Wire.new_switch id thread_id typedefs sw default in
  (w::wc, w)

let add_reg_read thread_id (typedefs : TypedefMap.t) (r : Lang.reg_def) (wc : t) : t * wire =
  let id = List.length wc in
  let w = Wire.new_reg_read id thread_id typedefs r in
  (w::wc, w)

let add_concat thread_id (typedefs : TypedefMap.t) (ws : wire list) (wc : t) : t * wire =
  let id = List.length wc in
  let w = Wire.new_concat id thread_id typedefs ws in
  (w::wc, w)

let add_msg_port thread_id (typedefs : TypedefMap.t)
  (msg_spec : Lang.message_specifier) (idx : int) (msg_def : Lang.message_def) (wc : t) : t * wire =
  let id = List.length wc in
  let w = Wire.new_msg_port id thread_id typedefs msg_spec idx msg_def in
  (w::wc, w)

let add_slice thread_id (dtype : Lang.data_type) (w : wire)  base_i len (wc : t) : t * wire =
  let id = List.length wc in
  let w = Wire.new_slice id thread_id dtype w base_i len in
  (w::wc, w)

let add_list thread_id (typedefs : TypedefMap.t) (ws : wire list) (wc : t) : (t * wire) option =
  let id = List.length wc in
  Wire.new_list id thread_id typedefs ws
  |> Option.map (fun w -> (w::wc, w))
