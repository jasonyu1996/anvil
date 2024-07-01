(* A wire described results of untimed computation *)


module Wire = struct
  type borrow_source = string

  type t = {
    id: int;
    source: wire_source;
    dtype: Lang.data_type;
    borrow_src: borrow_source list; (** Not actually borrowed. Just the dependency. *)
  }
  and wire_source =
    | Literal of Lang.literal
    | Binary of Lang.binop * t * t
    | Unary of Lang.unop * t
    | Switch of (t * t) list * t (* (cond, val) list, default *)
    | RegRead of Lang.identifier
    | MessagePort of Lang.message_specifier * int (* index of the port *)
    | Concat of t list
    | Slice of t * int * int

  let new_literal id lit =
    {
      id;
      source = Literal lit;
      dtype = (Lang.dtype_of_literal lit :> Lang.data_type);
      borrow_src = [];
    }

  let new_binary id typedefs binop w1 w2 =
    let sz1 = TypedefMap.data_type_size typedefs (w1.dtype :> Lang.data_type)
    and sz2 = TypedefMap.data_type_size typedefs (w2.dtype :> Lang.data_type) in
    let sz = let open Lang in match binop with
    | Add | Sub | Xor | And | Or ->
      (* TODO: performance improvement *)
      if sz1 = sz2 then
        Some sz1
      else None
    | Lt | Gt | Lte | Gte | Eq | Neq -> Some 1
    | Shl | Shr -> Some sz1 in
    {
      id;
      source = Binary (binop, w1, w2);
      dtype = `Array (`Logic, Option.get sz);
      borrow_src = w1.borrow_src @ w2.borrow_src;
    }

  let new_unary id _typedefs unop ow =
    {
      id;
      source = Unary (unop, ow);
      dtype = ow.dtype;
      borrow_src = ow.borrow_src;
    }

  let new_switch id _typedefs sw def =
    {
      id;
      source = Switch (sw, def);
      dtype = def.dtype;
      borrow_src = def.borrow_src @ (List.concat_map (fun (_, x) -> x.borrow_src) sw);
    }

  let new_reg_read id _typedefs (r : Lang.reg_def) =
    {
      id;
      source = RegRead r.name;
      dtype = r.dtype;
      borrow_src = [r.name]
    }

  let new_concat id typedefs ws =
    let sz = List.fold_left (fun sum w -> sum + (TypedefMap.data_type_size typedefs w.dtype)) 0 ws in
    {
      id;
      source = Concat ws;
      dtype = `Array (`Logic, sz);
      borrow_src = List.concat_map (fun w -> w.borrow_src) ws
    }

  let new_msg_port id _typedefs msg_spec idx msg_def =
    let open Lang in
    let t = List.nth msg_def.sig_types idx in
    {
      id;
      source = MessagePort (msg_spec, idx);
      dtype = t.dtype;
      borrow_src = [];
    }

  let new_slice id dtype w base_i end_i =
    {
      id;
      source = Slice (w, base_i, end_i);
      dtype;
      borrow_src = w.borrow_src;
    }
end

type wire = Wire.t

type t = wire list

let empty : t = []

let add_literal (lit : Lang.literal) (wc : t) : t * wire =
  let id = List.length wc in
  let w = Wire.new_literal id lit in
  (w::wc, w)

let add_binary (typedefs : TypedefMap.t) (op : Lang.binop)
              (w1 : wire) (w2 : wire) (wc : t) : t * wire =
  let id = List.length wc in
  let w = Wire.new_binary id typedefs op w1 w2 in
  (w::wc, w)

let add_unary (typedefs : TypedefMap.t) (op : Lang.unop)
              (ow : wire) (wc : t) : t * wire =
  let id = List.length wc in
  let w = Wire.new_unary id typedefs op ow in
  (w::wc, w)

let add_switch (typedefs : TypedefMap.t) (sw : (wire * wire) list)
              (default : wire) (wc : t) : t * wire =
  let id = List.length wc in
  let w = Wire.new_switch id typedefs sw default in
  (w::wc, w)

let add_reg_read (typedefs : TypedefMap.t) (r : Lang.reg_def) (wc : t) : t * wire =
  let id = List.length wc in
  let w = Wire.new_reg_read id typedefs r in
  (w::wc, w)

let add_concat (typedefs : TypedefMap.t) (ws : wire list) (wc : t) : t * wire =
  let id = List.length wc in
  let w = Wire.new_concat id typedefs ws in
  (w::wc, w)

let add_msg_port (typedefs : TypedefMap.t)
  (msg_spec : Lang.message_specifier) (idx : int) (msg_def : Lang.message_def) (wc : t) : t * wire =
  let id = List.length wc in
  let w = Wire.new_msg_port id typedefs msg_spec idx msg_def in
  (w::wc, w)

let add_slice (dtype : Lang.data_type) (w : wire) (base_i : int) (end_i : int) (wc : t) : t * wire =
  let id = List.length wc in
  let w = Wire.new_slice id dtype w base_i end_i in
  (w::wc, w)
