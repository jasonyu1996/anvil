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
