open Lang
open Except
(* Map with keys as string *)
type t = type_def Utils.string_map
(* Empty map *)
let empty : t = Utils.StringMap.empty
let of_list typedef_list =
  List.to_seq typedef_list |> Seq.map (fun (x : type_def) -> (x.name, x))
    |> Utils.StringMap.of_seq

let data_type_name_resolve (type_defs : t) (dtype : data_type) : data_type option =
  match dtype with
  | `Named (type_name, params) -> Utils.StringMap.find_opt type_name type_defs |> Option.map (fun (x : type_def) ->
      ParamConcretise.concretise_dtype x.params params x.body)
  | _ -> Some dtype

let rec data_type_size (type_defs : t) (macro_defs : macro_def list) (dtype : data_type) : int =
  match dtype with
  | `Logic -> 1
  | `Array (dtype', n) ->
      let n_concrete = match n with
        | ParamEnv.Concrete value -> value
        | ParamEnv.Param param_name ->
            match List.find_opt (fun (m: macro_def) -> m.id = param_name) macro_defs with
            | Some macro -> macro.value
            | None ->
                match ParamEnv.get_opt (Param param_name) with
                | Some value -> value
                | None -> failwith ("Unknown parameter " ^ param_name)
      in
      (data_type_size type_defs macro_defs dtype') * n_concrete
  | `Named (type_name, params) ->
      let type_def = match Utils.StringMap.find_opt type_name type_defs with
      | Some type_def -> type_def
      | None -> failwith("Unknown Datatype: "^type_name)
      in
      ParamConcretise.concretise_dtype type_def.params params type_def.body |> data_type_size type_defs macro_defs
  | `Variant vlist as var ->
      let mx_data_size = List.fold_left (fun m n -> max m (
        let inner_dtype_op = snd n in
        match inner_dtype_op with
        | None -> 0
        | Some inner_dtype -> data_type_size type_defs macro_defs inner_dtype
      )) 0 vlist
      and tag_size = variant_tag_size var in
      mx_data_size + tag_size
  | `Record flist ->
      List.fold_left (fun m n -> m + (snd n |> data_type_size type_defs macro_defs)) 0 flist
  | `Tuple comp_dtype_list ->
      List.fold_left (fun m n -> m + (data_type_size type_defs macro_defs n)) 0 comp_dtype_list
  | `Opaque _ -> raise (TypeError [Text "Opaque data type is unsized!"])


let data_type_indirect (type_defs : t) (macro_defs: macro_def list) (dtype : data_type) (fieldname : identifier) : (int * int * data_type) option =
  let ( let* ) = Option.bind in
  let* dtype' = data_type_name_resolve type_defs dtype in
  match dtype' with
  | `Record flist ->
      (* find the field by fieldname *)
      let found : data_type option ref = ref None
      and offset = ref 0 in
      let lookup = fun ((field, field_type) : identifier * data_type) ->
        if Option.is_none !found then begin
          if field = fieldname then
            found := Some field_type
          else
            offset := !offset + (data_type_size type_defs macro_defs field_type)
        end else ()
      in
      List.iter lookup flist;
      let* found_d = !found in
      Some (!offset, data_type_size type_defs macro_defs found_d, found_d)
  | _ -> None

let data_type_index (type_defs : t) (macro_defs : macro_def list) (expr_eval : expr_node -> 'a)
    (mul : int -> 'a -> 'a)
    (dtype : data_type) (ind : index) : ('a MaybeConst.maybe_int_const * int * data_type) option =
  let ( let* ) = Option.bind in
  let* dtype' = data_type_name_resolve type_defs dtype in
  let get_offset e =
    let open MaybeConst in
    match e.d with
    | Literal lit -> Const (literal_eval lit)
    | _ -> NonConst (expr_eval e)
  in
  match dtype' with
  | `Array (base_type, _n) ->
      let base_size = data_type_size type_defs macro_defs base_type in
      begin
        match ind with
        | Single e ->
            let idx = get_offset e in
            let le_off = MaybeConst.mul_const base_size mul idx in
            Some (le_off, base_size, base_type)
        | Range (e, {d = Literal lit_sz; _}) -> (* size part of the range must be constant *)
            let idx = get_offset e in
            let le_off = MaybeConst.mul_const base_size mul idx in
            let sz = literal_eval lit_sz in
            (* TODO: bounds check for the const case *)
            Some (le_off, base_size * sz, `Array (base_type, Concrete sz))
        | Range _ -> None
      end
  | _ -> None

let rec type_is_integral (type_defs : t) (dtype : data_type) : bool =
  let dtype_resolved = data_type_name_resolve type_defs dtype |> Option.get in
  match dtype_resolved with
  | `Logic -> true
  | `Array (dtype', _) -> type_is_integral type_defs dtype'
  | _ -> false

let type_check_binop (type_defs : t) binop dtype1 dtype2 =
  let dtype1_resolved = data_type_name_resolve type_defs dtype1 |> Option.get
  and dtype2_resolved = data_type_name_resolve type_defs dtype2 |> Option.get in
  (* only integral types can be used here *)
  if (not @@ type_is_integral type_defs dtype1_resolved)
    || (not @@ type_is_integral type_defs dtype2_resolved) then
    None
  else
  match binop with
  | Add | Sub | Xor | And | Or | Mul ->
    (* TODO: performance improvement *)
    Some (max dtype1_resolved dtype2_resolved)
    (* if dtype1_resolved = dtype2_resolved then
      Some dtype1_resolved
    else None *)
  | Lt | Gt | Lte | Gte | Eq | Neq -> Some `Logic
  | Shl | Shr -> Some dtype1_resolved
