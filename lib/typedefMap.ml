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
  | `Named type_name -> Utils.StringMap.find_opt type_name type_defs |> Option.map (fun (x : type_def) -> x.body)
  | _ -> Some dtype

let rec data_type_size (type_defs : t) (dtype : data_type) : int =
  match dtype with
  | `Logic -> 1
  | `Array (dtype', n) -> (data_type_size type_defs dtype') * n
  | `Named type_name ->
      let type_def = Utils.StringMap.find type_name type_defs in
      data_type_size type_defs type_def.body
  | `Variant vlist as var ->
      let mx_data_size = List.fold_left (fun m n -> max m (
        let inner_dtype_op = snd n in
        match inner_dtype_op with
        | None -> 0
        | Some inner_dtype -> data_type_size type_defs inner_dtype
      )) 0 vlist
      and tag_size = variant_tag_size var in
      mx_data_size + tag_size
  | `Record flist ->
      List.fold_left (fun m n -> m + (snd n |> data_type_size type_defs)) 0 flist
  | `Tuple comp_dtype_list ->
      List.fold_left (fun m n -> m + (data_type_size type_defs n)) 0 comp_dtype_list
  | `Opaque _ -> raise (TypeError "Opaque data type is unsized!")


let data_type_indirect (type_defs : t) (dtype : data_type) (fieldname : identifier) : (int * int * data_type) option =
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
            offset := !offset + (data_type_size type_defs field_type)
        end else ()
      in
      List.iter lookup flist;
      let* found_d = !found in
      Some (!offset, !offset + (data_type_size type_defs found_d), found_d)
  | _ -> None

let data_type_index (type_defs : t) (dtype : data_type) (ind : index) : (int * int * data_type) option =
  let ( let* ) = Option.bind in
  let* dtype' = data_type_name_resolve type_defs dtype in
  match dtype' with
  | `Array (base_type, n) ->
      let base_size = data_type_size type_defs base_type in
      begin
        (* TODO: we only support literals as indices for now *)
        match ind with
        | Single {d = Literal lit; _} ->
            let lit_val = literal_eval lit in
            if lit_val < 0 || lit_val >= n then None else
              Some (base_size * lit_val, base_size * (lit_val + 1), base_type)
        | Range ({d = Literal lit_le; _}, {d = Literal lit_ri; _}) ->
            let le = literal_eval lit_le
            and ri = literal_eval lit_ri in
            if le < 0 || ri >= n || le > ri then None else
              Some (base_size * le, base_size * (ri + 1), `Array (base_type, ri - le + 1))
        | _ -> None
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
  | Add | Sub | Xor | And | Or ->
    (* TODO: performance improvement *)
    if dtype1_resolved = dtype2_resolved then
      Some dtype1_resolved
    else None
  | Lt | Gt | Lte | Gte | Eq | Neq -> Some `Logic
  | Shl | Shr -> Some dtype1_resolved
