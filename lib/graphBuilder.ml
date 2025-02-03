open EventGraph
open Lang

let unwrap_or_err err_msg err_span opt =
  match opt with
  | Some d -> d
  | None -> raise (EventGraphError (err_msg, err_span))


module Typing = struct
  type context = timed_data Utils.string_map

  type build_context = {
    typing_ctx : context;
    current : event;
    shared_vars_info : (identifier, shared_var_info) Hashtbl.t;
    lt_check_phase : bool;
  }

  let event_create g source =
    let event_create_inner () =
      let n = {actions = []; sustained_actions = []; source; id = List.length g.events;
        control_endps = Utils.StringMap.empty;
        current_endps = Utils.StringMap.empty;
        outs = []; graph = g} in
      g.events <- n::g.events;
      (
        match source with
        | `Later (e1, e2)
        | `Either (e1, e2) ->
          e1.outs <- n::e1.outs;
          e2.outs <- n::e2.outs
        | `Branch (_, ev')
        | `Seq (ev', _) ->
          ev'.outs <- n::ev'.outs
        | _ -> ()
      );
      n
    in
    (
      match source with
      | `Later (e1, e2) -> (
        if GraphAnalysis.event_is_successor e1 e2 then
          e2
        else if GraphAnalysis.event_is_predecessor e1 e2 then
          e1
        else
          event_create_inner ()
      )
      | _ -> event_create_inner ()
    )


  let lifetime_intersect g (a : lifetime) (b : lifetime) =
    {
      live = event_create g (`Later (a.live, b.live));
      dead = a.dead @ b.dead;
    }

  let cycles_data g (n : int) (current : event) =
    let live_event = event_create g (`Seq (current, `Cycles n)) in
    {w = None; lt = {live = live_event; dead = [(live_event, `Eternal)]}; reg_borrows = []; dtype = unit_dtype}
  let sync_data g (current : event) (td: timed_data) =
    let ev = event_create g (`Later (current, td.lt.live)) in
    {td with lt = {td.lt with live = ev}}

  let const_data _g (w : wire option) dtype (current : event) = {w; lt = lifetime_const current; reg_borrows = []; dtype}
  let merged_data g (w : wire option) dtype (current : event) (tds : timed_data list) =
    let lts = List.map (fun x -> x.lt) tds in
    match lts with
    | [] -> const_data g w dtype current
    | lt::lts' ->
      let lt' = List.fold_left (lifetime_intersect g) lt lts' in
      let reg_borrows' = List.concat_map (fun x -> x.reg_borrows) tds in
      {w; lt = lt'; reg_borrows = reg_borrows'; dtype}
  let derived_data (w : wire option) (td : timed_data) = {td with w}
  let send_msg_data g (msg : message_specifier) (current : event) =
    let live_event = event_create g (`Seq (current, `Send msg)) in
    {w = None; lt = {live = live_event; dead = [(live_event, `Eternal)]}; reg_borrows = []; dtype = unit_dtype}

  let sync_event_data g ident gtd current =
    let event_synced = event_create g (`Seq (current, `Sync ident)) in
    let dpat = gtd.glt.e in
    (
      match dpat with
      | `Cycles _ -> ()
      | _ -> raise (Except.UnimplementedError "Non-static lifetime for shared data is unsupported!")
    );
    {w = gtd.w; lt = {live = event_synced; dead = [(event_synced, dpat)]}; reg_borrows = []; dtype = gtd.gdtype}

  let recv_msg_data g (w : wire option) (msg : message_specifier) (msg_def : message_def) (current : event) =
    let event_received = event_create g (`Seq (current, `Recv msg)) in
    let stype = List.hd msg_def.sig_types in
    let e = delay_pat_globalise msg.endpoint stype.lifetime.e in
    {w; lt = {live = event_received; dead = [(event_received, e)]}; reg_borrows = []; dtype = stype.dtype}

  let context_add (ctx : context) (v : identifier) (d : timed_data) : context =
    Utils.StringMap.add v d ctx
  let context_empty : context = Utils.StringMap.empty
  let context_lookup (ctx : context) (v : identifier) = Utils.StringMap.find_opt v ctx
  (* checks if lt lives at least as long as required *)

  module BuildContext = struct
    type t = build_context
    let create_empty g si lt_check_phase: t = {
      typing_ctx = context_empty;
      current = event_create g `Root;
      shared_vars_info = si;
      lt_check_phase;
    }

    let clear_bindings (ctx : t) : t =
      {ctx with typing_ctx = context_empty}
    let add_binding (ctx : t) (v : identifier) (d : timed_data) : t =
      {ctx with typing_ctx = context_add ctx.typing_ctx v d}
    let wait g (ctx : t) (other : event) : t =
      {ctx with current = event_create g (`Later (ctx.current, other))}
    let branch g (ctx : t) (td : timed_data) neg: t  =
      {ctx with current = event_create g (`Branch ({data = td; neg}, ctx.current))}
  end


end

type build_context = Typing.build_context
module BuildContext = Typing.BuildContext


let binop_td_const graph (ci:cunit_info) _ctx span op n td =
  let w = unwrap_or_err "Invalid value" span td.w in
  let sz = TypedefMap.data_type_size ci.typedefs ci.macro_defs td.dtype in
  let sz' = match op with
  | Add -> sz+1
  | Mul -> sz + Utils.int_log2 n
  | _ -> sz in
  let (wires', w') = if sz' > sz then
    let (wires', padding) = WireCollection.add_literal graph.thread_id
      ci.typedefs ci.macro_defs (Lang.WithLength (sz' - sz, 0)) graph.wires in
    WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [padding; w] wires'
  else (graph.wires, w) in
  let (wires'', wconst) = WireCollection.add_literal graph.thread_id
     ci.typedefs ci.macro_defs (WithLength (sz', n)) wires' in
  let (wires''', wres) = WireCollection.add_binary graph.thread_id ci.typedefs ci.macro_defs op w' wconst wires'' in
  graph.wires <- wires''';
  {td with w = Some wres; dtype = `Array (`Logic, ParamEnv.Concrete sz')}


let binop_td_td graph (ci:cunit_info) ctx span op td1 td2 =
  let w1 = unwrap_or_err "Invalid value" span td1.w in
  let w2 = unwrap_or_err "Invalid value" span td2.w in
  let sz1 = TypedefMap.data_type_size ci.typedefs ci.macro_defs td1.dtype
  and sz2 = TypedefMap.data_type_size ci.typedefs ci.macro_defs td2.dtype in
  let sz' = match op with
  | Add -> (max sz1 sz2) +1
  | Mul -> sz1 + sz2
  | _ -> max sz1 sz2 in
  let (wires', w1') = if sz' > sz1 then
    let (wires', padding) = WireCollection.add_literal graph.thread_id
    ci.typedefs ci.macro_defs (Lang.WithLength (sz' - sz1, 0)) graph.wires in
    WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [padding; w1] wires'
  else (graph.wires, w1) in
  let (wires', w2') = if sz' > sz2 then
    let (wires', padding) = WireCollection.add_literal graph.thread_id
    ci.typedefs ci.macro_defs (Lang.WithLength (sz' - sz2, 0)) wires' in
    WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [padding; w2] wires'
  else (wires', w2) in
  let (wires', wres) = WireCollection.add_binary graph.thread_id ci.typedefs ci.macro_defs op w1' w2' wires' in
  graph.wires <- wires';
  let open Typing in
  let new_dtype = (`Array (`Logic, ParamEnv.Concrete sz')) in
  Typing.merged_data graph (Some wres) new_dtype ctx.current [td1; td2]

let rec lvalue_info_of graph (ci:cunit_info) ctx span lval =
  let binop_td_const = binop_td_const graph ci ctx span
  and binop_td_td = binop_td_td graph ci ctx span in
  match lval with
  | Reg ident ->
    let r = List.find_opt (fun (r : reg_def) -> r.name = ident) graph.regs
      |> unwrap_or_err ("Undefined register " ^ ident) span in
    let sz = TypedefMap.data_type_size ci.typedefs ci.macro_defs r.dtype in
    {
      lval_range = full_reg_range ident sz;
      lval_dtype = r.dtype
    }
  | Indexed (lval', idx) ->
    (* TODO: better code reuse *)
    let lval_info' = lvalue_info_of graph ci ctx span lval' in
    let (le', _len') = lval_info'.lval_range.subreg_range_interval in
    let (le, len, dtype) =
      TypedefMap.data_type_index ci.typedefs ci.macro_defs
        (visit_expr graph ci ctx)
        (binop_td_const Mul)
        lval_info'.lval_dtype idx
      |> unwrap_or_err "Invalid lvalue indexing" span in
    let le_n = MaybeConst.add (binop_td_const Add) (binop_td_td Add) le' le
    in
    {
      lval_range = {lval_info'.lval_range with subreg_range_interval = (le_n, len)};
      lval_dtype = dtype
    }
  | Indirected (lval', fieldname) ->
    let lval_info' = lvalue_info_of graph ci ctx span lval' in
    let (le', _len') = lval_info'.lval_range.subreg_range_interval in
    let (le, len, dtype) = TypedefMap.data_type_indirect ci.typedefs ci.macro_defs lval_info'.lval_dtype fieldname
      |> unwrap_or_err ("Invalid lvalue indirection through field " ^ fieldname) span in
    let le_n = MaybeConst.add_const le (binop_td_const Add) le'
    in
    {
      lval_range = {lval_info'.lval_range with subreg_range_interval = (le_n, len)};
      lval_dtype = dtype
    }
and visit_expr (graph : event_graph) (ci : cunit_info)
                    (ctx : build_context) (e : expr_node) : timed_data =
  let binop_td_const = binop_td_const graph ci ctx
  and _binop_td_td = binop_td_td graph ci ctx in
  match e.d with
  | Literal lit ->
    let (wires', w) = WireCollection.add_literal graph.thread_id ci.typedefs ci.macro_defs lit graph.wires in
    graph.wires <- wires';
    Typing.const_data graph (Some w) (dtype_of_literal lit :> data_type) ctx.current
  | Sync ident ->
    (
      let shared_info = Hashtbl.find_opt ctx.shared_vars_info ident
        |> unwrap_or_err ("Undefined identifier: " ^ ident) e.span in
      Typing.sync_event_data graph ident shared_info.value ctx.current
    )
  | Identifier ident ->
      let ctx_val = Typing.context_lookup ctx.typing_ctx ident  in
      let macro_val = List.assoc_opt ident (List.map (fun (macro : macro_def) ->(macro.id, macro.value)) ci.macro_defs) in
      (match ctx_val, macro_val with
        | Some _, Some _ ->
          raise (EventGraphError (" Conflicting Identifier " ^ ident ^ " declarations found", e.span))
        | Some td, None -> Typing.sync_data graph ctx.current td
        | None, Some value ->
          let sz = Utils.int_log2 (value + 1) in
          let (wires', w) = WireCollection.add_literal graph.thread_id
              ci.typedefs ci.macro_defs
              (WithLength (sz, value)) graph.wires in
            graph.wires <- wires';
            Typing.const_data graph (Some w) (`Array (`Logic, ParamEnv.Concrete sz)) ctx.current
        | None, None ->
          Typing.context_lookup ctx.typing_ctx ident
          |> unwrap_or_err ("Undefined identifier: " ^ ident) e.span
          |> Typing.sync_data graph ctx.current
      )

  | Assign (lval, e') ->
    let td = visit_expr graph ci ctx e' in
    let lvi = lvalue_info_of graph ci ctx e.span lval in
    ctx.current.actions <- (RegAssign (lvi, td) |> tag_with_span e.span)::ctx.current.actions;
    Typing.cycles_data graph 1 ctx.current
  | Call (id, arg_list) ->
      let func = List.find_opt (fun (f: Lang.func_def) -> f.name = id) ci.func_defs
        |> unwrap_or_err ("Undefined function: " ^ id) e.span in
      let td_args = List.map (visit_expr graph ci ctx) arg_list in
      let ctx' = BuildContext.clear_bindings ctx |> ref in
      List.iter2 (fun td name ->
        ctx' := BuildContext.add_binding !ctx' name td
      ) td_args func.args;
      visit_expr graph ci !ctx' func.body
  | Binop (binop, e1, e2) ->
    let td1 = visit_expr graph ci ctx e1
    and td2 = visit_expr graph ci ctx e2 in
    let w1 = unwrap_or_err "Invalid value" e1.span td1.w
    and w2 = unwrap_or_err "Invalid value" e2.span td2.w in
    let (wires', w) = WireCollection.add_binary graph.thread_id ci.typedefs ci.macro_defs binop w1 w2 graph.wires in
    graph.wires <- wires';
    let new_dtype = `Array (`Logic, ParamEnv.Concrete w.size) in
    Typing.merged_data graph (Some w) new_dtype ctx.current [td1; td2]
  | Unop (unop, e') ->
    let td = visit_expr graph ci ctx e' in
    let w' = unwrap_or_err "Invalid value" e'.span td.w in
    let (wires', w) = WireCollection.add_unary graph.thread_id ci.typedefs unop w' graph.wires in
    graph.wires <- wires';
    Typing.derived_data (Some w) td
  | Tuple [] -> Typing.const_data graph None (unit_dtype) ctx.current
  | LetIn (idents, e1, e2) ->
    let td1 = visit_expr graph ci ctx e1 in
    (
      match idents, td1.w with
      | [], None | ["_"], _ ->
        let td = visit_expr graph ci ctx e2 in
        let lt = Typing.lifetime_intersect graph td1.lt td.lt in
        {td with lt} (* forcing the bound value to be awaited *)
      | [ident], _ ->
        let ctx' = BuildContext.add_binding ctx ident td1 in
        visit_expr graph ci ctx' e2
      | _ -> raise (EventGraphError ("Discarding expression results!", e.span))
    )
  | Wait (e1, e2) ->
    let td1 = visit_expr graph ci ctx e1 in
    let ctx' = BuildContext.wait graph ctx td1.lt.live in
    visit_expr graph ci ctx' e2
  | Ready msg_spec ->
    let wires, msg_valid_port = WireCollection.add_msg_valid_port graph.thread_id ci.typedefs msg_spec graph.wires in
    graph.wires <- wires;
    Typing.const_data graph (Some msg_valid_port) `Logic ctx.current
  | Cycle n -> Typing.cycles_data graph n ctx.current
  | IfExpr (e1, e2, e3) ->
    let td1 = visit_expr graph ci ctx e1 in
    (* TODO: type checking *)
    let w1 = unwrap_or_err "Invalid condition" e1.span td1.w in
    let ctx' = BuildContext.wait graph ctx td1.lt.live in
    let ctx_true = BuildContext.branch graph ctx' td1 false in
    let td2 = visit_expr graph ci ctx_true e2 in
    let ctx_false = BuildContext.branch graph ctx' td1 true in
    let td3 = visit_expr graph ci ctx_false e3 in
    let lt = {live = Typing.event_create graph (`Either (td2.lt.live, td3.lt.live));
      dead = td1.lt.dead @ td2.lt.dead @td3.lt.dead} in
    let reg_borrows' = td1.reg_borrows @ td2.reg_borrows @ td3.reg_borrows in
    (
      match td2.w, td3.w with
      | None, None -> {w = None; lt; reg_borrows = reg_borrows'; dtype = unit_dtype}
      | Some w2, Some w3 ->
      (* TODO: check that the data types are the same *)
        let (wires', w) = WireCollection.add_switch graph.thread_id ci.typedefs [(w1, w2)] w3 graph.wires in
        graph.wires <- wires';
        {w = Some w; lt; reg_borrows = reg_borrows'; dtype = td2.dtype}
      | _ -> raise (EventGraphError ("Invalid if expression!", e.span))
    )
  | Concat es ->
    let tds = List.map (fun e' -> (e', visit_expr graph ci ctx e')) es in
    let ws = List.map (fun (e', td) -> unwrap_or_err "Invalid value in concat" e'.span td.w) tds in
    let (wires', w) = WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs ws graph.wires in
    graph.wires <- wires';
    let new_dtype = `Array (`Logic, ParamEnv.Concrete w.size) in
    List.map snd tds |> Typing.merged_data graph (Some w) new_dtype ctx.current
  | Read reg_ident ->
    let r = List.find_opt (fun (r : Lang.reg_def) -> r.name = reg_ident) graph.regs
      |> unwrap_or_err ("Undefined register " ^ reg_ident) e.span in
    let (wires', w) = WireCollection.add_reg_read graph.thread_id ci.typedefs ci.macro_defs r graph.wires in
    graph.wires <- wires';
    let sz = TypedefMap.data_type_size ci.typedefs ci.macro_defs r.dtype in
    let borrow = {borrow_range = full_reg_range reg_ident sz; borrow_start = ctx.current} in
    {w = Some w; lt = EventGraph.lifetime_const ctx.current; reg_borrows = [borrow]; dtype = r.dtype}
  | Debug op ->
    (
      match op with
      | DebugPrint (s, e_list) ->
        let timed_ws = List.map (visit_expr graph ci ctx) e_list in
        ctx.current.actions <- (let open EventGraph in DebugPrint (s, timed_ws) |> tag_with_span e.span)::ctx.current.actions;
        {w = None; lt = EventGraph.lifetime_const ctx.current; reg_borrows = []; dtype = unit_dtype}
      | DebugFinish ->
        ctx.current.actions <- (let open EventGraph in tag_with_span e.span DebugFinish)::ctx.current.actions;
        {w = None; lt = EventGraph.lifetime_const ctx.current; reg_borrows = []; dtype = unit_dtype}
    )
  | Send send_pack ->
    (* just check that the endpoint and the message type is defined *)
    let _ = MessageCollection.lookup_message graph.messages send_pack.send_msg_spec ci.channel_classes
      |> unwrap_or_err "Invalid message specifier in send" e.span in
    let td = visit_expr graph ci ctx send_pack.send_data in
    let ntd = Typing.send_msg_data graph send_pack.send_msg_spec ctx.current in
    ctx.current.sustained_actions <-
      ({
        until = ntd.lt.live;
        ty = Send (send_pack.send_msg_spec, td)
      } |> tag_with_span e.span)::ctx.current.sustained_actions;
    ntd
  | Recv recv_pack ->
    let msg = MessageCollection.lookup_message graph.messages recv_pack.recv_msg_spec ci.channel_classes
      |> unwrap_or_err "Invalid message specifier in receive" e.span in
    let (wires', w) = WireCollection.add_msg_port graph.thread_id ci.typedefs ci.macro_defs recv_pack.recv_msg_spec 0 msg graph.wires in
    graph.wires <- wires';
    let ntd = Typing.recv_msg_data graph (Some w) recv_pack.recv_msg_spec msg ctx.current in
    ctx.current.sustained_actions <-
      ({until = ntd.lt.live; ty = Recv recv_pack.recv_msg_spec} |> tag_with_span e.span)::ctx.current.sustained_actions;
    ntd
  | Indirect (e', fieldname) ->
    let td = visit_expr graph ci ctx e' in
    let w = unwrap_or_err "Invalid value in indirection" e'.span td.w in
    let (offset_le, len, new_dtype) = TypedefMap.data_type_indirect ci.typedefs ci.macro_defs td.dtype fieldname
      |> unwrap_or_err "Invalid indirection" e.span in
    let (wires', new_w) = WireCollection.add_slice graph.thread_id w (Const offset_le) len graph.wires in
    graph.wires <- wires';
    {
      td with
      w = Some new_w;
      dtype = new_dtype
    }
  | EnumRef (id, variant) ->
    let enum_variants = List.assoc_opt id ci.enum_mappings
      |> unwrap_or_err ("Undefined enum type: " ^ id) e.span in
    let variant_idx = List.find_map (fun (v, idx) -> if v = variant then Some idx else None) enum_variants
      |> unwrap_or_err ("Invalid enum variant " ^ variant ^ " for enum " ^ id) e.span in
    let sz = Utils.int_log2 (List.length enum_variants) in
    let (wires', w) = WireCollection.add_literal graph.thread_id ci.typedefs ci.macro_defs
      (WithLength (sz, variant_idx)) graph.wires in
    graph.wires <- wires';
    Typing.const_data graph (Some w) (`Array (`Logic, ParamEnv.Concrete sz)) ctx.current
  | Index (e', ind) ->
    let td = visit_expr graph ci ctx e' in
    let w = unwrap_or_err "Invalid value in indexing" e'.span td.w in
    let (offset_le, len, new_dtype) =
      TypedefMap.data_type_index ci.typedefs ci.macro_defs
        (visit_expr graph ci ctx)
        (binop_td_const e.span Mul)
        td.dtype ind
      |> unwrap_or_err "Invalid indexing" e.span in
    let wire_of td = unwrap_or_err "Invalid indexing" e.span td.w in
    let offset_le_w = MaybeConst.map wire_of offset_le in
    let (wires', new_w) = WireCollection.add_slice graph.thread_id w offset_le_w len graph.wires in
    graph.wires <- wires';
    (
      match offset_le with
      | Const _ -> {td with w = Some new_w; dtype = new_dtype}
      | NonConst td_offset ->
          Typing.merged_data graph (Some new_w) new_dtype ctx.current [td; td_offset]
    )
  | Record (record_ty_name, field_exprs) ->
    (
      match TypedefMap.data_type_name_resolve ci.typedefs @@ `Named (record_ty_name, []) with
      | Some (`Record record_fields) ->
        (
          match Utils.list_match_reorder (List.map fst record_fields) field_exprs with
          | Some expr_reordered ->
            let tds = List.map (fun e' -> (e', visit_expr graph ci ctx e')) expr_reordered in
            let ws = List.rev_map (fun ((e', {w; _}) : expr_node * timed_data) ->
              unwrap_or_err "Invalid value in record field" e'.span w) tds in
            let (wires', w) = WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs ws graph.wires in
            graph.wires <- wires';
            List.map snd tds |> Typing.merged_data graph (Some w) (`Named (record_ty_name, [])) ctx.current
          | _ -> raise (EventGraphError ("Invalid record type value!", e.span))
        )
      | _ -> raise (EventGraphError ("Invalid record type name!", e.span))
    )
  | Construct (cstr_spec, cstr_expr_opt) ->
    (
      match TypedefMap.data_type_name_resolve ci.typedefs @@ `Named (cstr_spec.variant_ty_name, []) with
      | Some (`Variant _ as dtype) ->
        let e_dtype_opt = variant_lookup_dtype dtype cstr_spec.variant in
        (
          match e_dtype_opt, cstr_expr_opt with
          | Some e_dtype, Some cstr_expr ->
            let td = visit_expr graph ci ctx cstr_expr in
            let w = unwrap_or_err "Invalid value in variant construction" cstr_expr.span td.w in
            let tag_size = variant_tag_size dtype
            and data_size = TypedefMap.data_type_size ci.typedefs ci.macro_defs e_dtype
            and tot_size = TypedefMap.data_type_size ci.typedefs ci.macro_defs dtype
            and var_idx = variant_lookup_index dtype cstr_spec.variant
              |> unwrap_or_err ("Invalid constructor: " ^ cstr_spec.variant) e.span in
            let (wires', w_tag) = WireCollection.add_literal graph.thread_id ci.typedefs ci.macro_defs
              (WithLength (tag_size, var_idx)) graph.wires in
            let (wires', new_w) = if tot_size = tag_size + data_size then
              (* no padding *)
              WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [w; w_tag] wires'
            else begin
              (* padding needed *)
              let (wires', w_pad) = WireCollection.add_literal graph.thread_id ci.typedefs ci.macro_defs
                (WithLength (tot_size - tag_size - data_size, 0)) wires' in
              WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [w_pad; w; w_tag] wires'
            end in
            graph.wires <- wires';
            { td with w = Some new_w }
          | None, None ->
            let tag_size = variant_tag_size dtype
            and tot_size = TypedefMap.data_type_size ci.typedefs ci.macro_defs dtype
            and var_idx = variant_lookup_index dtype cstr_spec.variant
              |> unwrap_or_err ("Invalid constructor: " ^ cstr_spec.variant) e.span in
            let (wires', w_tag) = WireCollection.add_literal graph.thread_id ci.typedefs ci.macro_defs
              (WithLength (tag_size, var_idx)) graph.wires in
            let (wires', new_w) = if tot_size = tag_size then
              (wires', w_tag)
            else begin
              let (wires', w_pad) = WireCollection.add_literal graph.thread_id ci.typedefs ci.macro_defs
                (WithLength (tot_size - tag_size, 0)) wires' in
              WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [w_pad; w_tag] wires'
            end in
            graph.wires <- wires';
            Typing.const_data graph (Some new_w)  (`Named (cstr_spec.variant_ty_name, [])) ctx.current
          | _ -> raise (EventGraphError ("Invalid variant construct expression!", e.span))
        )
      | _ -> raise (EventGraphError ("Invalid variant type name!", e.span))
    )
  | SharedAssign (id, value_expr) ->
    let shared_info = Hashtbl.find_opt ctx.shared_vars_info id
      |> unwrap_or_err ("Undefined identifier " ^ id) e.span in
    if graph.thread_id = shared_info.assigning_thread then
      let value_td = visit_expr graph ci ctx value_expr in
      if not ctx.lt_check_phase then (
        if (Option.is_some shared_info.value.w) || (Option.is_some shared_info.assigned_at) then
          raise (EventGraphError ("Shared value can only be assigned in one place!", e.span));
        shared_info.value.w <- value_td.w;
        shared_info.assigned_at <- Some ctx.current;
      );
      ctx.current.actions <- (PutShared (id, shared_info, value_td) |> tag_with_span e.span)::ctx.current.actions;
      Typing.const_data graph None (unit_dtype) ctx.current
    else
      raise (EventGraphError ("Shared variable assigned in wrong thread", e.span))
  | List li ->
    let tds = List.map (visit_expr graph ci ctx) li in
    let ws = List.map (fun td -> unwrap_or_err "Invalid wires!" e.span td.w) tds in
    let (wires', new_w) = WireCollection.add_list graph.thread_id ci.typedefs ws graph.wires
      |> unwrap_or_err "Invalid list!" e.span in
    graph.wires <- wires';
    let td = List.hd tds in
    Typing.merged_data graph (Some new_w) (`Array (td.dtype, ParamEnv.Concrete (List.length tds))) ctx.current tds
  | _ -> raise (EventGraphError ("Unimplemented expression!", e.span))

(* Builds the graph representation for each process To Do: Add support for commands outside loop (be executed once or continuosly)*)
let build_proc (config : Config.compile_config) sched module_name param_values
              (ci : cunit_info) (proc : proc_def) : proc_graph =
  let proc =
    if param_values = [] then
      proc
    else
      ParamConcretise.concretise_proc param_values proc
  in
  match proc.body with
  | Native body ->
    let msg_collection = MessageCollection.create body.channels proc.args ci.channel_classes in
    let spawns =
    List.map (fun s ->
      let module_name = BuildScheduler.add_proc_task sched s.proc s.compile_params in
      (module_name, s)
    ) body.spawns in
    let shared_vars_info = Hashtbl.create (List.length body.shared_vars) in
    List.iter (fun sv ->
      let v = {
        w = None;
        glt = sv.shared_lifetime;
        gdtype = unit_dtype; (* explicitly annotate? *)
      } in
      let r = {
        assigning_thread = sv.assigning_thread;
        value = v;
        assigned_at = None;
      } in
        Hashtbl.add shared_vars_info sv.ident r
      ) body.shared_vars;
      let proc_threads = List.mapi (fun i e ->
        let graph = {
          thread_id = i;
          events = [];
          wires = WireCollection.empty;
          channels = body.channels;
          messages = msg_collection;
          spawns = body.spawns;
          regs = body.regs;
          last_event_id = 0;
        } in
        (* Bruteforce treatment: just run twice *)
        let graph_opt = if (not config.disable_lt_checks) || config.two_round_graph then (
          let tmp_graph = {graph with last_event_id = 0} in
          let td = visit_expr tmp_graph ci
            (BuildContext.create_empty tmp_graph shared_vars_info true)
            (dummy_ast_node_of_data (Wait (e, e))) in
          tmp_graph.last_event_id <- td.lt.live.id;
          (* Optimisation *)
          let tmp_graph = GraphOpt.optimize config true ci tmp_graph in
          if not config.disable_lt_checks then (
            LifetimeCheck.lifetime_check config ci tmp_graph
          );
          if config.two_round_graph then
            Some tmp_graph
          else
            None
        ) else None in
        match graph_opt with
        | Some graph -> graph
        | None -> (
            (* discard after type checking *)
            let ctx = (BuildContext.create_empty graph shared_vars_info false) in
            let td = visit_expr graph ci ctx e in
              graph.last_event_id <- td.lt.live.id;
            GraphOpt.optimize config false ci graph
        )
      ) body.loops in
      {name = module_name; extern_module = None;
        threads = proc_threads; shared_vars_info; messages = msg_collection;
        proc_body = proc.body; spawns}
    | Extern (extern_mod, _extern_body) ->
      let msg_collection = MessageCollection.create [] proc.args ci.channel_classes in
      {name = module_name; extern_module = Some extern_mod; threads = [];
        shared_vars_info = Hashtbl.create 0; messages = msg_collection;
        proc_body = proc.body; spawns = []}

let build (config : Config.compile_config) sched module_name param_values (cunit : compilation_unit) =
  let enum_mappings = List.map (fun (enum : enum_def) ->
    let variants_with_indices = List.mapi (fun i variant ->
      (variant, i)
    ) enum.variants in
    (enum.name, variants_with_indices)
  ) cunit.enum_defs in

  let macro_defs = cunit.macro_defs in
  let typedefs = TypedefMap.of_list cunit.type_defs in
  let func_defs = cunit.func_defs in
  let ci = {
    typedefs;
    channel_classes = cunit.channel_classes;
    enum_mappings;
    macro_defs;
    func_defs
  } in
  let graphs = List.map (build_proc config sched module_name param_values ci ) cunit.procs in
  {
    event_graphs = graphs;
    typedefs;
    macro_defs;
    channel_classes = cunit.channel_classes;
    external_event_graphs = [];
    enum_mappings;
  }
