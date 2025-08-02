open EventGraph
open EventGraphOps
open Lang

let unwrap_or_err err_msg err_span opt =
  match opt with
  | Some d -> d
  | None -> raise (event_graph_error_default err_msg err_span)

module Typing = struct
  type binding = {
    binding_val : timed_data;
    mutable binding_used : bool; (** if the binding has been used (to enforce relevance) *)
  }

  let use_binding binding =
    binding.binding_used <- true;
    binding.binding_val

  type context = binding Utils.string_map

  type build_context = {
    typing_ctx : context;
    current : event;
    shared_vars_info : (identifier, shared_var_info) Hashtbl.t;
    lt_check_phase : bool;
  }

  let event_create g source =
    let event_create_inner () =
      let parents =
        match source with
        | `Root None -> []
        | `Seq (e, _)
        | `Root Some (e, _) -> [e]
        | `Later (e1, e2) -> [e1; e2]
        | `Branch (_, { branches_val; _ }) -> branches_val
      in
      let new_preds = List.fold_left
        (fun preds e -> Utils.IntSet.union preds e.preds)
        Utils.IntSet.empty
        parents
      in
      let new_preds = Utils.IntSet.add (g.last_event_id + 1) new_preds in
      let n = {actions = []; sustained_actions = []; source; id = g.last_event_id + 1;
        is_recurse = false;
        outs = []; graph = g; preds = new_preds } in
      g.events <- n::g.events;
      g.last_event_id <- n.id;
      n
    in
    (
      match source with
      | `Later (e1, e2) -> (
        if Utils.IntSet.mem e1.id e2.preds then
          e2
        else if Utils.IntSet.mem e2.id e1.preds then
          e1
        else
          event_create_inner ()
      )
      | _ -> event_create_inner ()
    )


  let lifetime_intersect g (a : lifetime) (b : lifetime) =
    {
      live = event_create g (`Later (a.live, b.live));
      dead = Utils.list_unordered_join a.dead b.dead;
    }

  let cycles_data g (n : int) (current : event) =
    let live_event = event_create g (`Seq (current, `Cycles n)) in
    {w = None; lt = {live = live_event; dead = [(live_event, `Eternal)]}; reg_borrows = []; dtype = unit_dtype}
  let sync_data g (current : event) (td: timed_data) =
    let ev = event_create g (`Later (current, td.lt.live)) in
    {td with lt = {td.lt with live = ev}}

  let immediate_data _g (w : wire option) dtype (current : event) = {w; lt = lifetime_immediate current; reg_borrows = []; dtype}
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
      | _ -> raise (Except.UnimplementedError [Text "Non-static lifetime for shared data is unsupported!"])
    );
    {w = gtd.w; lt = {live = event_synced; dead = [(event_synced, dpat)]}; reg_borrows = []; dtype = gtd.gdtype}

  let recv_msg_data g (w : wire option) (msg : message_specifier) (msg_def : message_def) (current : event) =
    let event_received = event_create g (`Seq (current, `Recv msg)) in
    let stype = List.hd msg_def.sig_types in
    let e = delay_pat_globalise msg.endpoint stype.lifetime.e in
    {w; lt = {live = event_received; dead = [(event_received, e)]}; reg_borrows = []; dtype = stype.dtype}

  let context_add (ctx : context) (v : identifier) (d : timed_data) : context =
    Utils.StringMap.add v {binding_val = d; binding_used = false} ctx
  let context_empty : context = Utils.StringMap.empty
  let context_lookup (ctx : context) (v : identifier) = Utils.StringMap.find_opt v ctx
  (* checks if lt lives at least as long as required *)

  let context_clear_used =
    Utils.StringMap.map (fun r -> {r with binding_used = false})

  module BuildContext = struct
    type t = build_context
    let create_empty g si lt_check_phase: t = {
      typing_ctx = context_empty;
      current = event_create g (`Root None);
      shared_vars_info = si;
      lt_check_phase;
    }

    let clear_bindings (ctx : t) : t =
      {ctx with typing_ctx = context_empty}
    let add_binding (ctx : t) (v : identifier) (d : timed_data) : t =
      {ctx with typing_ctx = context_add ctx.typing_ctx v d}
    let wait g (ctx : t) (other : event) : t =
      {ctx with current = event_create g (`Later (ctx.current, other))}

    (* returns a pair of contexts for *)
    let branch_side g (ctx : t) (bi : branch_info) (sel : int) : branch_side_info * t  =
      let br_side_info = {branch_event = None; owner_branch = bi; branch_side_sel = sel} in
      let event_side_root = event_create g (`Root (Some (ctx.current, br_side_info))) in
      (br_side_info, {ctx with current = event_side_root; typing_ctx = context_clear_used ctx.typing_ctx})

    let branch g (ctx : t) (br_info : branch_info) : t =
      {ctx with current = event_create g (`Branch (ctx.current, br_info))}

    (* merge the used state in context *)
    let branch_merge ctx br_ctxs =
      let ctx1 = List.hd br_ctxs in
      let other_ctxs = List.tl br_ctxs in
      Utils.StringMap.iter (fun ident r ->
        if r.binding_used
            && (List.for_all
                (fun ctx2 -> (Utils.StringMap.find ident ctx2.typing_ctx).binding_used)
               other_ctxs) then (
          (Utils.StringMap.find ident ctx.typing_ctx).binding_used <- true
        )
      ) ctx1.typing_ctx
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
  let (wires''', wres) = WireCollection.add_binary graph.thread_id ci.typedefs ci.macro_defs op w' (`Single wconst) wires'' in
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
  let (wires', wres) = WireCollection.add_binary graph.thread_id ci.typedefs ci.macro_defs op w1' (`Single w2') wires' in
  graph.wires <- wires';
  let open Typing in
  let new_dtype = (`Array (`Logic, ParamEnv.Concrete sz')) in
  Typing.merged_data graph (Some wres) new_dtype ctx.current [td1; td2]
let check_dtype dtype1 dtype2 span =
  match dtype1 with
  | Some dt1 ->
    if dt1 <> dtype2 then
      raise (Except.TypeError [Text ("Invalid data type: expected " ^ (string_of_data_type dt1) ^ " but got " ^ (string_of_data_type dtype2)); Except.codespan_local span])
  | None -> ()

let rec recurse_unfold expr_full_node expr_node =
  let unfold = recurse_unfold expr_full_node in
  if expr_node.d = Recurse then
    expr_full_node
  else
    let expr' = match expr_node.d with
    | Literal _ | Identifier _
    | Cycle _ | Sync _
    | Ready _ | Read _ | Probe _
    | Debug DebugFinish
    | Recv _ -> expr_node.d
    | Call (ident, expr_nodes) ->
      Call (ident, List.map unfold expr_nodes)
    | Assign (lval, expr_node') ->
      Assign (lval, unfold expr_node')
    | Binop (op, e1, e2) ->
      ( match e2 with
        | `List es2 ->
          let es2' = List.map unfold es2 in
          let e1' = unfold e1 in
          Binop (op, e1', `List es2')
        | `Single e2' ->
          let e2n = unfold e2' in
          Binop (op, unfold e1, (`Single e2n))
      )
    | Unop (op, expr_node') ->
      Unop (op, unfold expr_node')
    | Tuple expr_nodes ->
      Tuple (List.map unfold expr_nodes)
    | Let (idents, dtype, e) ->
      Let (idents, dtype, unfold e)
    | Join (e1, e2) ->
      Join (unfold e1, unfold e2)
    | Wait (e1, e2) ->
      Wait (unfold e1, unfold e2)
    | IfExpr (e1, e2, e3) ->
      IfExpr (unfold e1, unfold e2, unfold e3)
    | TrySend (sp, e1, e2) ->
      let sp = {sp with send_data = unfold sp.send_data} in
      TrySend (sp, unfold e1, unfold e2)
    | TryRecv (ident, rp, e1, e2) ->
      TryRecv (ident, rp, unfold e1, unfold e2)
    | Construct (cs, e') ->
      Construct (cs, Option.map unfold e')
    | Record (ident, vs, base) ->
      Record (ident,
        List.map (fun (i, e) -> (i, unfold e)) vs,
        Option.map unfold base
      )
    | Index (e', idx) ->
      Index (unfold e', idx)
    | Indirect (e', ident) ->
      Indirect (unfold e', ident)
    | Concat (es, is_flat) ->
      Concat (List.map unfold es, is_flat)
    | Match (e, arms) ->
      Match (unfold e,
        List.map (fun (m, eop) -> (m, Option.map unfold eop)) arms
      )
    | Debug (DebugPrint (format, es)) ->
      Debug (DebugPrint (format, List.map unfold es))
    | Send sp ->
      Send {sp with send_data = unfold sp.send_data}
    | SharedAssign (ident, e') ->
      SharedAssign (ident, unfold e')
    | List es ->
      List (List.map unfold es)
    | Recurse -> failwith "Shouldn't reach here!"
    in
    {expr_node with d = expr'}

let rec lvalue_info_of graph (ci:cunit_info) ctx span lval =
  let binop_td_const = binop_td_const graph ci ctx span
  and binop_td_td = binop_td_td graph ci ctx span in
  match lval with
  | Reg ident ->
    let r = Utils.StringMap.find_opt ident graph.regs
      |> unwrap_or_err ("Undefined register " ^ ident) span in
    let sz = TypedefMap.data_type_size ci.typedefs ci.macro_defs r.d_type in
    {
      lval_range = full_reg_range ident sz;
      lval_dtype = r.d_type
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
      let ctx_val = Typing.context_lookup ctx.typing_ctx ident in
      let macro_val = List.assoc_opt ident (List.map (fun (macro : macro_def) ->(macro.id, macro.value)) ci.macro_defs) in
      (match ctx_val, macro_val with
        | Some _, Some _ ->
          raise (event_graph_error_default ("Conflicting Identifier " ^ ident ^ " declarations found") e.span)
        | Some binding, None -> Typing.use_binding binding |> Typing.sync_data graph ctx.current
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
          |> Typing.use_binding |> Typing.sync_data graph ctx.current
      )

  | Assign (lval, e') ->
    let td = visit_expr graph ci ctx e' in
    let lvi = lvalue_info_of graph ci ctx e.span lval in
    if td.dtype <> lvi.lval_dtype then
      raise (Except.TypeError [Text ("In assignment: Invalid data type for " ^ (string_of_lvalue lval) ^ ": expected " ^ (string_of_data_type lvi.lval_dtype) ^ " got " ^ (string_of_data_type td.dtype)); Except.codespan_local e.span]);
    ctx.current.actions <- (RegAssign (lvi, td) |> tag_with_span e.span)::ctx.current.actions;
    Typing.cycles_data graph 1 ctx.current
  | Call (id, arg_list) ->
      let func = List.find_opt (fun (f: Lang.func_def) -> f.name = id) ci.func_defs
        |> unwrap_or_err ("Undefined function: " ^ id) e.span in
      let td_args = List.map (visit_expr graph ci ctx) arg_list in
      let ctx' = BuildContext.clear_bindings ctx |> ref in
      if List.length td_args <> List.length func.args then
        raise (event_graph_error_default "Arguments missing in function call" e.span);
        List.iter2 (fun td arg ->
        (
          let _ = td.w in (* added for tc*)
          match arg.arg_type with
            | Some gtype ->
              if td.dtype <> gtype then
                raise (Except.TypeError [Text ("In function call " ^ id ^ ": Invalid argument type for " ^ arg.arg_name ^ ": expected " ^ (string_of_data_type gtype) ^ " got " ^ (string_of_data_type td.dtype)); Except.codespan_local e.span])
            | None -> ()
        );
        ctx' := BuildContext.add_binding !ctx' arg.arg_name td
      ) td_args func.args;
      visit_expr graph ci !ctx' func.body
  | Binop (binop, e1, e2) ->
    (
      let td1 = visit_expr graph ci ctx e1 in
      let w1 = unwrap_or_err "Invalid value" e1.span td1.w in
      match e2 with
      | `List ws2 ->
        let td2_list = List.map (fun e2 ->
          let td2 = visit_expr graph ci ctx e2 in
          td2
        ) ws2 in
        let w2_list = List.map (fun td2 ->
          unwrap_or_err "Invalid value of second operator" e1.span td2.w
        ) td2_list in
        let (wires', w) = WireCollection.add_binary graph.thread_id ci.typedefs ci.macro_defs binop w1 (`List w2_list) graph.wires in
        graph.wires <- wires';
        let new_dtype = `Array (`Logic, ParamEnv.Concrete w.size) in
        Typing.merged_data graph (Some w) new_dtype ctx.current (List.concat [[td1]; td2_list])
      | `Single e ->
        let td2 = visit_expr graph ci ctx e in
        let w2 = unwrap_or_err "Invalid value of second operator" e1.span td2.w in
        let (wires', w) = WireCollection.add_binary graph.thread_id ci.typedefs ci.macro_defs binop w1 (`Single w2) graph.wires in
        graph.wires <- wires';
        if td1.dtype <> td2.dtype then
          raise (Except.TypeError [Text ("In binary operation: Invalid argument types: " ^ (string_of_data_type td1.dtype) ^ " and " ^ (string_of_data_type td2.dtype)); Except.codespan_local e.span]);
        let new_dtype = td1.dtype in
        Typing.merged_data graph (Some w) new_dtype ctx.current [td1; td2]
    )
  | Unop (unop, e') ->
    let td = visit_expr graph ci ctx e' in
    let w' = unwrap_or_err "Invalid value" e'.span td.w in
    let (wires', w) = WireCollection.add_unary graph.thread_id ci.typedefs unop w' graph.wires in
    graph.wires <- wires';
    Typing.derived_data (Some w) td
  | Tuple [] -> Typing.const_data graph None (unit_dtype) ctx.current
  | Let (_idents, _, e) -> visit_expr graph ci ctx e
  | Join (e1, e2) ->
    let td1 = visit_expr graph ci ctx e1 in
    (
      match e1.d with
      | Let (["_"],dtype ,_) -> 
        check_dtype dtype td1.dtype e1.span;
        let td = visit_expr graph ci ctx e2 in
        let lt = Typing.lifetime_intersect graph td1.lt td.lt in
        {td with lt} (* forcing the bound value to be awaited *)
      | Let ([], dtype, _) ->
        check_dtype dtype td1.dtype e1.span;
        let td = visit_expr graph ci ctx e2 in
        let lt = Typing.lifetime_intersect graph td1.lt td.lt in
        {td with lt} (* forcing the bound value to be awaited *)
      | Let ([ident],dtype, _) ->
          check_dtype dtype td1.dtype e1.span;
          let ctx' = BuildContext.add_binding ctx ident td1 in
          let td = visit_expr graph ci ctx' e2 in
          (* check if the binding is used *)
          let binding = Typing.context_lookup ctx'.typing_ctx ident |> Option.get in
          (* no need to wait if the value is current *)
          if td1.lt.live.id <> ctx.current.id && not binding.binding_used then (
            raise (event_graph_error_default "Value is potentially not awaited!" e1.span)
          );
          td
      | Let _ -> raise (event_graph_error_default "Discarding expression results!" e.span)
      | _ ->
        let td = visit_expr graph ci ctx e2 in
        let lt = Typing.lifetime_intersect graph td1.lt td.lt in
        {td with lt} (* forcing the bound value to be awaited *)
    )
  | Wait (e1, e2) ->
    let td1 = visit_expr graph ci ctx e1 in
    (
      match e1.d with
      | Let (["_"], dtype, _) ->
        check_dtype dtype td1.dtype e1.span;
        let ctx' = BuildContext.wait graph ctx td1.lt.live in
        visit_expr graph ci ctx' e2
      | Let ([], dtype, _) ->
        check_dtype dtype td1.dtype e1.span;
        let ctx' = BuildContext.wait graph ctx td1.lt.live in
        visit_expr graph ci ctx' e2
      | Let ([ident], dtype, _) ->
        check_dtype dtype td1.dtype e1.span;
        (* add the binding to the context *)
        let ctx' = BuildContext.wait graph ctx td1.lt.live in
        let ctx' = BuildContext.add_binding ctx' ident td1 in
        visit_expr graph ci ctx' e2
      | Let _ -> raise (event_graph_error_default "Discarding expression results!" e.span)
      | _ ->
        let ctx' = BuildContext.wait graph ctx td1.lt.live in
        visit_expr graph ci ctx' e2
    )
  | Ready msg_spec ->
    let _ = MessageCollection.lookup_message graph.messages msg_spec ci.channel_classes
      |> unwrap_or_err "Invalid message specifier in ready" e.span in
    (* if msg.dir <> In then (
      (* mismatching direction *)
      raise (event_graph_error_default "Mismatching message direction!" e.span)
    ); *)
    let wires, msg_valid_port = WireCollection.add_msg_valid_port graph.thread_id ci.typedefs msg_spec graph.wires in
    graph.wires <- wires;
    Typing.immediate_data graph (Some msg_valid_port) `Logic ctx.current
  | Probe msg_spec ->
    let _ = MessageCollection.lookup_message graph.messages msg_spec ci.channel_classes
    |> unwrap_or_err "Invalid message specifier in probe" e.span in
    let wires, msg_ack_port = WireCollection.add_msg_ack_port graph.thread_id ci.typedefs msg_spec graph.wires in
    graph.wires <- wires;
    Typing.immediate_data graph (Some msg_ack_port) `Logic ctx.current
  | Cycle n -> Typing.cycles_data graph n ctx.current
  | IfExpr (e1, e2, e3) ->
    let td1 = visit_expr graph ci ctx e1 in
    (* TODO: type checking *)
    let w1 = unwrap_or_err "Invalid condition" e1.span td1.w in
    let ctx' = BuildContext.wait graph ctx td1.lt.live in
    let branch_info = {
      branch_cond_v = td1;
      branch_cond = TrueFalse;
      branch_count = 2; (* for if-else *)
      branches_to = [];
      branches_val = [];
    } in

    let (br_side_true, ctx_true) = BuildContext.branch_side graph ctx' branch_info 0 in
    let td2 = visit_expr graph ci ctx_true e2 in
    let (br_side_false, ctx_false) = BuildContext.branch_side graph ctx' branch_info 1 in
    let td3 = visit_expr graph ci ctx_false e3 in

    branch_info.branches_to <- [ctx_true.current; ctx_false.current];
    branch_info.branches_val <- [td2.lt.live; td3.lt.live];

    BuildContext.branch_merge ctx' [ctx_true; ctx_false];

    let ctx_br = BuildContext.branch graph ctx' branch_info in
    br_side_true.branch_event <- Some ctx_br.current;
    br_side_false.branch_event <- Some ctx_br.current;
    let lt = {live = ctx_br.current; (* branch event is reached when either split event is reached *)
      dead = Utils.list_unordered_join td1.lt.dead td2.lt.dead |> Utils.list_unordered_join td3.lt.dead} in
    let reg_borrows' = Utils.list_unordered_join td1.reg_borrows td2.reg_borrows |> Utils.list_unordered_join td3.reg_borrows in
    (
      match td2.w, td3.w with
      | None, None -> {w = None; lt; reg_borrows = reg_borrows'; dtype = unit_dtype}
      | Some w2, Some w3 ->
      (* TODO: check that the data types are the same *)
        let (wires', w) = WireCollection.add_switch graph.thread_id ci.typedefs [(w1, w2)] w3 graph.wires in
        graph.wires <- wires';
        {w = Some w; lt; reg_borrows = reg_borrows'; dtype = td2.dtype}
      | _ -> raise (event_graph_error_default "Invalid if expression!" e.span)
    )
  | TrySend (send_pack, e1, e2) ->
    (* data to send *)
    let td_send_data = visit_expr graph ci ctx send_pack.send_data in

    let wires, w_cond = WireCollection.add_msg_ack_port graph.thread_id ci.typedefs send_pack.send_msg_spec graph.wires in
    let td_cond = {
      w = Some w_cond;
      lt = {live = ctx.current; dead = [(ctx.current, `Cycles 1)]};
      reg_borrows = [];
      dtype = `Logic;
    } in
    graph.wires <- wires;

    (* TODO: dedup *)
    let branch_info = {
      branch_cond_v = td_cond;
      branch_cond = TrueFalse;
      branch_count = 2;
      branches_to = [];
      branches_val = [];
    } in

    let (br_side_true, ctx_true) = BuildContext.branch_side graph ctx branch_info 0 in
    let td1 = visit_expr graph ci ctx_true e1 in
    let (br_side_false, ctx_false) = BuildContext.branch_side graph ctx branch_info 1 in
    let td2 = visit_expr graph ci ctx_false e2 in

    branch_info.branches_to <- [ctx_true.current; ctx_false.current];
    branch_info.branches_val <- [td1.lt.live; td2.lt.live];

    BuildContext.branch_merge ctx [ctx_true; ctx_false];

    ctx_true.current.actions <- (ImmediateSend (send_pack.send_msg_spec, td_send_data) |> tag_with_span e.span)::ctx_true.current.actions;

    let ctx_br = BuildContext.branch graph ctx branch_info in
    br_side_true.branch_event <- Some ctx_br.current;
    br_side_false.branch_event <- Some ctx_br.current;
    let lt = {live = ctx_br.current; (* branch event is reached when either split event is reached *)
      dead = Utils.list_unordered_join td1.lt.dead td2.lt.dead |> Utils.list_unordered_join td_cond.lt.dead} in
    let reg_borrows' = Utils.list_unordered_join td1.reg_borrows td2.reg_borrows |> Utils.list_unordered_join td_cond.reg_borrows in
    (
      match td1.w, td2.w with
      | None, None -> {w = None; lt; reg_borrows = reg_borrows'; dtype = unit_dtype}
      | Some w1, Some w2 ->
      (* TODO: check that the data types are the same *)
        let (wires', w) = WireCollection.add_switch graph.thread_id ci.typedefs [(w_cond, w1)] w2 graph.wires in
        graph.wires <- wires';
        {w = Some w; lt; reg_borrows = reg_borrows'; dtype = td2.dtype}
      | _ -> raise (event_graph_error_default "Invalid try send expression!" e.span)
    )
  | TryRecv (ident, recv_pack, e1, e2) ->
    let wires, w_cond = WireCollection.add_msg_valid_port graph.thread_id ci.typedefs recv_pack.recv_msg_spec graph.wires in
    let td_cond = {
      w = Some w_cond;
      lt = {live = ctx.current; dead = [(ctx.current, `Cycles 1)]};
      reg_borrows = [];
      dtype = `Logic;
    } in
    graph.wires <- wires;

    (* TODO: dedup *)
    let branch_info = {
      branch_cond_v = td_cond;
      branch_cond = TrueFalse;
      branch_count = 2;
      branches_to = [];
      branches_val = [];
    } in

    let (br_side_true, ctx_true_no_binding) = BuildContext.branch_side graph ctx branch_info 0 in

    (* message *)
    let msg = MessageCollection.lookup_message graph.messages recv_pack.recv_msg_spec ci.channel_classes
      |> unwrap_or_err "Invalid message specifier in try receive" e.span in
    let (wires', w_recv) = WireCollection.add_msg_port graph.thread_id ci.typedefs ci.macro_defs recv_pack.recv_msg_spec 0 msg graph.wires in
    graph.wires <- wires';
    let stype = List.hd msg.sig_types in
    let d_recv = delay_pat_globalise recv_pack.recv_msg_spec.endpoint stype.lifetime.e in
    let td_recv = {
      w = Some w_recv;
      lt = {live = ctx_true_no_binding.current; dead = [(ctx_true_no_binding.current, d_recv)]};
      reg_borrows = [];
      dtype = stype.dtype;
    } in
    let ctx_true = BuildContext.add_binding ctx_true_no_binding ident td_recv in
    let td1 = visit_expr graph ci ctx_true e1 in
    let (br_side_false, ctx_false) = BuildContext.branch_side graph ctx branch_info 1 in
    let td2 = visit_expr graph ci ctx_false e2 in

    branch_info.branches_to <- [ctx_true.current; ctx_false.current];
    branch_info.branches_val <- [td1.lt.live; td2.lt.live];

    BuildContext.branch_merge ctx [ctx_true_no_binding; ctx_false];

    ctx_true.current.actions <- (ImmediateRecv recv_pack.recv_msg_spec |> tag_with_span e.span)::ctx_true.current.actions;

    let ctx_br = BuildContext.branch graph ctx branch_info in
    br_side_true.branch_event <- Some ctx_br.current;
    br_side_false.branch_event <- Some ctx_br.current;
    let lt = {live = ctx_br.current; (* branch event is reached when either split event is reached *)
      dead = Utils.list_unordered_join td1.lt.dead td2.lt.dead |> Utils.list_unordered_join td_cond.lt.dead} in
    let reg_borrows' = Utils.list_unordered_join td1.reg_borrows td2.reg_borrows |> Utils.list_unordered_join td_cond.reg_borrows in
    (
      match td1.w, td2.w with
      | None, None -> {w = None; lt; reg_borrows = reg_borrows'; dtype = unit_dtype}
      | Some w1, Some w2 ->
      (* TODO: check that the data types are the same *)
        let (wires', w) = WireCollection.add_switch graph.thread_id ci.typedefs [(w_cond, w1)] w2 graph.wires in
        graph.wires <- wires';
        {w = Some w; lt; reg_borrows = reg_borrows'; dtype = td2.dtype}
      | _ -> raise (event_graph_error_default "Invalid try recv expression!" e.span)
    )

  | Match (match_v, match_arms) ->
    let td_v = visit_expr graph ci ctx match_v in
    let w_v = unwrap_or_err "Invalid match expression" match_v.span td_v.w in
    let ctx' = BuildContext.wait graph ctx td_v.lt.live in
    let branch_info = {
      branch_cond_v = td_v;
      branch_cond = MatchCases [];
      branch_count = List.length match_arms;
      branches_to = [];
      branches_val = [];
    } in

    let (default_arms, match_arms) =
      List.partition (fun ((pat, _) : expr_node * expr_node option) -> pat.d = Identifier "_") match_arms in
    (
      match default_arms with
      | [default_arm] ->
        let td_pats = List.map (fun (pat, _) -> visit_expr graph ci ctx' pat) match_arms in
        (* make sure that the patterns are all constants *)
        List.iter2
          (fun ((pat, _) : expr_node * expr_node option) td ->
            let valid =
              match td.w with
              | None -> false
              | Some w -> w.is_const
            in
            if not valid then
              raise (event_graph_error_default "Match patterns must be constant values!" pat.span)
          ) match_arms td_pats;

        (* build bodies *)
        let branches = List.mapi
          (fun idx (_, body) ->
            let (br_side, ctx) = BuildContext.branch_side graph ctx' branch_info idx in
            let td = visit_expr graph ci ctx @@ Option.get body in
            ((ctx, br_side, td), (ctx.current, td.lt.live))
          )
          match_arms
        in
        let (br_side_default, ctx_default) = BuildContext.branch_side graph ctx' branch_info (branch_info.branch_count - 1) in
        let td_default = visit_expr graph ci ctx_default @@ Option.get @@ snd default_arm in

        let (branches, branches_events) = List.split branches in
        let (branches_to, branches_val) = List.split branches_events in
        branch_info.branches_to <- branches_to @ [ctx_default.current];
        branch_info.branches_val <- branches_val @ [td_default.lt.live];

        BuildContext.branch_merge ctx' @@ ctx_default::(List.map (fun (ctx, _, _) -> ctx) branches);
        let ctx_br = BuildContext.branch graph ctx' branch_info in
        List.iter
          (fun (_, br_side, _) ->
            br_side.branch_event <- Some ctx_br.current
          )
          branches;
        br_side_default.branch_event <- Some ctx_br.current;
        let lt = {
          live = ctx_br.current;
          dead = List.fold_left (fun l (_, _, td) -> Utils.list_unordered_join l td.lt.dead) td_v.lt.dead branches;
        } in
        let reg_borrows' = List.fold_left (fun l (_, _, td) -> Utils.list_unordered_join l td.reg_borrows) td_v.reg_borrows branches in
        branch_info.branch_cond <- MatchCases td_pats;
        if List.for_all (fun (_, _, td_val) -> Option.is_none td_val.w) branches then
          {w = None; lt; reg_borrows = reg_borrows'; dtype = unit_dtype}
        else (
          let (wires', w) = WireCollection.add_cases graph.thread_id ci.typedefs w_v
            (List.map2 (fun td_pat (_, _, td_val) -> (Option.get td_pat.w, Option.get td_val.w)) td_pats branches) (Option.get td_default.w)
            graph.wires in
          graph.wires <- wires';
          {w = Some w; lt; reg_borrows = reg_borrows'; dtype = td_default.dtype}
        )
      | _ -> raise @@ event_graph_error_default "Invalid match expression (exactly one default case expected)!" e.span
    )
  | Concat (es, is_flat) ->
    let tds = List.map (fun e' -> (e', visit_expr graph ci ctx e')) es in
    let ws = List.map (fun ((e', td) : expr_node * timed_data) -> unwrap_or_err "Invalid value in concat" e'.span td.w) tds in
    let (wires', w) = WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs ws graph.wires in
    graph.wires <- wires';
    let tdtype = List.map (fun ((_, td): expr_node * timed_data) -> td.dtype) tds in
    if not (List.for_all (fun d -> d = List.hd tdtype) tdtype) && (not is_flat) then
      raise (Except.TypeError [Text ("In concat: Incompatible types: " ^ (String.concat ", " (List.map string_of_data_type tdtype))); Except.codespan_local e.span]);
    let new_dtype = ( 
      match is_flat with 
      | false -> `Array (List.hd tdtype, ParamEnv.Concrete (List.length es))
      | _ -> `Array (`Logic, ParamEnv.Concrete (w.size))
    ) in
    List.map snd tds |> Typing.merged_data graph (Some w) new_dtype ctx.current
  | Read reg_ident ->
    let r = Utils.StringMap.find_opt reg_ident graph.regs
      |> unwrap_or_err ("Undefined register " ^ reg_ident) e.span in
    let (wires', w) = WireCollection.add_reg_read graph.thread_id ci.typedefs ci.macro_defs r graph.wires in
    graph.wires <- wires';
    let sz = TypedefMap.data_type_size ci.typedefs ci.macro_defs r.d_type in
    let borrow = {borrow_range = full_reg_range reg_ident sz; borrow_start = ctx.current; borrow_source_span = e.span} in
    {w = Some w; lt = EventGraphOps.lifetime_const ctx.current; reg_borrows = [borrow]; dtype = r.d_type}
  | Debug op ->
    (
      match op with
      | DebugPrint (s, e_list) ->
        let timed_ws = List.map (visit_expr graph ci ctx) e_list in
        ctx.current.actions <- (let open EventGraph in DebugPrint (s, timed_ws) |> tag_with_span e.span)::ctx.current.actions;
        {w = None; lt = EventGraphOps.lifetime_const ctx.current; reg_borrows = []; dtype = unit_dtype}
      | DebugFinish ->
        ctx.current.actions <- (let open EventGraph in tag_with_span e.span DebugFinish)::ctx.current.actions;
        {w = None; lt = EventGraphOps.lifetime_const ctx.current; reg_borrows = []; dtype = unit_dtype}
    )
  | Send send_pack ->
    (* just check that the endpoint and the message type is defined *)
    let msg = MessageCollection.lookup_message graph.messages send_pack.send_msg_spec ci.channel_classes
      |> unwrap_or_err "Invalid message specifier in send" e.span in
    if msg.dir <> Out then (
      (* mismatching direction *)
      raise (event_graph_error_default "Mismatching message direction!" e.span)
    );
    let td = visit_expr graph ci ctx send_pack.send_data in
    let msg_dtype = (List.hd msg.sig_types).dtype in
    if td.dtype <> msg_dtype then
      raise (Except.TypeError [Text ("In send: Invalid data type for message " ^ send_pack.send_msg_spec.endpoint ^ ": expected " ^ (string_of_data_type msg_dtype) ^ " got " ^ (string_of_data_type td.dtype)); Except.codespan_local e.span]);
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
    if msg.dir <> Inp then (
      (* mismatching direction *)
      raise (event_graph_error_default "Mismatching message direction!" e.span)
    );
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
      |> unwrap_or_err (Printf.sprintf "Invalid indirection %s" fieldname) e.span in
    let (wires', new_w) = WireCollection.add_slice graph.thread_id w (Const offset_le) len graph.wires in
    graph.wires <- wires';
    {
      td with
      w = Some new_w;
      dtype = new_dtype
    }
  | Index (e', ind) ->
    let td = visit_expr graph ci ctx e' in
    let w = unwrap_or_err "Invalid value in indexing" e'.span td.w in
    let (offset_le, len, new_dtype) =
      TypedefMap.data_type_index ci.typedefs ci.macro_defs
        (visit_expr graph ci ctx)
        (binop_td_const e.span Mul)
        td.dtype ind
      |> unwrap_or_err (Printf.sprintf "Invalid indexing %s for datatype %s" (string_of_index ind) (Lang.string_of_data_type td.dtype)) e.span in
    let wire_of (td:timed_data) = unwrap_or_err (Printf.sprintf "Invalid indexing for %s in data type %s" (string_of_index ind) (Lang.string_of_data_type td.dtype)) e.span td.w in
    let offset_le_w = MaybeConst.map wire_of offset_le in
    let (wires', new_w) = WireCollection.add_slice graph.thread_id w offset_le_w len graph.wires in
    graph.wires <- wires';
    (
      match offset_le with
      | Const _ -> {td with w = Some new_w; dtype = new_dtype}
      | NonConst td_offset ->
          Typing.merged_data graph (Some new_w) new_dtype ctx.current [td; td_offset]
    )
  | Record (record_ty_name, field_exprs, None) ->
    (
      match TypedefMap.data_type_name_resolve ci.typedefs @@ `Named (record_ty_name, []) with
      | Some (`Record record_fields) ->
        (
          match Utils.list_match_reorder (List.map fst record_fields) field_exprs with
          | Some expr_reordered ->
            let tds = List.map2 (fun (field_name, expected_dtype) e' ->
              let td = visit_expr graph ci ctx e' in
              if td.dtype <> expected_dtype then
                raise (Except.TypeError [
                  Text ("In record construction: Invalid data type for field " ^ field_name);
                  Text ("Expected " ^ (string_of_data_type expected_dtype) ^ " but got " ^ (string_of_data_type td.dtype));
                  Except.codespan_local e'.span
                ]);
              (e', td)
            ) record_fields expr_reordered in
            let ws = List.rev_map (fun ((e', {w; _}) : expr_node * timed_data) ->
              unwrap_or_err "Invalid value in record field" e'.span w) tds in
            let (wires', w) = WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs ws graph.wires in
            graph.wires <- wires';
            List.map snd tds |> Typing.merged_data graph (Some w) (`Named (record_ty_name, [])) ctx.current
          | _ -> raise (event_graph_error_default "Invalid record type value!" e.span)
        )
      | _ -> raise (event_graph_error_default "Invalid record type name!" e.span)
    )
  | Record (record_ty_name, field_exprs, Some field_base) ->
      (* record update *)
      let td_base = visit_expr graph ci ctx field_base in
      let tds = List.map (fun (field_ident, e') -> (field_ident, e', visit_expr graph ci ctx e')) field_exprs in
      let updates =
        (* bruteforce *)
        List.map (fun (field_ident, _e', td) ->
          match TypedefMap.data_type_indirect ci.typedefs ci.macro_defs (`Named (record_ty_name, [])) field_ident with
          | None -> raise (event_graph_error_default "Invalid record!" e.span)
          | Some (offset_le, len, _dtype) -> (offset_le, len, Option.get td.w)
        ) tds in
      let (wires', w) = WireCollection.add_update graph.thread_id ci.typedefs (Option.get td_base.w) updates graph.wires in
      graph.wires <- wires';
      Typing.merged_data graph (Some w) (`Named (record_ty_name, [])) ctx.current (td_base::(List.map (fun (_, _, td) -> td) tds))
  | Construct (cstr_spec, cstr_expr_opt) ->
    (
      match TypedefMap.data_type_name_resolve ci.typedefs @@ `Named (cstr_spec.variant_ty_name, []) with
      | Some (`Variant _ as dtype) ->
        let e_dtype_opt = variant_lookup_dtype dtype cstr_spec.variant in
        (
          match e_dtype_opt, cstr_expr_opt with
          | Some e_dtype, Some cstr_expr ->
            let td = visit_expr graph ci ctx cstr_expr in
            (* if td.dtype <> e_dtype then
              raise (Except.TypeError [Text ("In variant construction: Invalid data type for " ^ cstr_spec.variant ^ ": expected " ^ (string_of_data_type e_dtype) ^ " got " ^ (string_of_data_type td.dtype)); Except.codespan_local e.span]); *)
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
          | _ -> raise (event_graph_error_default "Invalid variant construct expression!" e.span)
        )
      | _ -> raise (event_graph_error_default "Invalid variant type name!" e.span)
    )
  | SharedAssign (id, value_expr) ->
    let shared_info = Hashtbl.find_opt ctx.shared_vars_info id
      |> unwrap_or_err ("Undefined identifier " ^ id) e.span in
    if graph.thread_id = shared_info.assigning_thread then
      let value_td = visit_expr graph ci ctx value_expr in
      if not ctx.lt_check_phase then (
        if (Option.is_some shared_info.value.w) || (Option.is_some shared_info.assigned_at) then (
          let prev_assign_action =
            (Option.get shared_info.assigned_at).actions
            |> List.find (fun {d; _} ->
              match d with
              | PutShared (id', _, _) -> id' = id
              | _ -> false
            )
          in
          raise (EventGraphError [
            Text "Shared value can only be assigned in one place!";
            Except.codespan_local e.span;
            Text "Previously assigned at:";
            Except.codespan_local prev_assign_action.span
          ]);
        );
        shared_info.value.w <- value_td.w;
        shared_info.assigned_at <- Some ctx.current;
      );
      ctx.current.actions <- (PutShared (id, shared_info, value_td) |> tag_with_span e.span)::ctx.current.actions;
      Typing.const_data graph None (unit_dtype) ctx.current
    else
      raise (event_graph_error_default "Shared variable assigned in wrong thread" e.span)
  | List li ->
    let tds = List.map (visit_expr graph ci ctx) li in
    let ws = List.map (fun td -> unwrap_or_err "Invalid wires!" e.span td.w) tds in
    let (wires', new_w) = WireCollection.add_list graph.thread_id ci.typedefs ws graph.wires
      |> unwrap_or_err "Invalid list!" e.span in
    graph.wires <- wires';
    let td = List.hd tds in
    Typing.merged_data graph (Some new_w) (`Array (td.dtype, ParamEnv.Concrete (List.length tds))) ctx.current tds
  | Recurse ->
    ctx.current.is_recurse <- true;
    Typing.const_data graph None unit_dtype ctx.current
  | Tuple _ -> raise (event_graph_error_default "Unimplemented expression!" e.span)


module IntHashTbl = Hashtbl.Make(Int)

(* unfold until sufficient for lifetime checks *)
let recurse_unfold_for_checks ci shared_vars_info graph expr_node =
  let tmp_graph = {graph with last_event_id = -1} in
  let td = visit_expr tmp_graph ci
    (BuildContext.create_empty tmp_graph shared_vars_info true)
    expr_node in
  (* now just check the total *)
  let root = List.find (fun e -> e.source = `Root None) tmp_graph.events in
  let recurse = List.find (fun e -> e.is_recurse) tmp_graph.events in
  let dists = GraphAnalysis.event_min_distance_with_later tmp_graph.events root root in
  let full_dist = IntHashTbl.find dists td.lt.live.id in
  let recurse_dist = IntHashTbl.find dists recurse.id in
  if recurse_dist = 0 then
    raise (event_graph_error_default "Recurse delay must be greater than 0!" expr_node.span);
  (* the number of times to unfold is minimum for the recurse time to first pass the end event *)
  let unfold_times = full_dist / recurse_dist in
  let cur_expr = ref expr_node in
  for _ = 1 to unfold_times do
    cur_expr := recurse_unfold !cur_expr expr_node
  done;
  !cur_expr

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
    let msg_collection = MessageCollection.create body.channels
                                      proc.args body.spawns ci.channel_classes in
    let spawns =
    List.map (fun (s : spawn_def ast_node) ->
      let module_name = BuildScheduler.add_proc_task sched ci.file_name s.span s.d.proc s.d.compile_params in
      (module_name, s)
    ) body.spawns in
    let shared_vars_info = Hashtbl.create (List.length body.shared_vars) in
    List.iter (fun sv ->
      let v = {
        w = None;
        glt = sv.d.shared_lifetime;
        gdtype = unit_dtype; (* explicitly annotate? *)
      } in
      let r = {
        assigning_thread = sv.d.assigning_thread;
        value = v;
        assigned_at = None;
      } in
        Hashtbl.add shared_vars_info sv.d.ident r
      ) body.shared_vars;
      let proc_threads = List.mapi (fun i ((e, reset_by) : expr_node * message_specifier option) ->
        let graph = {
          thread_id = i;
          events = [];
          wires = WireCollection.empty;
          channels = List.map data_of_ast_node body.channels;
          messages = msg_collection;
          spawns = List.map data_of_ast_node body.spawns;
          regs = List.map (fun (reg : Lang.reg_def ast_node) ->
                (reg.d.name, reg.d)) body.regs |> Utils.StringMap.of_list;
          last_event_id = -1;
          is_general_recursive = false;
          thread_codespan = e.span;
        } in
        (* Bruteforce treatment: just run twice *)
        let graph_opt = if (not config.disable_lt_checks) || config.two_round_graph then (
          let tmp_graph = {graph with last_event_id = -1} in
          let td = visit_expr tmp_graph ci
            (BuildContext.create_empty tmp_graph shared_vars_info true)
            (recurse_unfold_for_checks ci shared_vars_info graph e) in
          tmp_graph.last_event_id <- (EventGraphOps.find_last_event tmp_graph).id;
          tmp_graph.is_general_recursive <- tmp_graph.last_event_id <> td.lt.live.id;
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
        let g = match graph_opt with
        | Some graph -> graph
        | None -> (
            (* discard after type checking *)
            let ctx = (BuildContext.create_empty graph shared_vars_info false) in
            let td = visit_expr graph ci ctx e in
            graph.last_event_id <- (EventGraphOps.find_last_event graph).id;
            graph.is_general_recursive <- graph.last_event_id <> td.lt.live.id;
            GraphOpt.optimize config false ci graph
        ) in
        (g, reset_by)
      ) body.threads in
      {name = module_name; extern_module = None;
        threads = proc_threads; shared_vars_info; messages = msg_collection;
        proc_body = proc.body; spawns = List.map (fun (ident, {d; _}) -> (ident, d)) spawns}
    | Extern (extern_mod, _extern_body) ->
      let msg_collection = MessageCollection.create [] proc.args [] ci.channel_classes in
      {name = module_name; extern_module = Some extern_mod; threads = [];
        shared_vars_info = Hashtbl.create 0; messages = msg_collection;
        proc_body = proc.body; spawns = []}

let build (config : Config.compile_config) sched module_name param_values (cunit : compilation_unit) =
  let macro_defs = cunit.macro_defs in
  let typedefs = TypedefMap.of_list cunit.type_defs in
  let func_defs = cunit.func_defs in
  let ci = {
    file_name = Option.get cunit.cunit_file_name;
    typedefs;
    channel_classes = cunit.channel_classes;
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
  }

let syntax_tree_precheck (_config : Config.compile_config) cunit =
  (* just check if the channel definitions have well-formed sync modes *)
  List.iter (fun cc ->
    let msg_set = ref Utils.StringSet.empty in
    List.iter (fun (msg : message_def) ->
      if Utils.StringSet.mem msg.name !msg_set then
        raise (Except.TypeError [Text "Duplicate message!"; Except.codespan_local msg.span])
      else (
        msg_set := Utils.StringSet.add msg.name !msg_set
      )
    ) cc.messages;
    List.iter (fun (msg : message_def) ->
      List.iter (fun {lifetime = lt_d; _} ->
        match lt_d.e with
        | `Cycles _ | `Eternal -> ()
        | `Message msg_name ->
          if not @@ Utils.StringSet.mem msg_name !msg_set then
            raise (Except.TypeError [Text "Undefined message specified in delay pattern!"; Except.codespan_local msg.span])
      ) msg.sig_types;
      let check_dependent_msg msg_name =
        if msg_name = msg.name then
          raise (Except.TypeError [Text "Self-referential dependent sync mode is not allowed!"; Except.codespan_local msg.span]);
        if not @@ Utils.StringSet.mem msg_name !msg_set then
          raise (Except.TypeError [Text "Undefined message specified in dynamic sync mode!"; Except.codespan_local msg.span])
      in
      match msg.send_sync, msg.recv_sync with
      | Static (o_n, n), Static (o_m, m) ->
        if n <> m || o_n <> o_m then (* the cycle counts on both sides must be equal *)
          raise (Except.TypeError [Text "Static sync mode must be symmetric!"; Except.codespan_local msg.span])
      | Static _, Dependent (msg_name, _)
      | Dependent (msg_name, _), Static _
      | Dynamic, Dependent (msg_name, _) ->
        check_dependent_msg msg_name
      | Dependent _, Dynamic ->
        raise (Except.TypeError [Text "Dependent sync mode cannot be mixed with other sync mode!"; Except.codespan_local msg.span])
      | Dependent (msg1, n1), Dependent (msg2, n2) ->
        if msg1 <> msg2 || n1 <> n2 then
          raise (Except.TypeError [Text "Dependent sync mode must be symmetric!"; Except.codespan_local msg.span]);
        check_dependent_msg msg1
      | _ -> ()
    ) cc.messages
  ) cunit.channel_classes
