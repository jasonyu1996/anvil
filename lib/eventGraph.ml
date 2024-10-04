open Lang
open Except

type wire = WireCollection.wire
type wire_collection = WireCollection.t


type cunit_info = {
  typedefs : TypedefMap.t;
  channel_classes : channel_class_def list;
}

type action =
  | DebugPrint of string * wire list
  | DebugFinish
  | RegAssign of string * wire

type sustained_action_type =
  | Send of message_specifier * wire
  | Recv of message_specifier

type condition = {
  w : wire;
  neg : bool;
}


type atomic_delay = [
  | `Cycles of int
  | `Send of Lang.message_specifier
  | `Recv of Lang.message_specifier
]

type event = {
  id : int;
  mutable actions: action list;
  mutable sustained_actions : sustained_action list;
  source: event_source;
}
and event_source = [
  | `Root
  | `Later of event * event
  | `Seq of event * atomic_delay
  | `Branch of condition * event
  | `Either of event * event (* joining if-then-else branches *)
]
(* sustained actions are in effect when current is reached and until
  is not reached *)
and sustained_action = {
  until : event;
  ty : sustained_action_type
}

type event_pat = (event * Lang.delay_pat) list

type event_graph = {
  (* name : identifier; *)
  thread_id : int;
  mutable events: event list;
  mutable wires: wire_collection;
  channels: channel_def list;
  messages : MessageCollection.t;
  spawns: spawn_def list;
  regs: reg_def list;
  (* loops: expr list; *)
  (* the id of the last event *)
  (* the process loops from the start when this event is reached *)
  mutable last_event_id: int;
}
type proc_graph = {
  name: identifier;
  threads: event_graph list;
}
module Typing = struct
  type lifetime = {
    live : event;
    dead : event_pat;
  }

  type timed_data = {
    w : wire option;
    lt : lifetime;
  }

  type context = timed_data Utils.string_map
  type build_context = {
    typing_ctx : context;
    current : event;
  }

  let event_create g source =
    let n = {actions = []; sustained_actions = []; source; id = List.length g.events} in
    g.events <- n::g.events;
    n

  let lifetime_const current = {live = current; dead = [(current, `Eternal)]}
  let _lifetime_immediate current = {live = current; dead = [(current, `Cycles 1)]}
  let lifetime_intersect g (a : lifetime) (b : lifetime) =
    {
      live = event_create g (`Later (a.live, b.live));
      dead = a.dead @ b.dead;
    }

  let cycles_data g (n : int) (current : event) =
    let live_event = event_create g (`Seq (current, `Cycles n)) in
    {w = None; lt = {live = live_event; dead = [(live_event, `Eternal)]}}
  let const_data _g (w : wire option) (current : event) = {w; lt = lifetime_const current}
  let merged_data g (w : wire option) (current : event) (lts : lifetime list) =
    match lts with
    | [] -> const_data g w current
    | lt::lts' ->
      let lt' = List.fold_left (lifetime_intersect g) lt lts' in
      {w; lt = lt'}
  let derived_data (w : wire option) (lt : lifetime) = {w; lt}
  let send_msg_data g (msg : message_specifier) (current : event) =
    let live_event = event_create g (`Seq (current, `Send msg)) in
    {w = None; lt = {live = live_event; dead = [(live_event, `Eternal)]}}
  let recv_msg_data g (w : wire option) (msg : message_specifier) (_msg_def : message_def) (current : event) =
    let event_received = event_create g (`Seq (current, `Recv msg)) in
    (* FIXME: take into consideration the lifetime signature *)
    {w; lt = {live = event_received; dead = [(event_received, `Eternal)]}}

  let context_add (ctx : context) (v : identifier) (d : timed_data) : context =
    Utils.StringMap.add v d ctx
  let context_empty : context = Utils.StringMap.empty
  let context_lookup (ctx : context) (v : identifier) = Utils.StringMap.find_opt v ctx
  (* checks if lt lives at least as long as required *)

  (* let rec delay_leq_check (a : delay) (b : delay) : bool =
    match a, b with
    | _, `Ever -> true
    | _, `Later (b1, b2) ->
      (delay_leq_check a b1) || (delay_leq_check a b2)
    | _, `Earlier (b1, b2) ->
      (delay_leq_check a b1) && (delay_leq_check a b2)
    | `Later (a1, a2), _ ->
      (delay_leq_check a1 b) && (delay_leq_check a2 b)
    | `Earlier (a1, a2), _ ->
      (delay_leq_check a1 b) || (delay_leq_check a2 b)
    | `Root, _ -> true
    | _, `Root -> false
    | `Ever, `Seq (b', _) -> delay_leq_check `Ever b'
    (* only both seq or atomic *)
    | `Seq (a', da), `Seq (b', db) ->
      (
        match da, db with
        | `Cycles na, `Cycles nb ->
          let m = min na nb in
          if na = m then
            delay_leq_check a' (`Seq (b', `Cycles (nb - m)))
          else
            delay_leq_check (`Seq (a', `Cycles (na - m))) b'
        | `Send ma, `Send mb when ma = mb ->
          delay_leq_check a' b'
        | `Recv ma, `Recv mb when ma = mb ->
          delay_leq_check a' b'
        | _ ->
          delay_leq_check a' b
      ) *)

  module BuildContext = struct
    type t = build_context
    let create_empty g : t = {
      typing_ctx = context_empty;
      current = event_create g `Root
    }

    let add_binding (ctx : t) (v : identifier) (d : timed_data) : t =
      {ctx with typing_ctx = context_add ctx.typing_ctx v d}
    let wait g (ctx : t) (other : event) : t =
      {ctx with current = event_create g (`Later (ctx.current, other))}
    let branch g (ctx : t) (w : wire) : t * t =
      (
        {ctx with current = event_create g (`Branch ({w; neg = false}, ctx.current))},
        {ctx with current = event_create g (`Branch ({w; neg = true}, ctx.current))}
      )
  end


end

exception BorrowCheckError of string

type build_context = Typing.build_context
module BuildContext = Typing.BuildContext

let rec visit_expr (graph : event_graph) (ci : cunit_info)
                   (ctx : build_context) (e : expr) : Typing.timed_data =
  match e with
  | Literal lit ->
    let (wires', w) = WireCollection.add_literal lit graph.wires in
    graph.wires <- wires';
    Typing.const_data graph (Some w) ctx.current
  | Identifier ident -> Typing.context_lookup ctx.typing_ctx ident |> Option.get
  | Assign (lval, e') ->
    let td = visit_expr graph ci ctx e' in
    let w' = Option.get td.w in
    let reg_ident = (
      let open Lang in
      match lval with
      | Reg ident ->
        ident
      | _ -> raise (UnimplementedError "Lval with indexing/indirection unimplemented!")
    ) in
    ctx.current.actions <- (RegAssign (reg_ident, w'))::ctx.current.actions;
    Typing.cycles_data graph 1 ctx.current
  | Binop (binop, e1, e2) ->
    let td1 = visit_expr graph ci ctx e1
    and td2 = visit_expr graph ci ctx e2 in
    let w1 = Option.get td1.w
    and w2 = Option.get td2.w in
    let (wires', w) = WireCollection.add_binary ci.typedefs binop w1 w2 graph.wires in
    graph.wires <- wires';
    Typing.merged_data graph (Some w) ctx.current [td1.lt; td2.lt]
  | Unop (unop, e') ->
    let td = visit_expr graph ci ctx e' in
    let w' = Option.get td.w in
    let (wires', w) = WireCollection.add_unary ci.typedefs unop w' graph.wires in
    graph.wires <- wires';
    Typing.derived_data (Some w) td.lt
  | Tuple [] -> Typing.const_data graph None ctx.current
  | LetIn (idents, e1, e2) ->
    let td1 = visit_expr graph ci ctx e1 in
    (
      match idents, td1.w with
      | [], None | ["_"], _  -> visit_expr graph ci ctx e2
      | [ident], _ ->
        let ctx' = BuildContext.add_binding ctx ident td1 in
        visit_expr graph ci ctx' e2
      | _ -> raise (TypeError "Discarding expression results!")
    )
  | Wait (e1, e2) ->
    let td1 = visit_expr graph ci ctx e1 in
    let ctx' = BuildContext.wait graph ctx td1.lt.live in
    visit_expr graph ci ctx' e2
  | Cycle n -> Typing.cycles_data graph n ctx.current
  | IfExpr (e1, e2, e3) ->
    let td1 = visit_expr graph ci ctx e1 in
    (* TODO: type checking *)
    let w1 = Option.get td1.w in
    let (ctx_true, ctx_false) =
      let ctx' = BuildContext.wait graph ctx td1.lt.live in
      BuildContext.branch graph ctx' w1
    in
    let td2 = visit_expr graph ci ctx_true e2
    and td3 = visit_expr graph ci ctx_false e3 in
    (* FIXME: the dead time is incorrect *)
    let lt = let open Typing in {live = Typing.event_create graph (`Either (td2.lt.live, td3.lt.live));
      dead = td1.lt.dead @ td2.lt.dead @td3.lt.dead} in
    (
      match td2.w, td3.w with
      | None, None -> {w = None; lt}
      | Some w2, Some w3 ->
        let (wires', w) = WireCollection.add_switch ci.typedefs [(w1, w2)] w3 graph.wires in
        graph.wires <- wires';
        {w = Some w; lt}
      | _ -> raise (TypeError "Invalid if expression!")
    )
  | Concat es ->
    let tds = List.map (visit_expr graph ci ctx) es in
    let ws = List.map (fun (td : Typing.timed_data) -> Option.get td.w) tds in
    let (wires', w) = WireCollection.add_concat ci.typedefs ws graph.wires in
    graph.wires <- wires';
    Typing.merged_data graph (Some w) ctx.current (List.map (fun (td : Typing.timed_data) -> td.lt) tds)
  | Read reg_ident ->
    let r = List.find (fun (r : Lang.reg_def) -> r.name = reg_ident) graph.regs in
    let (wires', w) = WireCollection.add_reg_read ci.typedefs r graph.wires in
    graph.wires <- wires';
    {w = Some w; lt = Typing.lifetime_const ctx.current}
  | Debug op ->
    (
      match op with
      | DebugPrint (s, e_list) ->
        let timed_ws = List.map (visit_expr graph ci ctx) e_list in
        let ws = List.map (fun (x : Typing.timed_data) -> Option.get x.w)
          timed_ws
        in
        ctx.current.actions <- (DebugPrint (s, ws))::ctx.current.actions;
        (* TODO: incorrect lifetime *)
        {w = None; lt = Typing.lifetime_const ctx.current}
      | DebugFinish ->
        ctx.current.actions <- DebugFinish::ctx.current.actions;
        {w = None; lt = Typing.lifetime_const ctx.current}
    )
  | Send send_pack ->
    let td = visit_expr graph ci ctx send_pack.send_data in
    let _msg = MessageCollection.lookup_message graph.messages send_pack.send_msg_spec ci.channel_classes |> Option.get in
    (* TODO: lifetime checking *)
    let ntd = Typing.send_msg_data graph send_pack.send_msg_spec ctx.current in
    ctx.current.sustained_actions <-
      {
        until = ntd.lt.live;
        ty = Send (send_pack.send_msg_spec, Option.get td.w)
      }::ctx.current.sustained_actions;
    ntd
  | Recv recv_pack ->
    let msg = MessageCollection.lookup_message graph.messages recv_pack.recv_msg_spec ci.channel_classes |> Option.get in
    let (wires', w) = WireCollection.add_msg_port ci.typedefs recv_pack.recv_msg_spec 0 msg graph.wires in
    graph.wires <- wires';
    let ntd = Typing.recv_msg_data graph (Some w) recv_pack.recv_msg_spec msg ctx.current in
    ctx.current.sustained_actions <- {until = ntd.lt.live; ty = Recv recv_pack.recv_msg_spec}::ctx.current.sustained_actions;
    ntd
  | Indirect (e', fieldname) ->
    let td = visit_expr graph ci ctx e' in
    let w = Option.get td.w in
    let (offset_le, offset_ri, new_dtype) = TypedefMap.data_type_indirect ci.typedefs w.dtype fieldname |> Option.get in
    let (wires', new_w) = WireCollection.add_slice new_dtype w offset_le offset_ri graph.wires in
    graph.wires <- wires';
    {
      td with
      w = Some new_w
    }
  | Index (e', ind) ->
    let td = visit_expr graph ci ctx e' in
    let w = Option.get td.w in
    let (offset_le, offset_ri, new_dtype) = TypedefMap.data_type_index ci.typedefs w.dtype ind |> Option.get in
    let (wires', new_w) = WireCollection.add_slice new_dtype w offset_le offset_ri graph.wires in
    graph.wires <- wires';
    {
      td with
      w = Some new_w
    }
  | Record (record_ty_name, field_exprs) ->
    (
      match TypedefMap.data_type_name_resolve ci.typedefs @@ `Named record_ty_name with
      | Some (`Record record_fields) ->
        (
          match Utils.list_match_reorder (List.map fst record_fields) field_exprs with
          | Some expr_reordered ->
            let tds = List.map (visit_expr graph ci ctx) expr_reordered in
            let ws = List.rev_map (fun ({w; _} : Typing.timed_data) -> Option.get w) tds in
            let (wires', w) = WireCollection.add_concat ci.typedefs ws graph.wires in
            graph.wires <- wires';
            List.map (fun ({lt; _} : Typing.timed_data) -> lt) tds |>
            Typing.merged_data graph (Some w) ctx.current
          | _ -> raise (TypeError "Invalid record type value!")
        )
      | _ -> raise (TypeError "Invalid record type name!")
    )
  | Construct (cstr_spec, cstr_expr_opt) ->
    (
      match TypedefMap.data_type_name_resolve ci.typedefs @@ `Named cstr_spec.variant_ty_name with
      | Some (`Variant _ as dtype) ->
        let e_dtype_opt = variant_lookup_dtype dtype cstr_spec.variant in
        (
          match e_dtype_opt, cstr_expr_opt with
          | Some e_dtype, Some cstr_expr ->
            let td = visit_expr graph ci ctx cstr_expr in
            let w = Option.get td.w in
            let tag_size = variant_tag_size dtype
            and data_size = TypedefMap.data_type_size ci.typedefs e_dtype
            and tot_size = TypedefMap.data_type_size ci.typedefs dtype
            and var_idx = variant_lookup_index dtype cstr_spec.variant |> Option.get in
            let (wires', w_tag) = WireCollection.add_literal (WithLength (tag_size, var_idx)) graph.wires in
            let (wires', new_w) = if tot_size = tag_size + data_size then
              (* no padding *)
              WireCollection.add_concat ci.typedefs [w; w_tag] wires'
            else begin
              (* padding needed *)
              let (wires', w_pad) = WireCollection.add_literal (WithLength (tot_size - tag_size - data_size, 0)) wires' in
              WireCollection.add_concat ci.typedefs [w_pad; w; w_tag] wires'
            end in
            graph.wires <- wires';
            { td with w = Some new_w }
          | None, None ->
            let tag_size = variant_tag_size dtype
            and tot_size = TypedefMap.data_type_size ci.typedefs dtype
            and var_idx = variant_lookup_index dtype cstr_spec.variant |> Option.get in
            let (wires', w_tag) = WireCollection.add_literal (WithLength (tag_size, var_idx)) graph.wires in
            let (wires', new_w) = if tot_size = tag_size then
              (wires', w_tag)
            else begin
              let (wires', w_pad) = WireCollection.add_literal (WithLength (tot_size - tag_size, 0)) wires' in
              WireCollection.add_concat ci.typedefs [w_pad; w_tag] wires'
            end in
            graph.wires <- wires';
            Typing.const_data graph (Some new_w) ctx.current
          | _ -> raise (TypeError "Invalid variant construct expression!")
        )
      | _ -> raise (TypeError "Invalid variant type name!")
    )
  | _ -> raise (UnimplementedError "Unimplemented expression!")

let lifetime_check (_g : event_graph) = ()

(* Builds the graph representation for each process To Do: Add support for commands outside loop (be executed once or continuosly)*)
let build_proc (ci : cunit_info) (proc : proc_def) =
  let proc_threads = List.mapi (fun i e ->  (* Use List.mapi to get the index *)
    let graph = {
      (* name = proc.name; *)
      thread_id = i;
      events = [];
      wires = WireCollection.empty;
      channels = proc.body.channels;
      messages = MessageCollection.create proc.body.channels proc.args ci.channel_classes;
      spawns = proc.body.spawns;
      regs = proc.body.regs;
      last_event_id = 0;
    } in
    let td = visit_expr graph ci (BuildContext.create_empty graph) e in
    graph.last_event_id <- td.lt.live.id;  (* Set last_event_id for each graph *)
    lifetime_check graph;
    graph
  ) proc.body.loops in
  {name = proc.name; threads = proc_threads};

type event_graph_collection = {
  event_graphs : proc_graph list;
  typedefs : TypedefMap.t;
  channel_classes : channel_class_def list;
}

let build (cunit : compilation_unit) =
  let typedefs = TypedefMap.of_list cunit.type_defs in
  let ci = { typedefs; channel_classes = cunit.channel_classes } in
  let graphs = List.concat_map (fun proc -> [build_proc ci proc]) cunit.procs in
  {
    event_graphs = graphs;
    typedefs;
    channel_classes = cunit.channel_classes;
  }


module Endpoint = struct
  let canonicalize (endpoint : endpoint_def) : identifier =
    match endpoint.dir with
    | Left -> endpoint.name
    | Right -> Option.value ~default:endpoint.name endpoint.opp

  let canonicalize_endpoint_name (endpoint_name : identifier) (g : event_graph) : identifier =
    match MessageCollection.lookup_endpoint g.messages endpoint_name with
    | Some endpoint_local -> canonicalize endpoint_local
    | None -> endpoint_name
end

let canonicalize_endpoint_name = Endpoint.canonicalize_endpoint_name
