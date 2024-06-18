open Lang

exception UnimplementedError of string
exception TypeError of string

type event = delay
type wire = WireCollection.wire
type wire_collection = WireCollection.t

module Typing = struct
  type lifetime = {
    live : event;
    dead : event;
  }

  type timed_data = {
    w : wire option;
    lt : lifetime;
  }

  let lifetime_const = {live = `Cycles 0; dead = `Ever}
  let lifetime_intersect (a : lifetime) (b : lifetime) =
    {
      live = `Later(a.live, b.live);
      dead = `Earlier(a.live, b.live);
    }
  let lifetime_progress (by : delay) (a : lifetime) =
    {
      live = `ExceptLate (a.live, by);
      dead = `ExceptEarly (`Seq(a.live, a.dead), by);
    }

  let cycles_data (n : int) = {w = None; lt = {live = `Cycles n; dead = `Ever}}
  let const_data (w : wire option) = {w; lt = lifetime_const}
  let merged_data (w : wire option) (lts : lifetime list) =
    let lt = List.fold_left lifetime_intersect lifetime_const lts in
    {w; lt}
  let derived_data (w : wire option) (lt : lifetime) = {w; lt}

  let ahead_data d by =
    {d with lt = {d.lt with live = `Seq (by, d.lt.live)}}

  type context = timed_data Utils.string_map
  let context_add (ctx : context) (v : identifier) (d : timed_data) : context =
    Utils.StringMap.add v d ctx
  let context_empty : context = Utils.StringMap.empty
  let context_lookup (ctx : context) (v : identifier) = Utils.StringMap.find_opt v ctx
  let context_temporal_progress (ctx : context) (by: delay) : context =
    Utils.StringMap.map (fun d -> {d with lt = lifetime_progress by d.lt}) ctx
end

type event_graph = {
  mutable events: event list;
  mutable wires: wire_collection;
  typedefs: TypedefMap.t;
}

module BuildContext = struct
  type t = {
    typing_ctx : Typing.context;
  }

  let empty : t = {
    typing_ctx = Typing.context_empty
  }

  let add_binding (ctx : t) (v : identifier) (d : Typing.timed_data) : t =
    {typing_ctx = Typing.context_add ctx.typing_ctx v d}
  let temporal_progress (ctx : t) (by : delay) : t =
    {typing_ctx = Typing.context_temporal_progress ctx.typing_ctx by}
end

type build_context = BuildContext.t

let rec visit_expr (graph : event_graph) (ctx : build_context) (e : expr) : Typing.timed_data =
  match e with
  | Literal lit ->
    let (wires', w) = WireCollection.add_literal lit graph.wires in
    graph.wires <- wires';
    Typing.const_data (Some w)
  | Identifier ident -> Typing.context_lookup ctx.typing_ctx ident |> Option.get
  | Assign _ -> raise (UnimplementedError "Assign expression unimplemented!")
  | Binop (binop, e1, e2) ->
    let td1 = visit_expr graph ctx e1
    and td2 = visit_expr graph ctx e2 in
    let w1 = Option.get td1.w
    and w2 = Option.get td2.w in
    let (wires', w) = WireCollection.add_binary graph.typedefs binop w1 w2 graph.wires in
    graph.wires <- wires';
    Typing.merged_data (Some w) [td1.lt; td2.lt]
  | Unop (unop, e') ->
    let td = visit_expr graph ctx e' in
    let w' = Option.get td.w in
    let (wires', w) = WireCollection.add_unary graph.typedefs unop w' graph.wires in
    graph.wires <- wires';
    Typing.derived_data (Some w) td.lt
  | Tuple _ -> raise (UnimplementedError "Tuple expression unimplemented!")
  | LetIn (idents, e1, e2) ->
    let td1 = visit_expr graph ctx e1 in
    (
      match idents, td1.w with
      | [], None | ["_"], _  -> visit_expr graph ctx e2
      | [ident], _ ->
        let ctx' = BuildContext.add_binding ctx ident td1 in
        visit_expr graph ctx' e2
      | _ -> raise (TypeError "Discarding expression results!")
    )
  | Wait (e1, e2) ->
    let td1 = visit_expr graph ctx e1 in
    let ctx' = BuildContext.temporal_progress ctx td1.lt.live in
    let td2 = visit_expr graph ctx' e2 in
    Typing.ahead_data td2 td1.lt.live
  | Cycle n -> Typing.cycles_data n
  (* | IfExpr (e1, e2, e3) -> *)
    (* TODO: reduce the e1's lifetime to see if it's guaranteed to be alive *)
    (* let td1 = visit_expr graph ctx e1
    and td2 = visit_expr graph ctx e2
    and td3 = visit_expr graph ctx e3 in
    let w1 = Option.get td1.w in *)
  | _ -> raise (UnimplementedError "Unimplemented expression!")

let build_proc (typedefs : TypedefMap.t) (proc : proc_def) =
  let graph = {
    events = [];
    wires = WireCollection.empty;
    typedefs
  } in
  let _ = visit_expr graph BuildContext.empty proc.body.prog in
  graph


let build (cunit : compilation_unit) =
  let typedefs = TypedefMap.of_list cunit.type_defs in
  List.map (build_proc typedefs) cunit.procs

