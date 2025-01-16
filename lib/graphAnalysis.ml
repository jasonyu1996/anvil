open Lang
open EventGraph

let event_traverse (ev : event) visitor : event list =
  let visited = Seq.return ev.id |> Utils.IntSet.of_seq |> ref in
  let q = Seq.return ev |> Queue.of_seq in
  let res = ref [] in
  let add_to_queue ev' =
    if Utils.IntSet.mem ev'.id !visited |> not then (
      visited := Utils.IntSet.add ev'.id !visited;
      Queue.add ev' q
    )
    else ()
  in
  while Queue.is_empty q |> not do
    let cur = Queue.pop q in
    res := cur::!res;
    visitor add_to_queue cur
  done;
  !res

(** Compute predecessors of an event in the event graph.
  Result in topological order
*)
let event_predecessors (ev : event) : event list =
  let visitor add_to_queue cur =
    match cur.source with
    | `Later (e1, e2)
    | `Either (e1, e2) ->
      add_to_queue e1;
      add_to_queue e2
    | `Branch (_, ev')
    | `Seq (ev', _) ->
      add_to_queue ev'
    | _ -> ()
  in
  event_traverse ev visitor

(* Result in topological order *)
let event_successors (ev : event) : event list =
  let visitor add_to_queue cur =
    List.iter add_to_queue cur.outs
  in
  event_traverse ev visitor |> List.rev

let event_is_successor (ev : event) (ev' : event) =
  event_successors ev |> List.exists (fun x -> x.id = ev'.id)

let event_is_predecessor (ev : event) (ev' : event) =
  event_predecessors ev |> List.exists (fun x -> x.id = ev'.id)


let in_control_set (cur : (int * int) Utils.string_map) (cnt : (int * int) Utils.string_map) (s : identifier) =
  let (pre, post) = Utils.StringMap.find s cnt in
  if Utils.StringMap.find s cur |> fst = 0 then
    pre + 1 = post
  else
    pre + 2 = post

let in_control_set_reg (ev : event) = in_control_set ev.current_regs ev.control_regs
let in_control_set_endps (ev : event) = in_control_set ev.current_endps ev.control_endps

(** evs must be in topological order*)
let find_controller (endpts : identifier list) =
  List.find_opt
    (fun ev ->
      List.for_all (fun endpt -> in_control_set_endps ev endpt) endpts
    )

(* inclusive: include the matching event itself or its predecessor? *)
let find_first_msg_after (ev : event) (msg: Lang.message_specifier) inclusive =
  event_successors ev |> List.find_map
    (fun ev' ->
      List.find_map (fun sa ->
        match sa.d.ty with
        | Send (msg', _) | Recv msg' ->
            if msg' = msg then Some (ev', sa) else None
      ) ev'.sustained_actions
      |> Option.map (fun (ev', sa) -> if inclusive then sa.d.until else ev')
    )

let event_succ_msg_match_earliest (ev : event) (msg : message_specifier) =
  let preds = event_predecessors ev in
  let pred_controller = find_controller [msg.endpoint] (List.rev preds) |> Option.get in
  find_first_msg_after pred_controller msg

let event_succ_msg_match_latest (ev : event) (msg : message_specifier) =
  let succs = event_successors ev in
  let succ_controller = find_controller [msg.endpoint] succs |> Option.get in
  find_first_msg_after succ_controller msg


module IntHashtbl = Hashtbl.Make(Int)
let event_distance_max = 1 lsl 20
let event_succ_distance non_succ_dist msg_dist_f either_dist_f events (ev : event) (cur: event) =
  let preds = event_predecessors ev in
  let preds_cur = event_predecessors cur in
  let succs = event_successors cur in
  let dist = IntHashtbl.create 8 in
  IntHashtbl.add dist ev.id 0;
  let get_dist ev' = IntHashtbl.find_opt dist ev'.id |> Option.value ~default:non_succ_dist in
  let set_dist ev' d = IntHashtbl.add dist ev'.id d in
  List.iter (fun ev' -> set_dist ev' 0) preds; (* predecessors must have been reached *)
  List.rev events |> List.iter (fun ev' ->
    if IntHashtbl.find_opt dist ev'.id |> Option.is_none then
    let d = match ev'.source with
    | `Root -> raise (Except.UnknownError "Unexpected root!")
    | `Later (ev1, ev2) -> max (get_dist ev1) (get_dist ev2)
    | `Seq (ev1, ad) ->
      let d1 = get_dist ev1 in
      (
        match ad with
        | `Cycles n' -> d1 + n'
        | `Send _ | `Recv _ | `Sync _ -> msg_dist_f d1
      )
    | `Branch (_, ev1) ->
      (* We need to check carefully to decide if we are sure
        the branch has/hasn't been taken. *)
      if (List.mem ev' succs) || (List.mem ev' preds_cur) then
        get_dist ev1
      else
        non_succ_dist
    | `Either (ev1, ev2) ->
      either_dist_f (get_dist ev1) (get_dist ev2)
    in
    set_dist ev' (min d event_distance_max)
  );
  dist

let event_min_distance =
  event_succ_distance event_distance_max (fun d -> d) min

let event_max_distance =
  event_succ_distance event_distance_max (fun _ -> event_distance_max) max
