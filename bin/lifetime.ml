open Lang
type lifetime = sig_lifetime
type event = future

(* compute the new timestamp after event *)
(* the timestamp specifies a point in time that is no later than when the timestamp becomes 0 *)
let consume_lowerbound (t : future) (e : event) : future =
  match t, e with
  | Cycles n, Cycles m ->
    let n' = if n > m then n - m else 0 in
    Cycles n'
  | Cycles _, _ -> Cycles 0 (* we can make the timestamp earlier but no later *)
  | AtSend msg1, AtSend msg2 -> if msg1 = msg2 then Cycles 0 else t
  | AtRecv msg1, AtRecv msg2 -> if msg1 = msg2 then Cycles 0 else t
  | _ -> t

(* this is for timestamps that specify the upperbound of a point in time *)
let consume_upperbound (t : future) (e : event) : future =
  match t, e with
  | Cycles n, Cycles m ->
    let n' = if n > m then n - m else 0 in
    Cycles n'
  | AtSend msg1, AtSend msg2 -> if msg1 = msg2 then Cycles 0 else t
  | AtRecv msg1, AtRecv msg2 -> if msg1 = msg2 then Cycles 0 else t
  | _ -> t

(* can only shrink giving a lifetime that guarantees the reference is alive *)
let consume_lifetime_must_live (lt : lifetime) (e : event) : lifetime =
  (* so this lifetime.b is basically an upperbound of when the reference becomes alive *)
  (* and lifetime.e is a lowerbound of when the references becomes dead *)
  let b' = consume_upperbound lt.b e in
  let e' = consume_lowerbound lt.e e in
  { b = b'; e = e' }

(* can only expand giving a lifetime that contains all possible points in time when the reference might be alive *)
let consume_lifetime_might_live (lt : lifetime) (e : event) : lifetime =
  let b' = consume_lowerbound lt.b e in
  let e' = consume_upperbound lt.e e in
  { b = b'; e = e' }

(* future relation *)
let future_no_later_than (t1 : future) (t2 : future) : bool =
  match t1, t2 with
  | Cycles n, Cycles m -> n <= m
  | Cycles 0, _ | Cycles 1, _ -> true
  | _ -> t1 = t2

(* lifetime relation *)
let lifetime_covered_by (lt1 : lifetime) (lt2 : lifetime) : bool =
  (future_no_later_than lt2.b lt1.b) && (future_no_later_than lt1.e lt2.e)


(** The earlier of the two futures. *)
let future_earlier_of (t1 : future) (t2 : future) : future =
  match t1, t2 with
  | Cycles n, Cycles m -> Cycles (min n m)
  | _ when t1 = t2 -> t1
  | _ -> Cycles 0

(** The later of the two futures. *)
let future_later_of (t1 : future) (t2 : future) : future =
  match t1, t2 with
  | Cycles n, Cycles m -> Cycles (max n m)
  | _ when t1 = t2 -> t1
  | _ -> Eternal

let lifetime_merge_tight (lt1 : lifetime) (lt2 : lifetime) : lifetime =
  let b' = future_later_of lt1.b lt2.b
  and e' = future_earlier_of lt1.e lt2.e in
  { b = b'; e = e' }

let lifetime_merge_relaxed (lt1 : lifetime) (lt2 : lifetime) : lifetime =
  let b' = future_earlier_of lt1.b lt2.b
  and e' = future_later_of lt1.e lt2.e in
  { b = b'; e = e' }

(** Borrowing with the lifetime overlaps with a mutation of the current cycle,
    In other words, a mutation will change the borrowed value. *)
let lifetime_overlaps_current_mutation (lt : lifetime) : bool =
  (* only if the lifetime covers both this cycle and the next *)
  (lt.b = Cycles 0) &&
    match lt.e with
    | Cycles 0 | Cycles 1 -> false
    | _ -> true
