type lifetime = Lang.sig_lifetime
type future = Lang.type
type event = Lang.delay_def

(* compute the new timestamp after event *)
(* the timestamp specifies a point in time that is no later than when the timestamp becomes 0 *)
let consume_lowerbound (t : future) (e : event) : future =
  match t, e with
  | Cycles n, Cycles m ->
    let n' = if n > m then n - m else 0 in
    Cycles n'
  | Cycles n, _ -> Cycles 0 (* we can make the timestamp earlier but no later *)
  | _ ->
    if t = e then Cycles 0 else t

(* this is for timestamps that specify the upperbound of a point in time *)
let consume_upperbound (t : future) (e : event) : future =
  match t, e with
  | Cycles n, Cycles m ->
    let n' = if n > m then n - m else 0 in
    Cycles n'
  | _ ->
    if t = e then Cycles 0 else t (* we can make the timestamp later but no earlier *)

(* can only shrink giving a lifetime that guarantees the reference is alive *)
let consume_lifetime_must_live (lt : lifetime) (e : event) : lifetime =
  (* so this lifetime.b is basically an upperbound of when the reference becomes alive *)
  (* and lifetime.e is a lowerbound of when the references becomes dead *)
  let b' = consume_upperbound lt.b in
  let e' = consume_lowerbound lt.e in
  { b = b'; e = e' }

(* can only expand giving a lifetime that contains all possible points in time when the reference might be alive *)
let consume_lifetime_might_live (lt : lifetime) (e : event) : lifetime =
  let b' = consume_lowerbound lt.b in
  let e' = consume_upperbound lt.e in
  { b = b'; e = e' }
